library(tidyverse)
library(lubridate)
library(rjags)
library(readxl)
library(jsonlite)
library(writexl)
LIMITE <- 22/24

data <- fromJSON("DISP4G.json")

data <- data %>% 
  filter(ANF == 83) %>% 
  mutate(dia = as_date(Dia)) %>% 
  dplyr::select(-Dia)

head(data)

daily <- data %>%
  group_by(Site, dia) %>%
  summarize(evento = as.integer(any(Disp <= LIMITE)), .groups = "drop") %>% 
  rename(periodo = dia) %>% 
  mutate(periodo = as_datetime(periodo))

# Bayesian Weibull model matching PyMC3 version
library(rjags)
library(coda)

bayesian_weibull_model <- function(durations, events,
                                   n.chains = 2,
                                   n.adapt  = 1e3,
                                   n.iter   = 1e4) {
  
  model_string <- "
  model {
    # Priors
    lambda ~ dgamma(2, 1)
    rho    ~ dgamma(2, 1)

    for (i in 1:N) {
      # censura à direita
      event[i] ~ dinterval(T[i], durations[i])
      # tempo latente
      T[i] ~ dweib(rho, lambda)
    }
  }"
  
  data_jags <- list(
    durations = durations,
    events    = events,
    N         = length(durations)
  )
  
  # compila e adapta
  mod <- jags.model(textConnection(model_string),
                    data     = data_jags,
                    n.chains = n.chains,
                    n.adapt  = n.adapt,
                    quiet    = TRUE)
  
  # burn-in
  update(mod, n.adapt)
  
  # amostragem das posteriors
  samples <- coda.samples(mod,
                          variable.names = c("lambda","rho"),
                          n.iter         = n.iter)
  
  return(samples)
}

# Processing function with corrected probability calculation
process_site <- function(df) {
  event_dates <- df %>% filter(evento == 1) %>% pull(periodo)
  all_dates <- df %>% pull(periodo)
  
  if (length(event_dates) == 0) return(NA_real_)
  
  durations <- diff(sort(c(min(all_dates), event_dates, max(all_dates)))) %>% 
    as.numeric(units = "days")
  events <- c(rep(1, length(event_dates)), 0)
  
  if (length(durations) < 2) return(NA_real_)
  
  samples <- bayesian_weibull_model(durations, events)
  samples_matrix <- as.matrix(samples)
  
  s <- tail(durations, 1)
  lambda <- samples_matrix[, "lambda"]
  rho <- samples_matrix[, "rho"]
  
  # Corrected probability calculation
  mean(exp(-(s/lambda)^rho) - exp(-((s + 1)/lambda)^rho))
}



# Final processing
probs_df <- daily %>%
  group_by(Site) %>%
  group_modify(~ {
    result <- tryCatch(
      list(Probability = process_site(.x)),
      error = function(e) list(Probability = NA_real_))
    tibble(Probability = result$Probability)
  }) %>%
  filter(!is.na(Probability), Probability > 0.05) %>%
  arrange(desc(Probability))



write_xlsx(probs_df, "probabilidades.xlsx")
probs_df 
