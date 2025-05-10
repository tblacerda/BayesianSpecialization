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


daily <- data %>%
  group_by(Site, dia) %>%
  summarize(evento = as.integer(any(Disp <= LIMITE)), .groups = "drop") %>% 
  rename(periodo = dia) %>% 
  mutate(periodo = as_datetime(periodo))


# 1) função que processa um único site
process_site <- function(df){
  # datas de evento e todas as datas
  event_dates <- df %>% filter(evento == 1) %>% pull(periodo)
  all_dates   <- df %>% pull(periodo)
  
  # se não houve evento, nada a retornar
  if(length(event_dates)==0) return(tibble(duration = NA_real_, event = NA_integer_))
  
  # calcula durations e vetor de eventos
  durations <- diff(sort(c(min(all_dates), event_dates, max(all_dates)))) %>% 
    as.numeric(units="days")
  events    <- c(rep(1, length(event_dates)), 0)
  
  # monta tibble
  tibble(duration = durations, event = events)
}

# 2) aplica por site e “desenrola”
df_out <- daily %>%
  group_by(Site) %>%
  group_modify(~ process_site(.x)) %>%
  ungroup()


library(dplyr)
library(rjags)
library(coda)

# --- 1) Suponha que você já tenha produzido df_out com as colunas:
#     Site, duration e event
#    Vamos remover eventuais NAs (sites sem nenhum evento)
df_fit <- df_out %>% 
  filter(!is.na(duration), !is.na(event))

# --- 2) Montar a lista de dados para o JAGS
data_jags <- list(
  durations = df_fit$duration,  # vetor numérico
  event     = df_fit$event,     # vetor inteiro 0/1
  N         = nrow(df_fit)      # número total de observações
)

# --- 3) Especificar o modelo JAGS (igual ao anterior)
model_string <- "
model {
  # Priors
  lambda ~ dgamma(2, 1)
  rho    ~ dgamma(2, 1)

  for (i in 1:N) {
    # censura à direita
    event[i] ~ dinterval(T[i], durations[i])
    # tempo latente segue Weibull(shape=rho, scale=lambda)
    T[i] ~ dweib(rho, lambda)
  }
}
"

# --- 4) Compilar e adaptar
mod <- jags.model(
  textConnection(model_string),
  data     = data_jags,
  n.chains = 3,
  n.adapt  = 1000
)

# --- 5) Burn-in
update(mod, 1000)

# --- 6) Amostrar as posteriors
samples <- coda.samples(
  model          = mod,
  variable.names = c("lambda","rho"),
  n.iter         = 10000
)

# Pronto: ‘samples’ contém as cadeias MCMC para lambda e rho
summary(samples)
plot(samples)


  # burn-in
  update(mod, n.adapt)
  
  # amostragem das posteriors
  samples <- coda.samples(mod,
                          variable.names = c("lambda","rho"),
                          n.iter         = n.iter)
  
  return(samples)



  
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
