library(readxl)
library(dplyr)
library(rjags)
library(coda)

# Carregar e preparar os dados com group_id
data <- read_excel("DADOS BRUTOS/ECQ_ABR_25.xlsx", sheet = "Export") %>%
  head(-2) %>% 
  mutate(
    TESTES_ECQ_OK = round(TESTES_ECQ * ECQ, 0)
  ) %>%
  group_by(ANF, MUNICIPIO) %>%
  mutate(group_id = cur_group_id()) %>%  # Criar group_id no dado original
  ungroup()



# Dados para JAGS
jags_data <- list(
  N_group = max(data$group_id),        # Número de grupos únicos
  N_obs = nrow(data),                  # Número de observações
  group_id = data$group_id,            # ID do grupo para cada observação
  n_tests = data$TESTES_ECQ,           # Número de testes por observação
  n_success = data$TESTES_ECQ_OK       # Sucessos por observação
)

# Modelo JAGS corrigido incluindo a verossimilhança
model_string <- "
model {
  # Hiperparâmetros globais
  mu_global ~ dbeta(5, 3)
  sigma_global ~ dgamma(2, 0.5)

  # Parâmetros ANF
  alpha_anf <- mu_global * sigma_global
  beta_anf <- (1 - mu_global) * sigma_global 
  mu_anf ~ dbeta(alpha_anf, beta_anf)

  # Dispersão para municípios
  phi_municipio ~ dgamma(2, 0.9)

  # Hierarquia por município
  for(g in 1:N_group) {
    a_municipio[g] <- mu_anf * phi_municipio
    b_municipio[g] <- (1 - mu_anf) * phi_municipio
    mu_municipio[g] ~ dbeta(a_municipio[g], b_municipio[g])
  }

  # Verossimilhança (Binomial)
  for(i in 1:N_obs) {
    n_success[i] ~ dbin(mu_municipio[group_id[i]], n_tests[i])
  }
}
"

# Função de inicialização
inits <- function() {
  list(
    mu_global = rbeta(1, 5, 3),
    sigma_global = rgamma(1, 2, 0.5),
    phi_municipio = rgamma(1, 2, 0.9),
    mu_anf = rbeta(1, 3, 3)
  )
}

# Configurar e executar o modelo
model <- jags.model(
  textConnection(model_string),
  data = jags_data,
  inits = inits,
  n.chains = 4,
  n.adapt = 1000
)

samples <- coda.samples(
  model,
  variable.names = c("mu_anf", "mu_municipio"),
  n.iter = 5000,
  thin = 2
)

# Verificar convergência
gelman.diag(samples)
effectiveSize(samples)
traceplot(samples)
raftery.diag(samples) #para saber a quantidade de samples 
#gelman.plot(samples)
# Analisar resultados
summary(samples)

#Analise 
# 1. Mapear group_id para ANF e Município
group_map <- data %>%
  distinct(group_id, ANF, MUNICIPIO)

# 2. Extrair estatísticas dos municípios
municipio_stats <- samples %>%
  spread_draws(mu_municipio[group_id]) %>%
  group_by(group_id) %>%
  summarise(
    mean_municipio = mean(mu_municipio),
    stddev_municipio = sd(mu_municipio),
    hdi_inf_Mun = hdi(mu_municipio, credMass = 0.95)[1],
    hdi_sup_Mun = hdi(mu_municipio, credMass = 0.95)[2]
  ) %>%
  left_join(group_map, by = "group_id")

municipio_stats
