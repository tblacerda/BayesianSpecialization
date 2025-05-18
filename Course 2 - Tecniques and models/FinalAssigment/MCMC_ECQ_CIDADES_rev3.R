library(readxl)
library(dplyr)
library(rjags)
library(coda)
library(tidybayes)
library(tidyr)
library(HDInterval)
library(writexl)
library(tidyverse)


# Carregar e filtrar dados
data <- read_excel("ECQ_ABR_25.xlsx", sheet = "Export") %>%
  filter(ANF %in% c(83, 81)) %>%
  mutate(TESTES_ECQ_OK = round(TESTES_ECQ * ECQ, 0))  # Recriar a variável resposta

# Criar IDs únicos para grupos (agora só ANF 83)
data <- data %>%
  group_by(ANF, MUNICIPIO) %>%
  mutate(group_id = cur_group_id()) %>%
  ungroup() %>% 
  select(group_id, ANF, MUNICIPIO, ENDERECO_ID, TESTES_ECQ_OK, TESTES_ECQ)

# Mapeamento de grupos para ANF (só 1 ANF agora)
group_df <- data %>%
  distinct(group_id, ANF) %>%
  arrange(group_id)




jags_data <- list(
  N_group = max(data$group_id),
  N_sites = nrow(data),
  group_per_site = data$group_id,  # Vetor de grupos por site
  n_tests = data$TESTES_ECQ,       # Vetor de testes
  n_success = data$TESTES_ECQ_OK   # Vetor de sucessos
)

model_string <- "
model {
  # Hiperparâmetros globais
  mu_global ~ dbeta(3, 3)
  sigma_global ~ dgamma(2, 0.5)  # Prior Gamma mais informativa

  # Parâmetros ANF com restrição
  alpha_anf <- mu_global * sigma_global
  beta_anf <- (1 - mu_global) * sigma_global 
  mu_anf ~ dbeta(alpha_anf, beta_anf)

  # Priors para dispersão
  phi_municipio ~ dgamma(2, 0.9)  # Gamma mais suave
  phi_site ~ dgamma(2, 2)

  # Loop por municípios
  for(g in 1:N_group) {
    a_municipio[g] <- mu_anf * phi_municipio
    b_municipio[g] <- (1 - mu_anf) * phi_municipio
    mu_municipio[g] ~ dbeta(a_municipio[g], b_municipio[g])
  }

  # Loop por sites
  for(s in 1:N_sites) {
    logit_mu_site[s] <- logit(mu_municipio[group_per_site[s]])
    theta_site[s] <- ilogit(logit_mu_site[s] + epsilon[s])
    epsilon[s] ~ dnorm(0, 1/phi_site)
    
    n_success[s] ~ dbin(theta_site[s], n_tests[s])
  }
}
"


inits <- function() {
  list(
    mu_global = rbeta(1, 3, 3),
    sigma_global = rgamma(1, 2, 0.5),  # Coerente com o prior
    phi_municipio = rgamma(1, 2, 2),
    phi_site = rgamma(1, 2, 2),
    mu_anf = rbeta(1, 2, 2)  # Inicialização direta
    
  )
}

model <- jags.model(
  textConnection(model_string),
  data = jags_data,
  inits = inits,
  n.chains = 4,
  n.adapt = 1000  # Aumentar fase de adaptação
)

samples <- coda.samples(
  model,
  variable.names = c("mu_anf", "mu_municipio", "theta_site"),
  n.iter = 1000,
  thin = 2
)

# Verificar convergência
gelman.diag(samples)
effectiveSize(samples)
traceplot(samples)
raftery.diag(samples) #para saber a quantidade de samples 
gelman.plot(samples)
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

# 3. Extrair estatísticas dos sites
site_stats <- samples %>%
  spread_draws(theta_site[site_index]) %>%  # site_index corresponde à ordem nos dados originais
  group_by(site_index) %>%
  summarise(
    mean_site = mean(theta_site),
    stddev_site = sd(theta_site),
    hdi_inf = hdi(theta_site, credMass = 0.95)[1],
    hdi_sup = hdi(theta_site, credMass = 0.95)[2]
  ) %>%
  mutate(site_index = as.integer(site_index))

# 4. Combinar com dados originais
final_df <- data %>%
  mutate(site_index = row_number()) %>%  # Criar índice correspondente ao theta_site
  left_join(site_stats, by = "site_index") %>%
  left_join(municipio_stats, by = c("group_id", "ANF", "MUNICIPIO")) %>%
  mutate(
    FALHAS_ECQ = TESTES_ECQ - TESTES_ECQ_OK,
    # Calcular métricas de impacto
    FALHAS_ECQ_norm = (FALHAS_ECQ - min(FALHAS_ECQ)) / 
      (max(FALHAS_ECQ) - min(FALHAS_ECQ) + 1e-8),
    impacto = (1 - mean_site) * FALHAS_ECQ_norm,
    rank = dense_rank(desc(impacto))
  ) %>%
  select(
    ANF, MUNICIPIO, ENDERECO_ID,
    hdi_inf, mean_site, hdi_sup,
    hdi_inf_Mun, mean_municipio, hdi_sup_Mun,
    TESTES_ECQ, TESTES_ECQ_OK, FALHAS_ECQ,
    impacto, rank
  ) %>%
  arrange(ANF, MUNICIPIO, rank)

# 5. Verificar e exportar
if(nrow(final_df) == nrow(data)) {
  write_xlsx(final_df, "priorizacao_sites_ECQ_R_OUT24.xlsx")
  message("Arquivo exportado com sucesso!")
} else {
  warning("Verificar correspondência entre índices e dados!")
}


# Visualizar estrutura dos dados processados
glimpse(final_df)


# Verificar primeiros rankings
final_df %>%
  select(ENDERECO_ID, impacto, rank) %>%
  head(10)

# Extrair amostras de mu_anf
mu_anf_samples <- samples %>%
  spread_draws(mu_anf) %>%  # Extrai todas as amostras
  mutate(parameter = "mu_anf")  # Para identificação

# Calcular estatísticas sumárias
mu_anf_stats <- mu_anf_samples %>%
  group_by(parameter) %>%
  summarise(
    mean = mean(mu_anf),
    std_dev = sd(mu_anf),
    hdi_3 = hdi(mu_anf, credMass = 0.94)[1],
    hdi_97 = hdi(mu_anf, credMass = 0.94)[2]
  )

# Formatar para exibição
mu_anf_formatted <- mu_anf_stats %>%
  mutate(
    across(c(mean, std_dev, hdi_3, hdi_97), ~ round(., 3)),
    hdi_interval = glue::glue("[{hdi_3}, {hdi_97}]")
  ) %>%
  select(parameter, mean, std_dev, hdi_interval)

final_df_mun <- final_df %>% 
  select(-TESTES_ECQ, -TESTES_ECQ_OK, -FALHAS_ECQ)


final_df

dic.samples(model,n.iter = 1e3)

head(final_df)
head(final_df)

#####

library(ggplot2)
library(ggrepel)
library(dplyr)  

data_amostrada <- data %>% 
  slice_sample(prop = 0.5)  # ou sample_n(100) para 100 pontos fixos

ggplot(data_amostrada, aes(
  x = TESTES_ECQ,
  y = TESTES_ECQ_OK / TESTES_ECQ,
  label = ENDERECO_ID
)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text_repel(
    size = 3,
    max.overlaps = 5
  ) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  labs(
    title = "ECQ Tests vs. Success Rate (Random Sample)",
    x = "Number of ECQ Tests",
    y = "Success Rate (ECQ_OK / ECQ)",
    caption = "Points represent a random sample of network sites (ENDERECO_ID)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

####


# Gráficos agregados
plot_grid(
  traceplot(samples[,c("mu_anf", "sigma_global")], ncol = 1),
  densplot(samples[,sample(grep("mu_municipio", varnames(samples)), 9)]),
  ncol = 2
)

#######

head(final_df)

# Passo 1: Filtrar municípios com 5+ sites
mun_5plus <- final_df %>%
  group_by(MUNICIPIO) %>%
  filter(n() >= 30) %>%  # Conta linhas (cada linha = 1 site)
  ungroup() %>%
  distinct(MUNICIPIO)  # Lista de municípios elegíveis

# Passo 2: Aplicar filtro aos dados
final_df_filtrado <- final_df %>%
  semi_join(mun_5plus, by = "MUNICIPIO")

df_mun_filtrado <- final_df_filtrado %>%
  group_by(MUNICIPIO) %>%
  summarise(
    mean_mun = mean(mean_municipio),
    hdi_low = mean(hdi_inf_Mun),
    hdi_high = mean(hdi_sup_Mun),
    n_sites = n(),  # Número real de sites
    hdi_width = hdi_high - hdi_low
  ) %>%
  ungroup() %>%
  mutate(MUNICIPIO = fct_reorder(MUNICIPIO, mean_mun))

# Passo 3: Atualizar o plot
ggplot() +
  geom_errorbar(
    data = df_mun_filtrado,
    aes(x = MUNICIPIO, ymin = hdi_low, ymax = hdi_high, alpha = n_sites),
    width = 0.2, color = "gray40"
  ) +
  geom_point(
    data = df_mun_filtrado,
    aes(x = MUNICIPIO, y = mean_mun, size = hdi_width, fill = n_sites),
    shape = 21, stroke = 0.5
  ) +
  geom_point(
    data = filter(final_df_filtrado, rank <= 20),
    aes(x = MUNICIPIO, y = mean_site),
    shape = 4, size = 3, color = "red"
  ) +
  
  # Novidade: Texto repelido
  geom_text_repel(
    data = filter(final_df_filtrado, rank <= 20),
    aes(x = MUNICIPIO, y = mean_site, label = ENDERECO_ID),
    color = "red",
    direction = "y",  # Direção prioritária do repel
    nudge_y = 0.05,   # Deslocamento vertical inicial
    segment.color = "red",
    segment.size = 0.3,
    size = 3,
    max.overlaps = 20  # Permite mais overlaps controlados
  ) +
  scale_fill_viridis_c("Nº Sites", option = "viridis", breaks = seq(5, max(df_mun_filtrado$n_sites), 50)) +
  scale_alpha_continuous("Nº Sites", range = c(0.5, 1)) +
  scale_size_continuous("Uncertainty", range = c(2, 8)) +
  labs(
    x = "Municipality (≥30 sites)",
    caption = "Red Crosses: Worst sites in each municipality",
    title = "Average ECQ Success Rate by Municipality x Worst Sites",
  ) +
  coord_flip() +
  theme_minimal()


library(kableExtra)
library(dplyr)
final_df <- final_df%>% arrange(desc(impacto))
# Format numeric columns & translate headers
pretty_head <- final_df %>%
  head(20) %>%
  mutate(
    across(c(hdi_inf, mean_site, hdi_sup, hdi_inf_Mun, mean_municipio, hdi_sup_Mun, impacto),
           ~ round(., 3)),
    rank = as.integer(rank)
  ) %>%
  rename(
    Município = MUNICIPIO,
    Site = ENDERECO_ID,
    `HDI Inf.` = hdi_inf,
    `Média Site` = mean_site,
    `HDI Sup.` = hdi_sup,
    `HDI Mun. Inf.` = hdi_inf_Mun,
    `Média Município` = mean_municipio,
    `HDI Mun. Sup.` = hdi_sup_Mun,
    Testes = TESTES_ECQ,
    Sucessos = TESTES_ECQ_OK,
    Falhas = FALHAS_ECQ,
    Impacto = impacto,
    Prioridade = rank
  )

# Create formatted table
pretty_head %>%
  kable(
    align = c("l", "l", "l", rep("c", 11)),
    caption = "Amostra de Sites Prioritários (Top 6)"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  column_spec(4:7, background = "#f7f7f7") %>%  # Highlight HDI columns
  column_spec(12, color = "white", background = "#d7191c") %>%  # Priority
  row_spec(0, bold = TRUE, background = "#005b96", color = "white") %>%  # Header
  add_header_above(c(" " = 3, "Site" = 3, "Município" = 3, "Testes" = 3, " " = 2))