# --------------------------------------------------------- #
# |   INSTITUTO DE PESQUISA ECONOMICA APLICADA            | #
# |   PROJETO: PREVISAO PIB                               | #
# --------------------------------------------------------- #
# --------------------------------------------------------- #
# |   PROGRAMADORA: CAMILLA ESTAGIARIA                    | #
# --------------------------------------------------------- #
# --------------------------------------------------------- #
# --------------------------------------------------------- #
# |   LEITURA DOS DADOS                                   | #
# --------------------------------------------------------- #

# CARREGANDO PACOTES ----------------------------------------------------------
library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)
library(tibble)
library(forecast)
library(ggfortify)

# LEITURA DE DADOS ------------------------------------------------------------

# PIB Trimestral
PIB_modificado_tri <- 
  read_excel("Dados/PIB_modificado_trimestral.xlsx", 
             skip = 2) %>% 
  clean_names()

# PIB Anual
PIB_modificado_anual <- 
  read_excel("Dados/PIB_modificado_anual.xlsx", 
             skip = 2) %>% 
  clean_names()

# Buscar os novos dados e organizar num unico df
# tudo que e interessante pra modelagem de determinado pib

# DESCRITIVA ------------------------------------------------------------------

# ST PIB Agro
st_agro <- ts(PIB_modificado_anual$agropecuaria_total, 
              start = 1995, frequency = 1)

# Plot ST
autoplot(st_agro, ts.colour = 'darkgreen', ts.linetype = 'dashed')

# FAC
autoplot(acf(st_agro, plot = FALSE))

autoplot(pacf(st_agro, plot = FALSE))

# INPUT -----------------------------------------------------------------------
dados <- 
  data.frame(data = PIB_modificado_anual$ano,
             y = PIB_modificado_anual$agropecuaria_total, 
             x1 = lag(PIB_modificado_anual$agropecuaria_total, 1),
             x2 = lag(PIB_modificado_anual$agropecuaria_total, 2),
             x3 = lag(PIB_modificado_anual$agropecuaria_total, 3)) %>% 
  na.exclude() %>% 
  as_tibble()

fit <- lm("y ~ x1 + x2 + x3", data = dados)
summary(fit)






















# RASCUNHO

# fit <- forecast::Arima(st_agro, order = c(3, 1, 0))
# # fit <- auto.arima(st_agro)
# plot(forecast(fit,h=20))
# 
# 
# 
# PIB_modificado_tri[, c("agropecuaria_total",
#                        "industria_total", 
#                        "servicos_total",
#                        "imposto")] %>% 
#   cor()
# 
# plot(PIB_modificado_tri[, c("agropecuaria_total",
#                             "industria_total", 
#                             "servicos_total",
#                             "imposto")])
