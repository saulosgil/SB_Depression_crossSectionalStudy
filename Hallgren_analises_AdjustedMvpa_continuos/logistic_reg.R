# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(sjPlot)
library(logistf) # Para fazer Regressao logistica de Firth

# A regressão logística de Firth é uma variante penalizada da regressão logística criada para corrigir
# problemas de viés e instabilidade que ocorrem na regressão logística tradicional, especialmente em
# amostras pequenas ou quando há separação dos dados.

# A regressão logística de Firth é um método de estimação que modifica a função de verossimilhança da
# regressão logística padrão por meio de uma penalização baseada no viés (penalização de Jeffreys),
# com o objetivo de:

  # - Reduzir o viés dos estimadores de máxima verossimilhança

  # - Garantir estimativas finitas dos coeficientes

  # - Produzir odds ratios e intervalos de confiança válidos mesmo na presença de separação completa
# ou quase completa

# Formulação geral (intuição)

# Na regressão logística tradicional, os coeficientes são estimados por máxima verossimilhança (MLE).

# A regressão de Firth:-----------------------------------

  # Adiciona um termo de penalização à verossimilhança

# Esse termo impede que os coeficientes cresçam indefinidamente

# O máximo da verossimilhança penalizada sempre existe

# O modelo continua o mesmo, mas os coeficientes β são estimados com correção de viés

### Descrição para artigo

# Penalized logistic regression using Firth’s method was employed to reduce bias and address issues of
# complete separation, ensuring finite estimates of odds ratios and their corresponding 95% confidence
# intervals.

# Lendo a base para ajustes -------------------------------------------------------------------
df <- readr::read_rds("df_para_analise_hallgren.rds")

glimpse(df)

# criar coluna idade categorica para os modelos  -----------------------------------------------
# criar coluna idade categorica para os modelos RCS -----------------------------------------------
df2 <-
  df |>
  mutate(
    idade_cat = if_else(idade < 65, 0, 1),
    genero = case_when(genero == "Feminino" ~ 0,
                       genero == "Masculino" ~ 1,
                       genero == "N\xc6o bin\xa0rio" ~ 2,
                       genero == "Transg\x88nero" ~ 3),
    raca = case_when(raca == "Branco" ~ 0,
                     raca == "Preto" ~ 1,
                     raca == "Asi\xa0tico" ~ 2,
                     raca == "Ind\xa1gena" ~ 3)
  )

# Dicotomizar Depressao (<20 and ≥20) e SB ------------------------------------------------------------------
df2 <-
  df2 |>
  mutate(
    dep_cat = if_else(depressao_score < 20, 0, 1),
    dep_cat = as.factor(dep_cat),
    total_sb_cat = if_else(total_sb_hday < 4, "< 4 h/day", "≥ 4 h/day"),
    total_sb_cat = as.factor(total_sb_cat),
    total_mpsb_cat = if_else(total_sb_mp_hday < 4, "< 4 h/day", "≥ 4 h/day"),
    total_mpsb_cat = as.factor(total_mpsb_cat),
    total_masb_cat = if_else(total_sb_ma_hday < 4, "< 4 h/day", "≥ 4 h/day"),
    total_masb_cat = as.factor(total_masb_cat)
  )

df2$mvpa_minday <- df2$mvpa_minday / 60 # normalizar para 60 min/dia

glimpse(df2)

# Descriptive data and other metrics ----------------------------------------------------------
df2 |>
  count(dep_cat) |>
  mutate(percentual = round(n / sum(n) * 100, 1))

# Logistic models -----------------------------------------------------------------------------
# SB ------------------------------------------------------------------------------------------
m_crude_sb <- logistf(dep_cat ~ total_sb_cat,
               family = binomial(),
               data = df2)

m_adj_sb <- logistf(
  dep_cat ~ total_sb_cat + idade_cat + genero + raca + renda +
    has + dm2 + obesidade + mvpa_minday,
  family = binomial(),
  data = df2
)

summary(m_crude_sb)
summary(m_adj_sb)

# Coeficientes do modelo
coeficientes <- coef(m_adj_sb)

# Odds Ratios
OR <- exp(coeficientes)

# Intervalos de confiança 95% (perfil de verossimilhança)
IC <- exp(confint(m_adj_sb))

# Tabela final
resultado <- cbind(
  OR = OR,
  IC_2.5 = IC[, 1],
  IC_97.5 = IC[, 2]
)

round(resultado, 3)

# Descriptive data and other metrics ----------------------------------------------------------
df2 |>
  group_by(total_sb_cat) |>
  count(dep_cat) |>
  mutate(percentual = round(n / sum(n) * 100, 1))

# MPSB ------------------------------------------------------------------------------------------
m_crude_mpsb <- logistf(dep_cat ~ total_mpsb_cat,
                  family = binomial(),
                  data = df2)

m_adj_mpsb <- logistf(
  dep_cat ~ total_mpsb_cat + idade_cat + genero + raca + renda +
    has + dm2 + obesidade + mvpa_minday,
  family = binomial(),
  data = df2
)

summary(m_crude_mpsb)
summary(m_adj_mpsb)

# Coeficientes do modelo
coeficientes <- coef(m_adj_mpsb)

# Odds Ratios
OR <- exp(coeficientes)

# Intervalos de confiança 95% (perfil de verossimilhança)
IC <- exp(confint(m_adj_mpsb))

# Tabela final
resultado <- cbind(
  OR = OR,
  IC_2.5 = IC[, 1],
  IC_97.5 = IC[, 2]
)

round(resultado, 2)

# Descriptive data and other metrics ----------------------------------------------------------
df2 |>
  group_by(total_mpsb_cat) |>
  count(dep_cat) |>
  mutate(percentual = round(n / sum(n) * 100, 1))

# MASB ------------------------------------------------------------------------------------------
m_crude_masb <- logistf(dep_cat ~ total_masb_cat,
                    family = binomial(),
                    data = df2)

m_adj_masb <- logistf(
  dep_cat ~ total_masb_cat + idade_cat + genero + raca + renda +
    has + dm2 + obesidade + mvpa_minday,
  family = binomial(),
  data = df2
)

summary(m_crude_masb)
summary(m_adj_masb)

# Coeficientes do modelo
coeficientes <- coef(m_adj_masb)

# Odds Ratios
OR <- exp(coeficientes)

# Intervalos de confiança 95% (perfil de verossimilhança)
IC <- exp(confint(m_adj_masb))

# Tabela final
resultado <- cbind(
  OR = OR,
  IC_2.5 = IC[, 1],
  IC_97.5 = IC[, 2]
)

round(resultado, 2)

# Descriptive data and other metrics ----------------------------------------------------------
df2 |>
  group_by(total_masb_cat) |>
  count(dep_cat) |>
  mutate(percentual = round(n / sum(n) * 100, 1))

