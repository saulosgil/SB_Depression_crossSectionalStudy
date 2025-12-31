# Este script faz ajustes finos em todas as variaveis a fim de manter os valores corretos para
# analise e também trata os valores faltantes

# pacotes -------------------------------------------------------------------------------------
library(summarytools)
library(tidyverse)
library(DataExplorer)
library(mice)

# lendo a base --------------------------------------------------------------------------------
df <- read_rds(file = "df_para_analise.rds")
glimpse(df)

# Remove Percentual de gordura e massa magra (muitos valores errados)
df <-
  df |>
  select(
    -percentual_gordura,
    -percentual_mm
  )

# Verificando os dados - ainda para ajustes ---------------------------------------------------
df[, 0:35] |>
  select_if(is.numeric) |>
  dfSummary() |>
  stview()

df[, 35:70] |>
  select_if(is.numeric) |>
  dfSummary() |>
  stview()

# verificando ranges de valores numeros -------------------------------------------------------
# idade
range(df$idade, na.rm = TRUE) #correto

# peso
range(df$peso, na.rm = TRUE)

# substituindo valores errados pela média
df <-
  df |>
  mutate(
    peso = case_when(peso > 150 ~ mean(peso),
                     peso < 150 ~ peso),
    peso = round(peso, 0)
  )

range(df$peso, na.rm = TRUE) #correto

# estatura
range(df$estatura, na.rm = TRUE)

# substituindo valores errados pela média
df <-
  df |>
  mutate(
    estatura = case_when(estatura > 192 ~ mean(estatura),
                         estatura < 192 ~ estatura),
    estatura = case_when(estatura > 1.85 ~ estatura/100,
                         estatura < 1.85 ~ estatura),
    estatura = case_when(estatura < 1.0 ~ mean(estatura),
                         estatura > 1 ~ estatura),
    estatura = round(estatura, 2)
  )

range(df$estatura, na.rm = TRUE) #correto

# IMC e obesidade
range(df$imc, na.rm = TRUE)

df <-
  df |>
  mutate(
    imc = round(peso / (estatura^2), 1),
    obesidade = case_when(imc < 30 ~ 0,
                          imc >= 30 ~ 1)
  )

range(df$imc, na.rm = TRUE) #correto

# hand grip
range(df$hgs_max, na.rm = TRUE)

df <-
  df |>
  mutate(
    hgs_max = case_when(hgs_max > 100 ~ mean(hgs_max),
                        hgs_max < 100 ~ hgs_max)
  )

range(df$hgs_max, na.rm = TRUE) #correto

# TUG
range(df$tug_max, na.rm = TRUE)

df <-
  df |>
  mutate(
    tug_max = case_when(tug_max > 60 ~ mean(tug_max),
                        tug_max < 60 ~ tug_max)
  )

range(df$tug_max, na.rm = TRUE) # correto

# TS
range(df$ts_max, na.rm = TRUE)

df <-
  df |>
  mutate(
    ts_max = case_when(ts_max > 30 ~ mean(ts_max),
                        ts_max < 30 ~ ts_max)
  )

range(df$ts_max, na.rm = TRUE) # correto

# Ansiedade de depressão
range(df$ansiedade_score, na.rm = TRUE) #correto
range(df$depressao_score, na.rm = TRUE) #correto

# WHO-QoL
range(df$whoqol_fisico_escore_100, na.rm = TRUE) #correto
range(df$whoqol_psicol_escore_100, na.rm = TRUE) #correto
range(df$whoqol_social_escore_100, na.rm = TRUE) #correto
range(df$whoqol_ambiente_escore_100, na.rm = TRUE) #correto

# Verificando os dados novamente ---------------------------------------------------
df[, 0:35] |>
  select_if(is.numeric) |>
  dfSummary() |>
  stview()

df[, 35:70] |>
  select_if(is.numeric) |>
  dfSummary() |>
  stview()

# ajustando genero, estado civil, renda e raca ------------------------------------------------
unique(df$genero)
unique(df$estado_civil)
unique(df$renda)
unique(df$raca)

# substitui os valores ausentes pela moda (valor mais frequente)
df <-
  df |>
  mutate(
    across(
      c(genero, estado_civil, renda, raca),
      ~ replace(.x, is.na(.x), names(which.max(table(.x))))
    )
  )

unique(df$genero)
unique(df$estado_civil)
unique(df$renda)
unique(df$raca)

# Ajustar renda
df <-
  df |>
  mutate(
    renda = case_when(renda == "500.00" ~ "Até 1 salário mínimo",
                      renda == "$600 - Bolsa Família" ~ "Até 1 salário mínimo",
                      renda == "Não tem renda" ~ "Até 1 salário mínimo",
                      renda == "Até 1 salário mínimo" ~ "Até 1 salário mínimo",
                      renda == "1 – 2 salários mínimos" ~ "1 – 2 salários mínimos",
                      renda == "2 – 3 salários mínimos" ~ "2 – 3 salários mínimos",
                      renda == "4 – 5 salários mínimos" ~ "4 – 5 salários mínimos",
                      renda == "Mais de 3 salários mínimos" ~ "Mais de 5 salários mínimos",
                      renda == "Não relatou" ~ "Não relatou")
  )




# tratamento de missing -----------------------------------------------------------------------
# Verifricando os missing
df |>
  plot_missing()

# Imputação using pmm
tempData <- mice::mice(df,
                       m=5,
                       maxit=50,
                       meth='pmm',
                       seed=500)

# Base imputada
df <- complete(tempData,1)

# Verifricando (novamente) os missing
df |>
  plot_missing()

# Escrevendo novo df para analise ----------------------------------------------------------------------------------
write_rds(x = df,file =  "df_para_analise.rds")
