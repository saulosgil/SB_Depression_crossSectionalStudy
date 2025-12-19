# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# Lendo a base para ajustes -------------------------------------------------------------------
df <- readr::read_rds("df_para_CalcularVars.rds")


# Calculando as variaveis ---------------------------------------------------------------------
# IMC + ajustando data_nasc <- idade
df_ajustado <-
  df |>
  mutate(
    imc = round(peso/(estatura^2)*10000,1),
    obesidade = case_when(imc > 29 ~ 1,
                          imc < 30 ~ 0),
    idade = data_nasc
    ) |>
  select(-data_nasc)

# Comorbidades
has <- str_detect(string = df$comorbidades,pattern = "hipertensão arterial")
dm2 <- str_detect(string = df$comorbidades,pattern = "Diabetes")
ans <- str_detect(string = df$comorbidades,pattern = "Ansiedade")
tabacco <- str_detect(string = df$comorbidades,pattern = "Tabagismo")

df_ajustado <-
  df_ajustado |>
  mutate(
    has = has,
    dm2 = dm2,
    ans = ans,
    tabacco = tabacco
  ) |>
  mutate(
    has = case_when(has == TRUE ~ 1,
                    has == FALSE ~ 0),
    dm2 = case_when(dm2 == TRUE ~ 1,
                    dm2 == FALSE ~ 0),
    ans = case_when(ans == TRUE ~ 1,
                    ans == FALSE ~ 0),
    tabacco = case_when(tabacco == TRUE ~ 1,
                        tabacco == FALSE ~ 0)
  ) |>
  select(-comorbidades)

# Medicamentos
betablocker <- str_detect(string = df$medicamentos,regex("\\b\\w*lol\\b", ignore_case = TRUE))
hzd <- str_detect(string = df$medicamentos,regex("\\b\\w*zida\\b", ignore_case = TRUE))
inib_eca <- str_detect(string = df$medicamentos,regex("\\b\\w*ril\\b", ignore_case = TRUE))
losartana <- str_detect(string = df$medicamentos,regex("\\b\\w*tana\\b", ignore_case = TRUE))
metiformina <- str_detect(string = df$medicamentos,regex("\\b\\w*mina\\b", ignore_case = TRUE))
block_canal_calcio <- str_detect(string = df$medicamentos,regex("\\b\\w*pino\\b", ignore_case = TRUE))

df_ajustado <-
  df_ajustado |>
  mutate(
    betablocker = betablocker,
    hzd = hzd,
    inib_eca = inib_eca,
    losartana = losartana,
    metiformina = metiformina,
    block_canal_calcio = block_canal_calcio
  ) |>
  mutate(
    betablocker = case_when(betablocker == TRUE ~ 1,
                            betablocker == FALSE ~ 0,
                            is.na(betablocker) ~ 0),
    hzd = case_when(hzd == TRUE ~ 1,
                    hzd == FALSE ~ 0,
                    is.na(hzd) ~ 0),
    inib_eca = case_when(inib_eca == TRUE ~ 1,
                         inib_eca == FALSE ~ 0,
                         is.na(inib_eca) ~ 0),
    losartana = case_when(losartana == TRUE ~ 1,
                          losartana == FALSE ~ 0,
                          is.na(losartana) ~ 0),
    metiformina = case_when(metiformina == TRUE ~ 1,
                            metiformina == FALSE ~ 0,
                            is.na(metiformina) ~ 0),
    block_canal_calcio = case_when(block_canal_calcio == TRUE ~ 1,
                                   block_canal_calcio == FALSE ~ 0,
                                   is.na(block_canal_calcio) ~ 0)
  ) |>
  select(-medicamentos)

glimpse(df_ajustado)

# parei na circ_cintura


