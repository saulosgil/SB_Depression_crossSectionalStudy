# Pacotes -------------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

# ler planilha --------------------------------------------------------------------------------
df <- read_excel("dados_acelerometro_rct.xlsx")

# Calcular estatisticas descritivas - Table 1 -----------------------------------------------------------
df |>
  mutate(idade_cat = if_else(idade < 65, "<65", ">=65")) |>
  group_by(grupo) |>
  count(idade_cat) |>
  mutate(freq_rel = n / sum(n)*100)


df |>
  mutate(estado_civil = case_when(estado_civil == "solteira" ~ "Solteira",
                        estado_civil == "solteiro" ~ "Solteira",
                        estado_civil == "divorciada" ~ "Divorciada",
                        .default = as.character(estado_civil))) |>
  group_by(grupo) |>
  count(estado_civil) |>
  mutate(freq_rel = n / sum(n)*100)



df |>
  mutate(raca = case_when(raca == "pardo" ~ "Pardo",
                          raca == "Amarelo" ~ "Pardo",
                          raca == "preta" ~ "Preto",
                          .default = as.character(raca))) |>
  group_by(grupo) |>
  count(raca) |>
  mutate(freq_rel = n / sum(n)*100)


df |>
  mutate(TABAGO = case_when(TABAGO == "EX" ~ "ex",
                            TABAGO == "Ex" ~ "ex",
                            TABAGO == "eX" ~ "ex",
                            .default = as.character(TABAGO))) |>
  group_by(grupo) |>
  count(TABAGO) |>
  mutate(freq_rel = n / sum(n)*100)

df |>
  mutate(renda_fam = case_when(renda_fam == "Até 1" ~ "Up to 1 minimum wage",
                               renda_fam == "ate 1" ~ "Up to 1 minimum wage",
                               renda_fam == "até 1" ~ "Up to 1 minimum wage",
                               renda_fam == "1 a 2" ~ "1–2 minimum wages",
                               renda_fam == "1 a2" ~  "1–2 minimum wages",
                               renda_fam == "2 a 3" ~ "2–3 minimum wages",
                               renda_fam == "3" ~ "2–3 minimum wages",
                               renda_fam == "Mais que 3" ~ "More than 5 minimum wages"
                               )) |>
  group_by(grupo) |>
  count(renda_fam) |>
  mutate(freq_rel = n / sum(n)*100)


df |>
  group_by(grupo) |>
  count(DM) |>
  mutate(freq_rel = n / sum(n)*100)

df |>
  group_by(grupo) |>
  count(HAS) |>
  mutate(freq_rel = n / sum(n)*100)

df |>
  mutate(obesidade = if_else(imc>30, 1, 0)) |>
  group_by(grupo) |>
  count(obesidade) |>
  mutate(freq_rel = n / sum(n)*100)

# separar medicamentos
# Medicamentos
betablocker <- str_detect(string = df$DRUGS,regex("\\b\\w*lol\\b", ignore_case = TRUE))
hzd <- str_detect(string = df$DRUGS,regex("\\b\\w*zida\\b", ignore_case = TRUE))
inib_eca <- str_detect(string = df$DRUGS,regex("\\b\\w*ril\\b", ignore_case = TRUE))
losartana <- str_detect(string = df$DRUGS,regex("\\b\\w*tana\\b", ignore_case = TRUE))
metiformina <- str_detect(string = df$DRUGS,regex("\\b\\w*mina\\b", ignore_case = TRUE))
block_canal_calcio <- str_detect(string = df$DRUGS,regex("\\b\\w*pino\\b", ignore_case = TRUE))

df_medicamentos <-
  df |>
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
  )

df_medicamentos |>
  group_by(grupo) |>
  count(losartana) |>
  mutate(freq_rel = n / sum(n)*100)

# depressao score
df_sum <-
  df |>
  group_by(grupo) |>
  summarise(
    mean_dep = mean(BECK_SCORE, na.rm = TRUE),
    sd_dep   = sd(BECK_SCORE, na.rm = TRUE)
  ) |>
  mutate(
    grupo = recode(grupo,
                   "control" = "CONTROL",
                   "intervencao" = "INTERVENTION")
  )

dp_plot <-
  ggplot(df_sum,
       aes(x = grupo, y = mean_dep,fill = grupo)) +
  geom_col(width = 0.6,
           show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_dep - sd_dep,
        ymax = mean_dep + sd_dep),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "Depression score (a.u.)"
  ) +
  theme_classic()

dp_plot

# tug score
df$tug_2 <- as.numeric(df$tug_2)

df_sum <-
  df |>
  group_by(grupo) |>
  summarise(
    mean_tug = mean(tug_2, na.rm = TRUE),
    sd_tug   = sd(tug_2, na.rm = TRUE)
  ) |>
  mutate(
    grupo = recode(grupo,
                   "control" = "CONTROL",
                   "intervencao" = "INTERVENTION")
  )

tug_plot <-
  ggplot(df_sum,
         aes(x = grupo, y = mean_tug,fill = grupo)) +
  geom_col(width = 0.6,
           show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_tug - sd_tug,
        ymax = mean_tug + sd_tug),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "Timed up and go (seconds)"
  ) +
  theme_classic()

tug_plot

# ts score
df$ts_2 <- as.numeric(df$ts_2)

df_sum <-
  df |>
  group_by(grupo) |>
  summarise(
    mean_ts = mean(ts_2, na.rm = TRUE),
    sd_ts   = sd(ts_2, na.rm = TRUE)
  ) |>
  mutate(
    grupo = recode(grupo,
                   "control" = "CONTROL",
                   "intervencao" = "INTERVENTION")
  )

ts_plot <-
  ggplot(df_sum,
         aes(x = grupo, y = mean_ts,fill = grupo)) +
  geom_col(width = 0.6,
           show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_ts - sd_ts,
        ymax = mean_ts + sd_ts),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "Timed up and go (seconds)"
  ) +
  theme_classic()

ts_plot

#layout
dp_plot / tug_plot / ts_plot

# QOL
# Calculando WoL em todos os dominios
df_ajustado <-
  df |>
  # ajustando as questões do WHOQoL
  mutate(
    who_qol4 = case_when(
      who_qol4 == 1 ~ 5,
      who_qol4 == 2 ~ 4,
      who_qol4 == 3 ~ 3,
      who_qol4 == 4 ~ 2,
      who_qol4 == 5 ~ 1
    ),
    who_qol5 = case_when(
      who_qol5 == 1 ~ 5,
      who_qol5 == 2 ~ 4,
      who_qol5 == 3 ~ 3,
      who_qol5 == 4 ~ 2,
      who_qol5 == 5 ~ 1
    ),
    who_qol27 = case_when(
      who_qol27 == 1 ~ 5,
      who_qol27 == 2 ~ 4,
      who_qol27 == 3 ~ 3,
      who_qol27 == 4 ~ 2,
      who_qol27 == 5 ~ 1
    )
  )

# Calculando o WHOQoL -------------------------------------------------------------------------
# físico
whoqol_fisico <-
  df_ajustado |>
  select(who_qol4,
         who_qol5,
         who_qol11,
         who_qol16,
         who_qol7,
         who_qol18,
         who_qol19)

whoqol_fisico <-
  whoqol_fisico |>
  mutate(whoqol_fisico = apply(whoqol_fisico[,1:6],MARGIN = 1,FUN = mean)*4) |>
  select(whoqol_fisico)

# verificando os valores - range 0-20
min(whoqol_fisico, na.rm = TRUE)
max(whoqol_fisico, na.rm = TRUE)

# psicologico
whoqol_psicol <-
  df_ajustado |>
  select(who_qol5,
         who_qol6,
         who_qol7,
         who_qol11,
         who_qol19,
         who_qol26)


whoqol_psicol <-
  whoqol_psicol |>
  mutate(whoqol_psicol = apply(whoqol_psicol[,1:6],MARGIN = 1,FUN = mean)*4) |>
  select(whoqol_psicol)

# verificando os valores - range 0-20
min(whoqol_psicol, na.rm = TRUE)
max(whoqol_psicol, na.rm = TRUE)

# Relações sociais
whoqol_social <-
  df_ajustado |>
  select(who_qol20,
         who_qol21,
         who_qol22)

whoqol_social <-
  whoqol_social |>
  mutate(whoqol_social = apply(whoqol_social[,1:3],MARGIN = 1,FUN = mean)*4) |>
  select(whoqol_social)

# verificando os valores - range 0-20
min(whoqol_social, na.rm = TRUE)
max(whoqol_social, na.rm = TRUE)

# Meio ambiente
whoqol_ambiente <-
  df_ajustado |>
  select(who_qol8,
         who_qol9,
         who_qol12,
         who_qol13,
         who_qol14,
         who_qol23,
         who_qol24,
         who_qol25)

whoqol_ambiente <-
  whoqol_ambiente |>
  mutate(whoqol_ambiente = apply(whoqol_ambiente[,1:3],MARGIN = 1,FUN = mean)*4) |>
  select(whoqol_ambiente)

# verificando os valores - range 0-20
min(whoqol_ambiente, na.rm = TRUE)
max(whoqol_ambiente, na.rm = TRUE)

# Juntando os dominios do WHOQol com a base ---------------------------------------------------
#juntado os dominios
fis_psi <- bind_cols(whoqol_fisico, whoqol_psicol)
fis_psi_soc <- bind_cols(fis_psi,whoqol_social)
todos_dominios <- bind_cols(fis_psi_soc,whoqol_ambiente)

# transformar scores em escala de 0-100
todos_dominios <-
  todos_dominios |>
  mutate(
    whoqol_fisico_escore_100 = (whoqol_fisico - 4)*(100/16),
    whoqol_psicol_escore_100 = (whoqol_psicol - 4)*(100/16),
    whoqol_social_escore_100 = (whoqol_social - 4)*(100/16),
    whoqol_ambiente_escore_100 = (whoqol_ambiente - 4)*(100/16)
  )

# Juntando os dominios do WHOQoL com a base e removendo as colunas isoladas
df_ajustado <-
  bind_cols(df_ajustado, todos_dominios) |>
  select(-starts_with(match = "who_"),
         -whoqol_fisico,
         -whoqol_ambiente,
         -whoqol_psicol,
         -whoqol_social) |>
  # arrendondando os valores
  mutate(
    whoqol_fisico_escore_100 = round(whoqol_fisico_escore_100, 0),
    whoqol_psicol_escore_100 = round(whoqol_psicol_escore_100, 0),
    whoqol_social_escore_100 = round(whoqol_social_escore_100, 0),
    whoqol_ambiente_escore_100 = round(whoqol_ambiente_escore_100, 0)
  )
## QOL plots
# Qol score - whoqol_fisico_escore_100
df_sum <-
  df_ajustado |>
  group_by(grupo) |>
  summarise(
    mean_ts = mean(whoqol_fisico_escore_100, na.rm = TRUE),
    sd_ts   = sd(whoqol_fisico_escore_100, na.rm = TRUE)
  ) |>
  mutate(
    grupo = recode(grupo,
                   "control" = "CONTROL",
                   "intervencao" = "INTERVENTION")
  )

whoqol_fisico_escore_100_plot <-
  ggplot(df_sum,
         aes(x = grupo, y = mean_ts,fill = grupo)) +
  geom_col(width = 0.6,
           show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_ts - sd_ts,
        ymax = mean_ts + sd_ts),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "WHOQOL-BREF\nphysical domain (a.u.)"
  ) +
  theme_classic()

whoqol_fisico_escore_100_plot

# Qol score - whoqol_psicol_escore_100
df_sum <-
  df_ajustado |>
  group_by(grupo) |>
  summarise(
    mean_ts = mean(whoqol_psicol_escore_100, na.rm = TRUE),
    sd_ts   = sd(whoqol_psicol_escore_100, na.rm = TRUE)
  ) |>
  mutate(
    grupo = recode(grupo,
                   "control" = "CONTROL",
                   "intervencao" = "INTERVENTION")
  )

whoqol_psicol_escore_100_plot <-
  ggplot(df_sum,
         aes(x = grupo, y = mean_ts,fill = grupo)) +
  geom_col(width = 0.6,
           show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_ts - sd_ts,
        ymax = mean_ts + sd_ts),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "WHOQOL-BREF\npychological domain (a.u.)"
  ) +
  theme_classic()

whoqol_psicol_escore_100_plot

# Qol score - whoqol_social_escore_100
df_sum <-
  df_ajustado |>
  group_by(grupo) |>
  summarise(
    mean_ts = mean(whoqol_social_escore_100, na.rm = TRUE),
    sd_ts   = sd(whoqol_social_escore_100, na.rm = TRUE)
  ) |>
  mutate(
    grupo = recode(grupo,
                   "control" = "CONTROL",
                   "intervencao" = "INTERVENTION")
  )

whoqol_social_escore_100_plot <-
  ggplot(df_sum,
         aes(x = grupo, y = mean_ts,fill = grupo)) +
  geom_col(width = 0.6,
           show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_ts - sd_ts,
        ymax = mean_ts + sd_ts),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "WHOQOL-BREF\nsocial domain (a.u.)"
  ) +
  theme_classic()

whoqol_social_escore_100_plot

# Qol score - whoqol_ambiente_escore_100
df_sum <-
  df_ajustado |>
  group_by(grupo) |>
  summarise(
    mean_ts = mean(whoqol_ambiente_escore_100, na.rm = TRUE),
    sd_ts   = sd(whoqol_ambiente_escore_100, na.rm = TRUE)
  ) |>
  mutate(
    grupo = recode(grupo,
                   "control" = "CONTROL",
                   "intervencao" = "INTERVENTION")
  )

whoqol_ambiente_escore_100_plot <-
  ggplot(df_sum,
         aes(x = grupo, y = mean_ts,fill = grupo)) +
  geom_col(width = 0.6,
           show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_ts - sd_ts,
        ymax = mean_ts + sd_ts),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "WHOQOL-BREF\nenvironmental domain (a.u.)"
  ) +
  theme_classic()

whoqol_ambiente_escore_100_plot

# layout
(whoqol_fisico_escore_100_plot + whoqol_psicol_escore_100_plot)/(whoqol_ambiente_escore_100_plot + whoqol_social_escore_100_plot)

