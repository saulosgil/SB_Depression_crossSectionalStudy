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
  select(-medicamentos,
         -circ_abdominal,
         -circ_cintura)

# pegando os valores os valores de handgrip_direita, tug e ts
df_ajustado <-
  df_ajustado |>
  # separando as tentativas handgrip_direita
  tidyr::separate(col = handgrip_direita,
                  c("handgrip_direita_1", "handgrip_direita_2", "handgrip_direita_3"),
                  sep = "/",
                  convert = TRUE) |>
  # convertendo a handgrip_direita para numerico
  mutate(handgrip_direita_1 = as.numeric(handgrip_direita_1),
         handgrip_direita_2 = as.numeric(handgrip_direita_2),
         handgrip_direita_3 = as.numeric(handgrip_direita_3)) |>
  # separando as tentativas tug
  tidyr::separate(col = tug,
                  c("tug_1", "tug_2", "tug_3"),
                  sep = "/",
                  convert = TRUE) |>
  # convertendo a tug para numerico
  mutate(tug_1 = as.numeric(tug_1),
         tug_2 = as.numeric(tug_2),
         tug_3 = as.numeric(tug_3)) |>
  # separando as tentativas ts
  tidyr::separate(col = ts,
                  c("ts_1", "ts_2", "ts_3"),
                  sep = "/",
                  convert = TRUE) |>
  # convertendo a tug para numerico
  mutate(ts_1 = as.numeric(ts_1),
         ts_2 = as.numeric(ts_2),
         ts_3 = as.numeric(ts_3))

# pegando só o melhor valor do hgs, tug e ts
df_ajustado <-
  df_ajustado |>
  mutate(hgs_max = apply(df_ajustado[,10:12], MARGIN = 1,FUN = max),
         tug_max = apply(df_ajustado[,13:15], MARGIN = 1,FUN = max),
         ts_max = apply (df_ajustado[,16:18], MARGIN = 1,FUN = max)
         ) |>
  select(-handgrip_direita_1,
         -handgrip_direita_2,
         -handgrip_direita_3,
         -tug_1,
         -tug_2,
         -tug_3,
         -ts_1,
         -ts_2,
         -ts_3
         )

# Calculando o score de ansiedade e depressao
df_ajustado <-
  df_ajustado |>
  mutate(
    # escore de ansiedade
    ansiedade_score = apply(df_ajustado[,28:48],MARGIN = 1,FUN = sum),
    # escore de depressão
    depressao_score = apply(df_ajustado[,49:69],MARGIN = 1,FUN = sum),
  ) |>
  mutate(
    # ajuste devido o escore estar vindo de 1 a 4 e não 0 a 3 como deveria!
    ansiedade_score = ansiedade_score - 21
  ) |>
  # removendo as colunas que não iremos usar
  select(
    -starts_with("ansiedade_beck"),
    -starts_with("depressao_beck")
  )

df_ajustado |>
  select(starts_with("who"))


# Calculando WoL em todos os dominios
df_ajustado|>
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


# Calculando IPAC -----------------------------------------------------------------------------
df_ajustado <-
  df_ajustado |>
  mutate(
    caminhada_10min_minday = (ipac_1a * ipac_1b)/7,
    atv_moderada_minday = (ipac_2a * ipac_2b)/7,
    atv_vigorosa_minday = (ipac_3a * ipac_3b)/7,
    mvpa_minday = atv_moderada_minday + atv_vigorosa_minday,
    tempo_sentado_minday = ipac_4a,
    tempo_sentado_fds_minday = ipac_4b
  ) |>
  mutate(
    caminhada_10min_minday = round(caminhada_10min_minday, 0),
    atv_moderada_minday = round(atv_moderada_minday, 0),
    atv_vigorosa_minday = round(atv_vigorosa_minday, 0),
    mvpa_minday = round(atv_moderada_minday + atv_vigorosa_minday, 0),
    tempo_sentado_minday = round(ipac_4a, 0),
    tempo_sentado_fds_minday = round(ipac_4b, 0)
  )  |>
  select(-starts_with("ipac"))

# Calculando LASA -----------------------------------------------------------------------------
df_ajustado <-
  df_ajustado |>
  # separando as tentativas soneca
  tidyr::separate(
    col = sbq_1a_1b,
    c(
      "soneca_manha_sem_minday",
      "soneca_tarde_sem_minday",
      "soneca_noite_sem_minday",
      "soneca_manha_fds_minday",
      "soneca_tarde_fds_minday",
      "soneca_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    soneca_manha_sem_minday = as.integer(soneca_manha_sem_minday),
    soneca_tarde_sem_minday = as.integer(soneca_tarde_sem_minday),
    soneca_noite_sem_minday = as.integer(soneca_noite_sem_minday),
    soneca_manha_fds_minday = as.integer(soneca_manha_fds_minday),
    soneca_tarde_fds_minday = as.integer(soneca_tarde_fds_minday),
    soneca_noite_fds_minday = as.integer(soneca_noite_fds_minday)
  ) |>
  mutate(
    soneca_manha_sem_minday = if_else(is.na(soneca_manha_sem_minday), 0, soneca_manha_sem_minday),
    soneca_tarde_sem_minday = if_else(is.na(soneca_tarde_sem_minday), 0, soneca_tarde_sem_minday),
    soneca_noite_sem_minday = if_else(is.na(soneca_noite_sem_minday), 0, soneca_noite_sem_minday),
    soneca_manha_fds_minday = if_else(is.na(soneca_manha_fds_minday), 0, soneca_manha_fds_minday),
    soneca_tarde_fds_minday = if_else(is.na(soneca_tarde_fds_minday), 0, soneca_tarde_fds_minday),
    soneca_noite_fds_minday = if_else(is.na(soneca_noite_fds_minday), 0, soneca_noite_fds_minday)
  ) |>
  mutate(
    total_soneca_sem_minday = soneca_manha_sem_minday + soneca_tarde_sem_minday + soneca_noite_sem_minday,
    total_soneca_minday_fds = soneca_manha_fds_minday + soneca_tarde_fds_minday + soneca_noite_fds_minday
  ) |>
  mutate(
    total_soneca_minday = (total_soneca_sem_minday * 5 + total_soneca_minday_fds * 2)/7
  ) |>
  # separando as tentativas leitura
  tidyr::separate(
    col = sbq_2a_2b,
    c(
      "leitura_manha_sem_minday",
      "leitura_tarde_sem_minday",
      "leitura_noite_sem_minday",
      "leitura_manha_fds_minday",
      "leitura_tarde_fds_minday",
      "leitura_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    leitura_manha_sem_minday = as.integer(leitura_manha_sem_minday),
    leitura_tarde_sem_minday = as.integer(leitura_tarde_sem_minday),
    leitura_noite_sem_minday = as.integer(leitura_noite_sem_minday),
    leitura_manha_fds_minday = as.integer(leitura_manha_fds_minday),
    leitura_tarde_fds_minday = as.integer(leitura_tarde_fds_minday),
    leitura_noite_fds_minday = as.integer(leitura_noite_fds_minday)
  ) |>
  mutate(
    leitura_manha_sem_minday = if_else(is.na(leitura_manha_sem_minday), 0, leitura_manha_sem_minday),
    leitura_tarde_sem_minday = if_else(is.na(leitura_tarde_sem_minday), 0, leitura_tarde_sem_minday),
    leitura_noite_sem_minday = if_else(is.na(leitura_noite_sem_minday), 0, leitura_noite_sem_minday),
    leitura_manha_fds_minday = if_else(is.na(leitura_manha_fds_minday), 0, leitura_manha_fds_minday),
    leitura_tarde_fds_minday = if_else(is.na(leitura_tarde_fds_minday), 0, leitura_tarde_fds_minday),
    leitura_noite_fds_minday = if_else(is.na(leitura_noite_fds_minday), 0, leitura_noite_fds_minday)
  ) |>
  mutate(
    total_leitura_sem_minday = leitura_manha_sem_minday + leitura_tarde_sem_minday + leitura_noite_sem_minday,
    total_leitura_minday_fds = leitura_manha_fds_minday + leitura_tarde_fds_minday + leitura_noite_fds_minday
  ) |>
  mutate(
    total_leitura_minday = (total_leitura_sem_minday * 5 + total_leitura_minday_fds * 2)/7
  ) |>
  # separando as tentativas oracoes musica
  tidyr::separate(
    col = sbq_3a_3b,
    c(
      "musica_manha_sem_minday",
      "musica_tarde_sem_minday",
      "musica_noite_sem_minday",
      "musica_manha_fds_minday",
      "musica_tarde_fds_minday",
      "musica_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    musica_manha_sem_minday = as.integer(musica_manha_sem_minday),
    musica_tarde_sem_minday = as.integer(musica_tarde_sem_minday),
    musica_noite_sem_minday = as.integer(musica_noite_sem_minday),
    musica_manha_fds_minday = as.integer(musica_manha_fds_minday),
    musica_tarde_fds_minday = as.integer(musica_tarde_fds_minday),
    musica_noite_fds_minday = as.integer(musica_noite_fds_minday)
  ) |>
  mutate(
    musica_manha_sem_minday = if_else(is.na(musica_manha_sem_minday), 0, musica_manha_sem_minday),
    musica_tarde_sem_minday = if_else(is.na(musica_tarde_sem_minday), 0, musica_tarde_sem_minday),
    musica_noite_sem_minday = if_else(is.na(musica_noite_sem_minday), 0, musica_noite_sem_minday),
    musica_manha_fds_minday = if_else(is.na(musica_manha_fds_minday), 0, musica_manha_fds_minday),
    musica_tarde_fds_minday = if_else(is.na(musica_tarde_fds_minday), 0, musica_tarde_fds_minday),
    musica_noite_fds_minday = if_else(is.na(musica_noite_fds_minday), 0, musica_noite_fds_minday)
  ) |>
  mutate(
    total_musica_sem_minday = musica_manha_sem_minday + musica_tarde_sem_minday + musica_noite_sem_minday,
    total_musica_minday_fds = musica_manha_fds_minday + musica_tarde_fds_minday + musica_noite_fds_minday
  ) |>
  mutate(
    total_musica_minday = (total_musica_sem_minday * 5 + total_musica_minday_fds * 2)/7
  ) |>
  # separando as tentativas tv
  tidyr::separate(
    col = sbq_4a_4b,
    c(
      "tv_manha_sem_minday",
      "tv_tarde_sem_minday",
      "tv_noite_sem_minday",
      "tv_manha_fds_minday",
      "tv_tarde_fds_minday",
      "tv_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    tv_manha_sem_minday = as.integer(tv_manha_sem_minday),
    tv_tarde_sem_minday = as.integer(tv_tarde_sem_minday),
    tv_noite_sem_minday = as.integer(tv_noite_sem_minday),
    tv_manha_fds_minday = as.integer(tv_manha_fds_minday),
    tv_tarde_fds_minday = as.integer(tv_tarde_fds_minday),
    tv_noite_fds_minday = as.integer(tv_noite_fds_minday)
  ) |>
  mutate(
    tv_manha_sem_minday = if_else(is.na(tv_manha_sem_minday), 0, tv_manha_sem_minday),
    tv_tarde_sem_minday = if_else(is.na(tv_tarde_sem_minday), 0, tv_tarde_sem_minday),
    tv_noite_sem_minday = if_else(is.na(tv_noite_sem_minday), 0, tv_noite_sem_minday),
    tv_manha_fds_minday = if_else(is.na(tv_manha_fds_minday), 0, tv_manha_fds_minday),
    tv_tarde_fds_minday = if_else(is.na(tv_tarde_fds_minday), 0, tv_tarde_fds_minday),
    tv_noite_fds_minday = if_else(is.na(tv_noite_fds_minday), 0, tv_noite_fds_minday)
  ) |>
  mutate(
    total_tv_sem_minday = tv_manha_sem_minday + tv_tarde_sem_minday + tv_noite_sem_minday,
    total_tv_minday_fds = tv_manha_fds_minday + tv_tarde_fds_minday + tv_noite_fds_minday
  ) |>
  mutate(
    total_tv_minday = (total_tv_sem_minday * 5 + total_tv_minday_fds * 2)/7
  ) |>
  # separando as tentativas jogos
  tidyr::separate(
    col = sbq_5a_5b,
    c(
      "jogos_manha_sem_minday",
      "jogos_tarde_sem_minday",
      "jogos_noite_sem_minday",
      "jogos_manha_fds_minday",
      "jogos_tarde_fds_minday",
      "jogos_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    jogos_manha_sem_minday = as.integer(jogos_manha_sem_minday),
    jogos_tarde_sem_minday = as.integer(jogos_tarde_sem_minday),
    jogos_noite_sem_minday = as.integer(jogos_noite_sem_minday),
    jogos_manha_fds_minday = as.integer(jogos_manha_fds_minday),
    jogos_tarde_fds_minday = as.integer(jogos_tarde_fds_minday),
    jogos_noite_fds_minday = as.integer(jogos_noite_fds_minday)
  ) |>
  mutate(
    jogos_manha_sem_minday = if_else(is.na(jogos_manha_sem_minday), 0, jogos_manha_sem_minday),
    jogos_tarde_sem_minday = if_else(is.na(jogos_tarde_sem_minday), 0, jogos_tarde_sem_minday),
    jogos_noite_sem_minday = if_else(is.na(jogos_noite_sem_minday), 0, jogos_noite_sem_minday),
    jogos_manha_fds_minday = if_else(is.na(jogos_manha_fds_minday), 0, jogos_manha_fds_minday),
    jogos_tarde_fds_minday = if_else(is.na(jogos_tarde_fds_minday), 0, jogos_tarde_fds_minday),
    jogos_noite_fds_minday = if_else(is.na(jogos_noite_fds_minday), 0, jogos_noite_fds_minday)
  ) |>
  mutate(
    total_jogos_sem_minday = jogos_manha_sem_minday + jogos_tarde_sem_minday + jogos_noite_sem_minday,
    total_jogos_minday_fds = jogos_manha_fds_minday + jogos_tarde_fds_minday + jogos_noite_fds_minday
  ) |>
  mutate(
    total_jogos_minday = (total_jogos_sem_minday * 5 + total_jogos_minday_fds * 2)/7
  ) |>
  # separando as tentativas telefone
  tidyr::separate(
    col = sbq_6a_6b,
    c(
      "telefone_manha_sem_minday",
      "telefone_tarde_sem_minday",
      "telefone_noite_sem_minday",
      "telefone_manha_fds_minday",
      "telefone_tarde_fds_minday",
      "telefone_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    telefone_manha_sem_minday = as.integer(telefone_manha_sem_minday),
    telefone_tarde_sem_minday = as.integer(telefone_tarde_sem_minday),
    telefone_noite_sem_minday = as.integer(telefone_noite_sem_minday),
    telefone_manha_fds_minday = as.integer(telefone_manha_fds_minday),
    telefone_tarde_fds_minday = as.integer(telefone_tarde_fds_minday),
    telefone_noite_fds_minday = as.integer(telefone_noite_fds_minday)
  ) |>
  mutate(
    telefone_manha_sem_minday = if_else(is.na(telefone_manha_sem_minday), 0, telefone_manha_sem_minday),
    telefone_tarde_sem_minday = if_else(is.na(telefone_tarde_sem_minday), 0, telefone_tarde_sem_minday),
    telefone_noite_sem_minday = if_else(is.na(telefone_noite_sem_minday), 0, telefone_noite_sem_minday),
    telefone_manha_fds_minday = if_else(is.na(telefone_manha_fds_minday), 0, telefone_manha_fds_minday),
    telefone_tarde_fds_minday = if_else(is.na(telefone_tarde_fds_minday), 0, telefone_tarde_fds_minday),
    telefone_noite_fds_minday = if_else(is.na(telefone_noite_fds_minday), 0, telefone_noite_fds_minday)
  ) |>
  mutate(
    total_telefone_sem_minday = telefone_manha_sem_minday + telefone_tarde_sem_minday + telefone_noite_sem_minday,
    total_telefone_minday_fds = telefone_manha_fds_minday + telefone_tarde_fds_minday + telefone_noite_fds_minday
  ) |>
  mutate(
    total_telefone_minday = (total_telefone_sem_minday * 5 + total_telefone_minday_fds * 2)/7
  ) |>
  # separando as tentativas telefone
  tidyr::separate(
    col = sbq_7a_7b,
    c(
      "pc_manha_sem_minday",
      "pc_tarde_sem_minday",
      "pc_noite_sem_minday",
      "pc_manha_fds_minday",
      "pc_tarde_fds_minday",
      "pc_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    pc_manha_sem_minday = as.integer(pc_manha_sem_minday),
    pc_tarde_sem_minday = as.integer(pc_tarde_sem_minday),
    pc_noite_sem_minday = as.integer(pc_noite_sem_minday),
    pc_manha_fds_minday = as.integer(pc_manha_fds_minday),
    pc_tarde_fds_minday = as.integer(pc_tarde_fds_minday),
    pc_noite_fds_minday = as.integer(pc_noite_fds_minday)
  ) |>
  mutate(
    pc_manha_sem_minday = if_else(is.na(pc_manha_sem_minday), 0, pc_manha_sem_minday),
    pc_tarde_sem_minday = if_else(is.na(pc_tarde_sem_minday), 0, pc_tarde_sem_minday),
    pc_noite_sem_minday = if_else(is.na(pc_noite_sem_minday), 0, pc_noite_sem_minday),
    pc_manha_fds_minday = if_else(is.na(pc_manha_fds_minday), 0, pc_manha_fds_minday),
    pc_tarde_fds_minday = if_else(is.na(pc_tarde_fds_minday), 0, pc_tarde_fds_minday),
    pc_noite_fds_minday = if_else(is.na(pc_noite_fds_minday), 0, pc_noite_fds_minday)
  ) |>
  mutate(
    total_pc_sem_minday = pc_manha_sem_minday + pc_tarde_sem_minday + pc_noite_sem_minday,
    total_pc_minday_fds = pc_manha_fds_minday + pc_tarde_fds_minday + pc_noite_fds_minday
  ) |>
  mutate(
    total_pc_minday = (total_pc_sem_minday * 5 + total_pc_minday_fds * 2)/7
  ) |>
  # separando as tentativas atvdomesticas
  tidyr::separate(
    col = sbq_8a_8b,
    c(
      "atvdomesticas_manha_sem_minday",
      "atvdomesticas_tarde_sem_minday",
      "atvdomesticas_noite_sem_minday",
      "atvdomesticas_manha_fds_minday",
      "atvdomesticas_tarde_fds_minday",
      "atvdomesticas_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    atvdomesticas_manha_sem_minday = as.integer(atvdomesticas_manha_sem_minday),
    atvdomesticas_tarde_sem_minday = as.integer(atvdomesticas_tarde_sem_minday),
    atvdomesticas_noite_sem_minday = as.integer(atvdomesticas_noite_sem_minday),
    atvdomesticas_manha_fds_minday = as.integer(atvdomesticas_manha_fds_minday),
    atvdomesticas_tarde_fds_minday = as.integer(atvdomesticas_tarde_fds_minday),
    atvdomesticas_noite_fds_minday = as.integer(atvdomesticas_noite_fds_minday)
  ) |>
  mutate(
    atvdomesticas_manha_sem_minday = if_else(is.na(atvdomesticas_manha_sem_minday), 0, atvdomesticas_manha_sem_minday),
    atvdomesticas_tarde_sem_minday = if_else(is.na(atvdomesticas_tarde_sem_minday), 0, atvdomesticas_tarde_sem_minday),
    atvdomesticas_noite_sem_minday = if_else(is.na(atvdomesticas_noite_sem_minday), 0, atvdomesticas_noite_sem_minday),
    atvdomesticas_manha_fds_minday = if_else(is.na(atvdomesticas_manha_fds_minday), 0, atvdomesticas_manha_fds_minday),
    atvdomesticas_tarde_fds_minday = if_else(is.na(atvdomesticas_tarde_fds_minday), 0, atvdomesticas_tarde_fds_minday),
    atvdomesticas_noite_fds_minday = if_else(is.na(atvdomesticas_noite_fds_minday), 0, atvdomesticas_noite_fds_minday)
  ) |>
  mutate(
    total_atvdomesticas_sem_minday = atvdomesticas_manha_sem_minday + atvdomesticas_tarde_sem_minday + atvdomesticas_noite_sem_minday,
    total_atvdomesticas_minday_fds = atvdomesticas_manha_fds_minday + atvdomesticas_tarde_fds_minday + atvdomesticas_noite_fds_minday
  ) |>
  mutate(
    total_atvdomesticas_minday = (total_atvdomesticas_sem_minday * 5 + total_atvdomesticas_minday_fds * 2)/7
  ) |>
  # separando as tentativas transporte
  tidyr::separate(
    col = sbq_9a_9b,
    c(
      "transporte_manha_sem_minday",
      "transporte_tarde_sem_minday",
      "transporte_noite_sem_minday",
      "transporte_manha_fds_minday",
      "transporte_tarde_fds_minday",
      "transporte_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    transporte_manha_sem_minday = as.integer(transporte_manha_sem_minday),
    transporte_tarde_sem_minday = as.integer(transporte_tarde_sem_minday),
    transporte_noite_sem_minday = as.integer(transporte_noite_sem_minday),
    transporte_manha_fds_minday = as.integer(transporte_manha_fds_minday),
    transporte_tarde_fds_minday = as.integer(transporte_tarde_fds_minday),
    transporte_noite_fds_minday = as.integer(transporte_noite_fds_minday)
  ) |>
  mutate(
    transporte_manha_sem_minday = if_else(is.na(transporte_manha_sem_minday), 0, transporte_manha_sem_minday),
    transporte_tarde_sem_minday = if_else(is.na(transporte_tarde_sem_minday), 0, transporte_tarde_sem_minday),
    transporte_noite_sem_minday = if_else(is.na(transporte_noite_sem_minday), 0, transporte_noite_sem_minday),
    transporte_manha_fds_minday = if_else(is.na(transporte_manha_fds_minday), 0, transporte_manha_fds_minday),
    transporte_tarde_fds_minday = if_else(is.na(transporte_tarde_fds_minday), 0, transporte_tarde_fds_minday),
    transporte_noite_fds_minday = if_else(is.na(transporte_noite_fds_minday), 0, transporte_noite_fds_minday)
  ) |>
  mutate(
    total_transporte_sem_minday = transporte_manha_sem_minday + transporte_tarde_sem_minday + transporte_noite_sem_minday,
    total_transporte_minday_fds = transporte_manha_fds_minday + transporte_tarde_fds_minday + transporte_noite_fds_minday
  ) |>
  mutate(
    total_transporte_minday = (total_transporte_sem_minday * 5 + total_transporte_minday_fds * 2)/7
  ) |>
  # separando as tentativas igreja ou atvculturais
  tidyr::separate(
    col = sbq_10a_10b,
    c(
      "atvculturais_manha_sem_minday",
      "atvculturais_tarde_sem_minday",
      "atvculturais_noite_sem_minday",
      "atvculturais_manha_fds_minday",
      "atvculturais_tarde_fds_minday",
      "atvculturais_noite_fds_minday"
    ),

    sep = "/",
    convert = TRUE
  ) |>
  mutate(
    atvculturais_manha_sem_minday = as.integer(atvculturais_manha_sem_minday),
    atvculturais_tarde_sem_minday = as.integer(atvculturais_tarde_sem_minday),
    atvculturais_noite_sem_minday = as.integer(atvculturais_noite_sem_minday),
    atvculturais_manha_fds_minday = as.integer(atvculturais_manha_fds_minday),
    atvculturais_tarde_fds_minday = as.integer(atvculturais_tarde_fds_minday),
    atvculturais_noite_fds_minday = as.integer(atvculturais_noite_fds_minday)
  ) |>
  mutate(
    atvculturais_manha_sem_minday = if_else(is.na(atvculturais_manha_sem_minday), 0, atvculturais_manha_sem_minday),
    atvculturais_tarde_sem_minday = if_else(is.na(atvculturais_tarde_sem_minday), 0, atvculturais_tarde_sem_minday),
    atvculturais_noite_sem_minday = if_else(is.na(atvculturais_noite_sem_minday), 0, atvculturais_noite_sem_minday),
    atvculturais_manha_fds_minday = if_else(is.na(atvculturais_manha_fds_minday), 0, atvculturais_manha_fds_minday),
    atvculturais_tarde_fds_minday = if_else(is.na(atvculturais_tarde_fds_minday), 0, atvculturais_tarde_fds_minday),
    atvculturais_noite_fds_minday = if_else(is.na(atvculturais_noite_fds_minday), 0, atvculturais_noite_fds_minday)
  ) |>
  mutate(
    total_atvculturais_sem_minday = atvculturais_manha_sem_minday + atvculturais_tarde_sem_minday + atvculturais_noite_sem_minday,
    total_atvculturais_minday_fds = atvculturais_manha_fds_minday + atvculturais_tarde_fds_minday + atvculturais_noite_fds_minday
  ) |>
  mutate(
    total_atvculturais_minday = (total_atvculturais_sem_minday * 5 + total_atvculturais_minday_fds * 2)/7
  )

# remove idade < 18
df_ajustado <-
  df_ajustado |>
  filter(
    idade > 17
  ) |>
  mutate(
    idade = if_else(idade > 100, mean(idade), idade),
    idade = round(idade, 0)
  )

# arredondando os valores total de Atv e  SB em cada dominio
df_ajustado <-
  df_ajustado |>
  mutate(
    total_soneca_minday = round(total_soneca_minday, 0),
    total_leitura_minday = round(total_leitura_minday, 0),
    total_musica_minday = round(total_musica_minday, 0),
    total_tv_minday = round(total_tv_minday, 0),
    total_jogos_minday = round(total_jogos_minday, 0),
    total_telefone_minday = round(total_telefone_minday, 0),
    total_pc_minday = round(total_pc_minday, 0),
    total_atvdomesticas_minday = round(total_atvdomesticas_minday, 0),
    total_transporte_minday = round(total_transporte_minday, 0),
    total_atvculturais_minday = round(total_atvculturais_minday, 0)
  )

# Parei aqui ----------------------------------------------------------------------------------
write_rds(x = df_ajustado,file =  "df_para_analise.rds")
df_ajustado <- readr::read_rds("df_para_analise.rds")
glimpse(df_ajustado)


# TODO ---------------------------------------------------------------------------------------
# 1 - verificar se o calculo ta sendo feito certo [(weekday time * 5 + weekend day time * 2) / 7 ];
# 2 - Tratar os valores extremos do SB;
# 3 - calcular a soma do SB;
# 4 - calcular SB mentalmente passivo e ativo


# falta calcular SB mentalmente passivo e ativo

df_ajustado |>
  select(
    starts_with("total_")
  ) |> view()


