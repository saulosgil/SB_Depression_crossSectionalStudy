# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# Lendo a base para ajustes -------------------------------------------------------------------
df_renomeado <- readr::read_rds("database_17122025.rds")

# Excluindo os duplicados para analise transversal --------------------------------------------
# Ajustando o nome das variaveis
df_renomeado <- janitor::clean_names(df_renomeado)

# Pegando somente quem tem racimed e excluindo que tem um segundo registro (reavaliação)
com_racimed <-
  df_renomeado |>
  filter(!is.na(racimed)) |>
  distinct(racimed,.keep_all = TRUE)

# Pegando quem não tem racimed
racimed_na <-
  df_renomeado |>
  filter(is.na(racimed))

# Juntando as bases
df <-
  bind_rows(com_racimed,
            racimed_na)

# Selecionando as variaveis que iremos usar ---------------------------------------------------
df_paper <-
  df |>
  select(
    nome,
    data_nasc,
    estado_civil,
    genero,
    raca,
    renda,
    peso,
    estatura,
    comorbidades,
    medicamentos,
    percentual_gordura,
    percentual_mm,
    circ_abdominal,
    circ_cintura,
    handgrip_direita,
    tug,
    ts,
    starts_with("ipac"),
    starts_with("sbq"),
    starts_with("ansiedade"),
    starts_with("depressao"),
    starts_with("who_qol")
  )

# tratamento das variáveis que aparecem como list ---------------------------------------------
# data de nascimento
# tira da lista e cria um vetor de datas de nascimento (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$data_nasc,
            use.names = TRUE)

# Muda a barra para hífen para depois converter para data
d <- str_replace_all(d,  "/", "")

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
data_nasc_ajustado <- bind_rows(d, c_d)
data_nasc_ajustado <- data_nasc_ajustado$value
data_nasc_ajustado <- substr(data_nasc_ajustado, nchar(data_nasc_ajustado) - 3, nchar(data_nasc_ajustado))
data_nasc_ajustado <- as.numeric(data_nasc_ajustado)
data_nasc_ajustado <- 2025 - data_nasc_ajustado
data_nasc_ajustado

# Insere a idade no df
df_paper <-
  df_paper |>
  mutate(
    data_nasc = data_nasc_ajustado
  )

# peso
# tira da lista e cria um vetor do peso (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$peso,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
peso_ajustado <- bind_rows(d, c_d)
peso_ajustado <- as.numeric(peso_ajustado$value)
peso_ajustado

# Insere o peso no df
df_paper <-
  df_paper |>
  mutate(
    peso = peso_ajustado
  )

# estatura
# tira da lista e cria um vetor do estatura (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$estatura,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
estatura_ajustado <- bind_rows(d, c_d)
estatura_ajustado <- as.numeric(estatura_ajustado$value)
estatura_ajustado

# Insere a estatura no df
df_paper <-
  df_paper |>
  mutate(
    estatura = estatura_ajustado

  )

# percentual_gordura
# tira da lista e cria um vetor do percentual_gordura (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$percentual_gordura,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
percentual_gordura_ajustado <- bind_rows(d, c_d)
percentual_gordura_ajustado <- as.numeric(percentual_gordura_ajustado$value)
percentual_gordura_ajustado

# Insere o percentual_gordura no df
df_paper <-
  df_paper |>
  mutate(
    percentual_gordura = percentual_gordura_ajustado
  )

# percentual_mm
# tira da lista e cria um vetor do percentual_mm (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$percentual_mm,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
percentual_mm_ajustado <- bind_rows(d, c_d)
percentual_mm_ajustado <- as.numeric(percentual_mm_ajustado$value)
percentual_mm_ajustado

# Insere o percentual_mm no df
df_paper <-
  df_paper |>
  mutate(
    percentual_mm = percentual_mm_ajustado
  )

# circ_abdominal
# tira da lista e cria um vetor do circ_abdominal (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$circ_abdominal,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
circ_abdominal_ajustado <- bind_rows(d, c_d)
circ_abdominal_ajustado <- circ_abdominal_ajustado$value
circ_abdominal_ajustado

# Insere o circ_abdominal no df
df_paper <-
  df_paper |>
  mutate(
    circ_abdominal = circ_abdominal_ajustado
  )

# circ_cintura
# tira da lista e cria um vetor do circ_cintura (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$circ_cintura,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
circ_cintura_ajustado <- bind_rows(d, c_d)
circ_cintura_ajustado <- circ_cintura_ajustado$value
circ_cintura_ajustado

# Insere o circ_cintura no df
df_paper <-
  df_paper |>
  mutate(
    circ_cintura = circ_cintura_ajustado
  )

# TUG
# tira da lista e cria um vetor do TUG (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$tug,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
tug_ajustado <- bind_rows(d, c_d)
tug_ajustado <- tug_ajustado$value
tug_ajustado

# Insere o TUG no df
df_paper <-
  df_paper |>
  mutate(
    tug = tug_ajustado
  )

# TS
# tira da lista e cria um vetor do TS (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ts,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ts_ajustado <- bind_rows(d, c_d)
ts_ajustado <- ts_ajustado$value
ts_ajustado

# Insere o TUG no df
df_paper <-
  df_paper |>
  mutate(
    ts = ts_ajustado
  )

# ipac_1b
# tira da lista e cria um vetor do ipac_1b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ipac_1b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ipac_1b_ajustado <- bind_rows(d, c_d)
ipac_1b_ajustado <- ipac_1b_ajustado$value
ipac_1b_ajustado <- as.numeric(ipac_1b_ajustado)
ipac_1b_ajustado

# Insere o TUG no df
df_paper <-
  df_paper |>
  mutate(
    ipac_1b = ipac_1b_ajustado
  )

# ipac_2b
# tira da lista e cria um vetor do ipac_2b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ipac_2b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ipac_2b_ajustado <- bind_rows(d, c_d)
ipac_2b_ajustado <- ipac_2b_ajustado$value
ipac_2b_ajustado <- as.numeric(ipac_2b_ajustado)
ipac_2b_ajustado

# Insere o TUG no df
df_paper <-
  df_paper |>
  mutate(
    ipac_2b = ipac_2b_ajustado
  )

# ipac_3b
# tira da lista e cria um vetor do ipac_2b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ipac_3b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ipac_3b_ajustado <- bind_rows(d, c_d)
ipac_3b_ajustado <- ipac_3b_ajustado$value
ipac_3b_ajustado <- as.numeric(ipac_3b_ajustado)
ipac_3b_ajustado

# Insere o TUG no df
df_paper <-
  df_paper |>
  mutate(
    ipac_3b = ipac_3b_ajustado
  )

# ipac_4a
# tira da lista e cria um vetor do ipac_2b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ipac_4a,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ipac_4a_ajustado <- bind_rows(d, c_d)
ipac_4a_ajustado <- ipac_4a_ajustado$value
ipac_4a_ajustado <- as.numeric(ipac_4a_ajustado)
ipac_4a_ajustado

# Insere o TUG no df
df_paper <-
  df_paper |>
  mutate(
    ipac_4a = ipac_4a_ajustado
  )

# ipac_4b
# tira da lista e cria um vetor do ipac_2b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ipac_4b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ipac_4b_ajustado <- bind_rows(d, c_d)
ipac_4b_ajustado <- ipac_4b_ajustado$value
ipac_4b_ajustado <- as.numeric(ipac_4b_ajustado)
ipac_4b_ajustado

# Insere o TUG no df
df_paper <-
  df_paper |>
  mutate(
    ipac_4b = ipac_4b_ajustado
  )

# sbq_1a_1b
# tira da lista e cria um vetor do ipac_2b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_1a_1b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_1a_1b_ajustado <- bind_rows(d, c_d)
sbq_1a_1b_ajustado <- sbq_1a_1b_ajustado$value
sbq_1a_1b_ajustado

# Insere o TUG no df
df_paper <-
  df_paper |>
  mutate(
    sbq_1a_1b = sbq_1a_1b_ajustado
  )

# sbq_2a_2b
# tira da lista e cria um vetor do sbq_2a_2b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_2a_2b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_2a_2b_ajustado <- bind_rows(d, c_d)
sbq_2a_2b_ajustado <- sbq_2a_2b_ajustado$value
sbq_2a_2b_ajustado

# Insere o sbq_2a_2b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_2a_2b = sbq_2a_2b_ajustado
  )

# sbq_3a_3b
# tira da lista e cria um vetor do sbq_3a_3b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_3a_3b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_3a_3b_ajustado <- bind_rows(d, c_d)
sbq_3a_3b_ajustado <- sbq_3a_3b_ajustado$value
sbq_3a_3b_ajustado

# Insere o sbq_3a_3b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_3a_3b = sbq_3a_3b_ajustado
  )

# sbq_4a_4b
# tira da lista e cria um vetor do sbq_4a_4b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_4a_4b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_4a_4b_ajustado <- bind_rows(d, c_d)
sbq_4a_4b_ajustado <- sbq_4a_4b_ajustado$value
sbq_4a_4b_ajustado

# Insere o sbq_4a_4b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_4a_4b = sbq_4a_4b_ajustado
  )

# sbq_5a_5b
# tira da lista e cria um vetor do sbq_5a_5b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_5a_5b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_5a_5b_ajustado <- bind_rows(d, c_d)
sbq_5a_5b_ajustado <- sbq_5a_5b_ajustado$value
sbq_5a_5b_ajustado

# Insere o sbq_5a_5b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_5a_5b = sbq_5a_5b_ajustado
  )

# sbq_6a_6b
# tira da lista e cria um vetor do sbq_6a_6b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_6a_6b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_6a_6b_ajustado <- bind_rows(d, c_d)
sbq_6a_6b_ajustado <- sbq_6a_6b_ajustado$value
sbq_6a_6b_ajustado

# Insere o sbq_6a_6b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_6a_6b = sbq_6a_6b_ajustado
  )

# sbq_7a_7b
# tira da lista e cria um vetor do sbq_7a_7b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_7a_7b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_7a_7b_ajustado <- bind_rows(d, c_d)
sbq_7a_7b_ajustado <- sbq_7a_7b_ajustado$value
sbq_7a_7b_ajustado

# Insere o sbq_7a_7b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_7a_7b = sbq_7a_7b_ajustado
  )

# sbq_8a_8b
# tira da lista e cria um vetor do sbq_8a_8b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_8a_8b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_8a_8b_ajustado <- bind_rows(d, c_d)
sbq_8a_8b_ajustado <- sbq_8a_8b_ajustado$value
sbq_8a_8b_ajustado

# Insere o sbq_8a_8b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_8a_8b = sbq_8a_8b_ajustado
  )

# sbq_9a_9b
# tira da lista e cria um vetor do sbq_9a_9b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_9a_9b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_9a_9b_ajustado <- bind_rows(d, c_d)
sbq_9a_9b_ajustado <- sbq_9a_9b_ajustado$value
sbq_9a_9b_ajustado

# Insere o sbq_9a_9b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_9a_9b = sbq_9a_9b_ajustado
  )

# sbq_10a_10b
# tira da lista e cria um vetor do sbq_10a_10b (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$sbq_10a_10b,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
sbq_10a_10b_ajustado <- bind_rows(d, c_d)
sbq_10a_10b_ajustado <- sbq_10a_10b_ajustado$value
sbq_10a_10b_ajustado

# Insere o sbq_10a_10b no df
df_paper <-
  df_paper |>
  mutate(
    sbq_10a_10b = sbq_10a_10b_ajustado
  )

# ansiedade_beck_1
# tira da lista e cria um vetor do ansiedade_beck_1 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_1,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_1_ajustado <- bind_rows(d, c_d)
ansiedade_beck_1_ajustado <- ansiedade_beck_1_ajustado$value
ansiedade_beck_1_ajustado <- as.integer(ansiedade_beck_1_ajustado)
ansiedade_beck_1_ajustado

# Insere o ansiedade_beck_1 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_1 = ansiedade_beck_1_ajustado
  )

# ansiedade_beck_2
# tira da lista e cria um vetor do ansiedade_beck_2 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_2,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_2_ajustado <- bind_rows(d, c_d)
ansiedade_beck_2_ajustado <- ansiedade_beck_2_ajustado$value
ansiedade_beck_2_ajustado <- as.integer(ansiedade_beck_2_ajustado)
ansiedade_beck_2_ajustado

# Insere o ansiedade_beck_2 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_2 = ansiedade_beck_2_ajustado
  )

# ansiedade_beck_3
# tira da lista e cria um vetor do ansiedade_beck_3 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_3,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_3_ajustado <- bind_rows(d, c_d)
ansiedade_beck_3_ajustado <- ansiedade_beck_3_ajustado$value
ansiedade_beck_3_ajustado <- as.integer(ansiedade_beck_3_ajustado)
ansiedade_beck_3_ajustado

# Insere o ansiedade_beck_3 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_3 = ansiedade_beck_3_ajustado
  )

# ansiedade_beck_4
# tira da lista e cria um vetor do ansiedade_beck_4 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_4,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_4_ajustado <- bind_rows(d, c_d)
ansiedade_beck_4_ajustado <- ansiedade_beck_4_ajustado$value
ansiedade_beck_4_ajustado <- as.integer(ansiedade_beck_4_ajustado)
ansiedade_beck_4_ajustado

# Insere o ansiedade_beck_4 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_4 = ansiedade_beck_4_ajustado
  )

# ansiedade_beck_5
# tira da lista e cria um vetor do ansiedade_beck_5 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_5,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_5_ajustado <- bind_rows(d, c_d)
ansiedade_beck_5_ajustado <- ansiedade_beck_5_ajustado$value
ansiedade_beck_5_ajustado <- as.integer(ansiedade_beck_5_ajustado)
ansiedade_beck_5_ajustado

# Insere o ansiedade_beck_5 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_5 = ansiedade_beck_5_ajustado
  )

# ansiedade_beck_8
# tira da lista e cria um vetor do ansiedade_beck_8 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_8,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d <- as.numeric(c_d$value)
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_8_ajustado <- bind_rows(d, c_d)
ansiedade_beck_8_ajustado <- ansiedade_beck_8_ajustado$value
ansiedade_beck_8_ajustado <- as.integer(ansiedade_beck_8_ajustado)
ansiedade_beck_8_ajustado

# Insere o ansiedade_beck_8 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_8 = ansiedade_beck_8_ajustado
  )

# ansiedade_beck_10
# tira da lista e cria um vetor do ansiedade_beck_10 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_10,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_10_ajustado <- bind_rows(d, c_d)
ansiedade_beck_10_ajustado <- ansiedade_beck_10_ajustado$value
ansiedade_beck_10_ajustado <- as.integer(ansiedade_beck_10_ajustado)
ansiedade_beck_10_ajustado

# Insere o ansiedade_beck_10 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_10 = ansiedade_beck_10_ajustado
  )

# ansiedade_beck_11
# tira da lista e cria um vetor do ansiedade_beck_11 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_11,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as.numeric(c_d)
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_11_ajustado <- bind_rows(d, c_d)
ansiedade_beck_11_ajustado <- ansiedade_beck_11_ajustado$value
ansiedade_beck_11_ajustado <- as.integer(ansiedade_beck_11_ajustado)
ansiedade_beck_11_ajustado

# Insere o ansiedade_beck_11 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_11 = ansiedade_beck_11_ajustado
  )

# ansiedade_beck_12
# tira da lista e cria um vetor do ansiedade_beck_12 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$ansiedade_beck_12,
            use.names = TRUE)

# converte em tibble
d <- as.numeric(d)
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d <- as.numeric(c_d$value)
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
ansiedade_beck_12_ajustado <- bind_rows(d, c_d)
ansiedade_beck_12_ajustado <- ansiedade_beck_12_ajustado$value
ansiedade_beck_12_ajustado <- as.integer(ansiedade_beck_12_ajustado)
ansiedade_beck_12_ajustado

# Insere o ansiedade_beck_12 no df
df_paper <-
  df_paper |>
  mutate(
    ansiedade_beck_12 = ansiedade_beck_12_ajustado
  )

# depressao_beck_11
# tira da lista e cria um vetor do depressao_beck_11 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$depressao_beck_11,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
depressao_beck_11_ajustado <- bind_rows(d, c_d)
depressao_beck_11_ajustado <- depressao_beck_11_ajustado$value
depressao_beck_11_ajustado <- as.integer(depressao_beck_11_ajustado)
depressao_beck_11_ajustado

# Insere o depressao_beck_11 no df
df_paper <-
  df_paper |>
  mutate(
    depressao_beck_11 = depressao_beck_11_ajustado
  )

# depressao_beck_12
# tira da lista e cria um vetor do depressao_beck_12 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$depressao_beck_12,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
depressao_beck_12_ajustado <- bind_rows(d, c_d)
depressao_beck_12_ajustado <- depressao_beck_12_ajustado$value
depressao_beck_12_ajustado <- as.integer(depressao_beck_12_ajustado)
depressao_beck_12_ajustado

# Insere o depressao_beck_12 no df
df_paper <-
  df_paper |>
  mutate(
    depressao_beck_12 = depressao_beck_12_ajustado
  )

# depressao_beck_19
# tira da lista e cria um vetor do depressao_beck_19 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$depressao_beck_19,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
depressao_beck_19_ajustado <- bind_rows(d, c_d)
depressao_beck_19_ajustado <- depressao_beck_19_ajustado$value
depressao_beck_19_ajustado <- as.integer(depressao_beck_19_ajustado)
depressao_beck_19_ajustado

# Insere o depressao_beck_19 no df
df_paper <-
  df_paper |>
  mutate(
    depressao_beck_19 = depressao_beck_19_ajustado
  )

# who_qol2
# tira da lista e cria um vetor do who_qol2 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol2,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol2_ajustado <- bind_rows(d, c_d)
who_qol2_ajustado <- who_qol2_ajustado$value
who_qol2_ajustado <- as.integer(who_qol2_ajustado)
who_qol2_ajustado

# Insere o who_qol2 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol2 = who_qol2_ajustado
  )

# who_qol3
# tira da lista e cria um vetor do who_qol3 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol3,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol3_ajustado <- bind_rows(d, c_d)
who_qol3_ajustado <- who_qol3_ajustado$value
who_qol3_ajustado <- as.integer(who_qol3_ajustado)
who_qol3_ajustado

# Insere o who_qol3 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol3 = who_qol3_ajustado
  )

# who_qol4
# tira da lista e cria um vetor do who_qol4 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol4,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as.numeric(c_d)
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol4_ajustado <- bind_rows(d, c_d)
who_qol4_ajustado <- who_qol4_ajustado$value
who_qol4_ajustado <- as.integer(who_qol4_ajustado)
who_qol4_ajustado

# Insere o who_qol4 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol4 = who_qol4_ajustado
  )

# who_qol5
# tira da lista e cria um vetor do who_qol5 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol5,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol5_ajustado <- bind_rows(d, c_d)
who_qol5_ajustado <- who_qol5_ajustado$value
who_qol5_ajustado <- as.integer(who_qol5_ajustado)
who_qol5_ajustado

# Insere o who_qol5 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol5 = who_qol5_ajustado
  )

# who_qol6
# tira da lista e cria um vetor do who_qol6 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol6,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as.numeric(c_d)
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol6_ajustado <- bind_rows(d, c_d)
who_qol6_ajustado <- who_qol6_ajustado$value
who_qol6_ajustado <- as.integer(who_qol6_ajustado)
who_qol6_ajustado

# Insere o who_qol6 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol6 = who_qol6_ajustado
  )

# who_qol7
# tira da lista e cria um vetor do who_qol7 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol7,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol7_ajustado <- bind_rows(d, c_d)
who_qol7_ajustado <- who_qol7_ajustado$value
who_qol7_ajustado <- as.integer(who_qol7_ajustado)
who_qol7_ajustado

# Insere o who_qol7 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol7 = who_qol7_ajustado
  )

# who_qol8
# tira da lista e cria um vetor do who_qol8 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol8,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol8_ajustado <- bind_rows(d, c_d)
who_qol8_ajustado <- who_qol8_ajustado$value
who_qol8_ajustado <- as.integer(who_qol8_ajustado)
who_qol8_ajustado

# Insere o who_qol8 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol8 = who_qol8_ajustado
  )

# who_qol12
# tira da lista e cria um vetor do who_qol12 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol12,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol12_ajustado <- bind_rows(d, c_d)
who_qol12_ajustado <- who_qol12_ajustado$value
who_qol12_ajustado <- as.integer(who_qol12_ajustado)
who_qol12_ajustado

# Insere o who_qol12 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol12 = who_qol12_ajustado
  )

# who_qol13
# tira da lista e cria um vetor do who_qol13 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol13,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol13_ajustado <- bind_rows(d, c_d)
who_qol13_ajustado <- who_qol13_ajustado$value
who_qol13_ajustado <- as.integer(who_qol13_ajustado)
who_qol13_ajustado

# Insere o who_qol13 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol13 = who_qol13_ajustado
  )

# who_qol14
# tira da lista e cria um vetor do who_qol14 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol14,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol14_ajustado <- bind_rows(d, c_d)
who_qol14_ajustado <- who_qol14_ajustado$value
who_qol14_ajustado <- as.integer(who_qol14_ajustado)
who_qol14_ajustado

# Insere o who_qol14 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol14 = who_qol14_ajustado
  )

# who_qol16
# tira da lista e cria um vetor do who_qol16 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol16,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol16_ajustado <- bind_rows(d, c_d)
who_qol16_ajustado <- who_qol16_ajustado$value
who_qol16_ajustado <- as.integer(who_qol16_ajustado)
who_qol16_ajustado

# Insere o who_qol16 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol16 = who_qol16_ajustado
  )

# who_qol17
# tira da lista e cria um vetor do who_qol17 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol17,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol17_ajustado <- bind_rows(d, c_d)
who_qol17_ajustado <- who_qol17_ajustado$value
who_qol17_ajustado <- as.integer(who_qol17_ajustado)
who_qol17_ajustado

# Insere o who_qol17 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol17 = who_qol17_ajustado
  )

# who_qol18
# tira da lista e cria um vetor do who_qol18 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol18,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol18_ajustado <- bind_rows(d, c_d)
who_qol18_ajustado <- who_qol18_ajustado$value
who_qol18_ajustado <- as.integer(who_qol18_ajustado)
who_qol18_ajustado

# Insere o who_qol18 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol18 = who_qol18_ajustado
  )

# who_qol21
# tira da lista e cria um vetor do who_qol21 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol21,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")


# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol21_ajustado <- bind_rows(d, c_d)
who_qol21_ajustado <- who_qol21_ajustado$value
who_qol21_ajustado <- as.integer(who_qol21_ajustado)
who_qol21_ajustado

# Insere o who_qol21 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol21 = who_qol21_ajustado
  )

# who_qol22
# tira da lista e cria um vetor do who_qol22 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol22,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- rep(NA_character_, 149)

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol22_ajustado <- bind_rows(d, c_d)
who_qol22_ajustado <- who_qol22_ajustado$value
who_qol22_ajustado <- as.integer(who_qol22_ajustado)
who_qol22_ajustado

# Insere o who_qol22 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol22 = who_qol22_ajustado
  )

# who_qol27
# tira da lista e cria um vetor do who_qol27 (porém as linhas erradas foram excluidas)
d <- unlist(df_paper$who_qol27,
            use.names = TRUE)

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- rep(NA_character_, 10)

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
who_qol27_ajustado <- bind_rows(d, c_d)
who_qol27_ajustado <- who_qol27_ajustado$value
who_qol27_ajustado <- as.integer(who_qol27_ajustado)
who_qol27_ajustado

# Insere o who_qol27 no df
df_paper <-
  df_paper |>
  mutate(
    who_qol27 = who_qol27_ajustado
  )


# verifica as colunas do dataset para ver SE AINDA TEM LIST
df_paper |>
  select(where(is.list)) |>
  glimpse()

glimpse(df_paper)

# tabela para analise -------------------------------------------------------------------------
write_rds(x = df_paper,file =  "df_para_CalcularVars.rds")

