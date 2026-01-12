# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(rms)   # Para splines
library(aod)

# Lendo a base para ajustes -------------------------------------------------------------------
df <- readr::read_rds("df_para_analise.rds")

glimpse(df)

# Descriptive data and other metrics ----------------------------------------------------------
df |>
  summarise(
    media = mean(mvpa_minday, na.rm = TRUE),
    DP = sd(     mvpa_minday, na.rm = TRUE),
  )

df |>
  count(genero) |>
  mutate(percentual = round(n / sum(n) * 100, 1))

# criar coluna idade categorica para os modelos RCS -----------------------------------------------
df <-
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
                       raca == "Ind\xa1gena" ~ 3),
      renda = case_when(renda == "At\x82 1 sal\xa0rio m\xa1nimo" ~ 0,
                        renda == "1 ? 2 sal\xa0rios m\xa1nimos" ~ 1,
                        renda == "2 ? 3 sal\xa0rios m\xa1nimos" ~ 2,
                        renda == "4 ? 5 sal\xa0rios m\xa1nimos" ~ 3,
                        renda == "Mais de 5 sal\xa0rios m\xa1nimos" ~ 4,
                        renda == "N\xc6o relatou" ~ 5)
    )

df <-
  df |>
  select(-estado_civil)

glimpse(df)

# Restricted cubic splines (RCS) models -------------------------------------------------------
# O que são restricted cubic splines (RCS)

# Uma spline cúbica restrita é uma forma flexível de modelar relações não-lineares entre uma variável
# contínua e o desfecho:

#   Em vez de assumir uma linha reta ou uma parábola fixa, a spline permite que a curva mude de
# inclinação em diferentes pontos.

# "Cúbica" → cada segmento entre dois nós é uma função polinomial de grau 3.

# "Restrita" → a curva é linear fora do intervalo dos nós extremos, evitando extrapolações exageradas.

# Por padrão, o RMS coloca os nós em quantis percentuais da variável:

#   Para 3 nós, os percentis típicos são: 10%, 50%, 90%
        # - Primeiro nó: 10º percentil de total_sb_mp_hday;
        # - Segundo nó: 50º percentil (mediana);
        # - Terceiro nó: 90º percentil.
# Esses nós definem onde a curva pode mudar de inclinação.

### Possivel descrição para o artigo
# We examined the non-linear association between mentally passive sedentary behavior (MASB) and
# depression score using restricted cubic splines with 3 knots (placed at the 10th, 50th, and 90th percentiles).
# The model was adjusted for age category, sex, race/ethnicity, income, hypertension, diabetes, and obesity.

dd <- datadist(df)   # necessário para rms
options(datadist = "dd")

# SB ------------------------------------------------------------------------------------------
model_rcs <- ols(
  depressao_score ~ rcs(total_sb_hday, 3) +  # spline 3 nós para não-linearidade
    idade_cat +                  # categoria de idade
    genero +                     # sexo
    raca +                       # raça/etnia
    renda +                      # renda
    has +                        # hipertensão arterial sistêmica
    dm2 +                        # diabetes tipo 2
    obesidade,                   # obesidade
  data = df
)

model_rcs
anova(model_rcs)

# Gerar intervalo de valores da variável de interesse
## funcao para pegar categoria mais frequente
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Calcula categoria mais frequente
idade_cat_mode <- get_mode(df$idade_cat)
genero_mode   <- get_mode(df$genero)
raca_mode     <- get_mode(df$raca)
renda_mode    <- get_mode(df$renda)
has_mode    <- get_mode(df$has)
dm2_mode    <- get_mode(df$dm2)
obesidade_mode    <- get_mode(df$obesidade)


new_data <- expand.grid(
  total_sb_hday = seq(min(df$total_sb_hday, na.rm = TRUE),
                      max(df$total_sb_hday, na.rm = TRUE),
                      length.out = 100),
  idade_cat = idade_cat_mode,  # moda de todas as categoricas
  genero = genero_mode,
  raca = raca_mode,
  renda = renda_mode,
  has = has_mode,
  dm2 = dm2_mode,
  obesidade = obesidade_mode
)

glimpse(new_data)

# Previsão do modelo com IC
pred <- Predict(model_rcs,
                total_sb_hday = new_data$total_sb_hday,
                fun = identity,
                conf.int = 0.95)

pred_df <- as.data.frame(pred)

glimpse(pred_df)

# Versão refinada com ggplot2
SB <- ggplot() +

  # Linha ajustada
  geom_line(data = pred_df, aes(x = total_sb_hday, y = yhat),
            color = "black", size = 1.1) +

  # Faixa de IC 95%
  geom_ribbon(data = pred_df, aes(x = total_sb_hday, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.3) +

  # Eixos e labels
  labs(
    x = "Sedentary Behavior (h/day)",
    y = "Beck Depression Invetory Score (a.u.)"
  ) +
  scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15, 3)) +

  # Tema minimalista tipo JAMA
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

SB

# Mentalmente passivo SB ----------------------------------------------------------------------
model_rcs <- ols(
    depressao_score ~ rcs(total_sb_mp_hday, 3) +  # spline 3 nós para não-linearidade
      idade_cat +                  # categoria de idade
      genero +                     # sexo
      raca +                       # raça/etnia
      renda +                      # renda
      has +                        # hipertensão arterial sistêmica
      dm2 +                        # diabetes tipo 2
      obesidade,                   # obesidade
    data = df
  )

model_rcs
anova(model_rcs)

# Gerar intervalo de valores da variável de interesse
new_data <- expand.grid(
  total_sb_mp_hday = seq(min(df$total_sb_mp_hday, na.rm = TRUE),
                         max(df$total_sb_mp_hday, na.rm = TRUE),
                         length.out = 100),
  idade_cat = idade_cat_mode,  # moda de todas as categoricas
  genero = genero_mode,
  raca = raca_mode,
  renda = renda_mode,
  has = has_mode,
  dm2 = dm2_mode,
  obesidade = obesidade_mode
)

glimpse(new_data)

# Previsão do modelo com IC
pred <- Predict(model_rcs,
                total_sb_mp_hday = new_data$total_sb_mp_hday,
                fun = identity,
                conf.int = 0.95)

pred_df <- as.data.frame(pred)

glimpse(pred_df)

# Versão refinada com ggplot2
MPSB <- ggplot() +

  # Linha ajustada
  geom_line(data = pred_df, aes(x = total_sb_mp_hday, y = yhat),
            color = "black", size = 1.1) +

  # Faixa de IC 95%
  geom_ribbon(data = pred_df, aes(x = total_sb_mp_hday, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.3) +

  # Eixos e labels
  labs(
    x = "Mentally Passive Sedentary Behavior (h/day)",
    y = "Beck Depression Invetory Score (a.u.)"
  ) +
  scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15, 3)) +

  # Tema minimalista tipo JAMA
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

MPSB

# Mentalmente ativo SB ------------------------------------------------------------------------
model_rcs <- ols(
  depressao_score ~ rcs(total_sb_ma_hday, 3) +  # spline 3 nós para não-linearidade
    idade_cat +                  # categoria de idade
    genero +                     # sexo
    raca +                       # raça/etnia
    renda +                      # renda
    has +                        # hipertensão arterial sistêmica
    dm2 +                        # diabetes tipo 2
    obesidade,                   # obesidade
  data = df
)

model_rcs
anova(model_rcs)

# Gerar intervalo de valores da variável de interesse
new_data <- expand.grid(
  total_sb_ma_hday = seq(min(df$total_sb_ma_hday, na.rm = TRUE),
                         max(df$total_sb_ma_hday, na.rm = TRUE),
                         length.out = 100),
  idade_cat = idade_cat_mode,  # moda de todas as categoricas
  genero = genero_mode,
  raca = raca_mode,
  renda = renda_mode,
  has = has_mode,
  dm2 = dm2_mode,
  obesidade = obesidade_mode
)

glimpse(new_data)

# Previsão do modelo com IC
pred <- Predict(model_rcs,
                total_sb_ma_hday = new_data$total_sb_ma_hday,
                fun = identity,
                conf.int = 0.95)

pred_df <- as.data.frame(pred)

glimpse(pred_df)

# Versão refinada com ggplot2
MASB <- ggplot() +

  # Linha ajustada
  geom_line(data = pred_df, aes(x = total_sb_ma_hday, y = yhat),
            color = "black", size = 1.1) +

  # Faixa de IC 95%
  geom_ribbon(data = pred_df, aes(x = total_sb_ma_hday, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.3) +

  # Eixos e labels
  labs(
    x = "Mentally Active Sedentary Behavior (h/day)",
    y = "Beck Depression Invetory Score (a.u.)"
  ) +
  scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15, 3)) +

  # Tema minimalista tipo JAMA
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", size = 0.6),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank()
  )

MASB

# layout
SB / MPSB / MASB

