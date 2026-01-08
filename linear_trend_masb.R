# Linear regression models - Linear trend -----------------------------------------------------

# Because depression symptom scores were right-skewed and included zeros, the outcome was transformed
# using the natural logarithm with a constant added [ln(depressao_score + 1)]. Back-transformed estimates
# are presented as geometric means (and 95% confidence intervals). Depression symptoms were compared
# across quartile (Q1–Q4) of sedentary behavior using linear regression model. A test for linear trend
# was performed by modeling quartile as an ordinal continuous variable (coded 1–4).
# Two models were fitted: (1) crude and (2) adjusted for age (<65 vs ≥65 years), sex (male vs female),
# race/ethnicity (white, black, and pardo), family income (≤R$2,604 vs >R$2,604), and
# pre-existing conditions (hypertension [yes/no], diabetes [yes/no], and obesity [BMI <30 vs ≥30]).

# Pacotes
library(dplyr)
library(broom)
library(emmeans)
library(ggplot2)

# Lendo a base para ajustes -------------------------------------------------------------------
df <- readr::read_rds("df_para_analise.rds")

glimpse(df)

# Preparação dos dados ------------------------------------------------------------------------
df2 <-
  df |>
  mutate(
    # Desfecho com zeros: ln(y+1)
    dep_log = log(depressao_score + 1),

    # Quintis de SB (1 a 5)
    sb_q_num = ntile(total_sb_ma_hday, 4),
    sb_q = factor(sb_q_num, levels = 1:4, labels = paste0("Q", 1:4)),

    # Covariáveis (ajuste os nomes conforme seu banco)
    idade_cat = if_else(idade < 65, 0, 1),
  )

DataExplorer::plot_missing(df2)

# Teste de tendência linear (quintil 1–5 como contínuo) ---------------------------------------
m_crude_trend <- lm(dep_log ~ sb_q_num, data = df2)

m_adj_trend <- lm(
  dep_log ~ sb_q_num + idade_cat + genero + raca + renda +
    has + dm2 + obesidade,
  data = df2
)

summary(m_crude_trend)
summary(m_adj_trend)

p_trend_crude <- tidy(m_crude_trend) |> filter(term == "sb_q_num") |> pull(p.value)
p_trend_adj   <- tidy(m_adj_trend)   |> filter(term == "sb_q_num") |> pull(p.value)

p_trend_crude
p_trend_adj

# Médias geométricas e IC95% por quintil (back-transform) -------------------------------------
# Modelos por quintil (categoria) - necessários para médias por Q1–Q5
m_crude_q <- lm(dep_log ~ sb_q, data = df2)

m_adj_q <- lm(
  dep_log ~ sb_q + idade_cat + genero + raca + renda + has + dm2 + obesidade,
  data = df2
)


fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.0001) "<0.0001" else sprintf("%.4f", p)
}

# N efetivo usado no modelo ajustado (considera missing nas covariáveis)
n_by_q_adj <-
  model.frame(m_adj_q) |>
  as_tibble() |>
  count(sb_q, name = "N_adj") |>
  rename(Quintile = sb_q)

# GM + IC95% (back-transform) - dep_log = ln(score + 1) => GM = exp(mu)-1
gm_table <- function(model, label) {
  em <- emmeans(model, specs = "sb_q") |> as.data.frame()

  em  |>
    transmute(
      Quintile = sb_q,
      gm = exp(emmean) - 1,
      lo = exp(lower.CL) - 1,
      hi = exp(upper.CL) - 1
    ) |>
    mutate(
      "{label}" := sprintf("%.2f (%.2f, %.2f)", gm, lo, hi)
    ) |>
    select(Quintile, all_of(label))
}

tab_gm_crude <- gm_table(m_crude_q, "Crude GM (95% CI)")
tab_gm_adj   <- gm_table(m_adj_q,   "Adjusted GM (95% CI)")

# 6) Tabela final com N + GM + p-trend
tab_final <-
  tab_gm_crude |>
  left_join(tab_gm_adj, by = "Quintile") |>
  left_join(n_by_q_adj,   by = "Quintile") |>
  transmute(
    Quintile,
    N = N_adj,  # <-- troque para N_all ou N_crude se preferir
    `Crude GM (95% CI)`,
    `Adjusted GM (95% CI)`,
    `p-trend (crude)`    = if_else(Quintile == "Q4", fmt_p(p_trend_crude), ""),
    `p-trend (adjusted)` = if_else(Quintile == "Q4", fmt_p(p_trend_adj), "")
  )

tab_final
