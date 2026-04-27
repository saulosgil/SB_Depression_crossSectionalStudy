# 🧠📉 SB_Depression_crossSectionalStudy

Repositório com scripts em **R** para análise da associação entre **comportamento sedentário** e **sintomas depressivos** em um estudo transversal.

O projeto inclui rotinas para:

- 📦 preparação dos dados;
- 🧹 tratamento de dados faltantes;
- 📊 análises de regressão logística;
- 📈 análises de tendência linear;
- 📉 modelagem com **restricted cubic splines (RCS)**;
- 🧾 geração de bases analíticas intermediárias.

O repositório atualmente contém múltiplas pastas de análises, scripts principais em R, arquivos `.rds`, uma planilha `.xlsx`, uma imagem `.jpeg` e um arquivo de projeto do RStudio.

---

## 📖 Sobre o projeto

Este repositório organiza o fluxo analítico de um estudo transversal que investiga a relação entre:

- **tempo sedentário total**;
- **comportamento sedentário mentalmente passivo**;
- **comportamento sedentário mentalmente ativo**;

e **desfechos relacionados à depressão**.

Pelos scripts disponíveis, a variável de depressão é trabalhada tanto como **escore contínuo** quanto como **desfecho categórico**, incluindo uma dicotomização do escore de depressão em `< 20` versus `≥ 20`. As exposições sedentárias também aparecem tanto de forma contínua quanto categorizadas em `< 4 h/day` versus `≥ 4 h/day`.

---

## 🎯 Objetivos analíticos

Com base nos arquivos do repositório, este projeto permite:

- avaliar a associação entre **sedentary behavior (SB)** e depressão;
- separar as análises por **mentally passive sedentary behavior (MPSB)** e **mentally active sedentary behavior (MASB)**;
- testar associações lineares e não lineares;
- ajustar os modelos para variáveis sociodemográficas e clínicas;
- explorar análises adicionais com subgrupos e versões alternativas dos modelos.

---

## 🗂️ Estrutura do repositório

```text
SB_Depression_crossSectionalStudy/
├── Hallgren_analises_AdjustedMvpa_continuos/
├── analises_AdjustedMvpa_categorical/
├── analises_AdjustedMvpa_continuos/
├── analises_AdjustedMvpa_continuos_older54/
├── .gitignore
├── SB_Depression_crossSectionalStudy.Rproj
├── calculando_vars.R
├── dados_acelerometro_rct.xlsx
├── data_prep.R
├── database_17122025.rds
├── df_para_CalcularVars.rds
├── df_para_analise.rds
├── df_para_analise_hallgren.rds
├── linear_trend_masb.R
├── linear_trend_mpsb.R
├── linear_trend_sb.R
├── logistic_reg.R
├── partial_analises_rct.R
├── rcs_analysis.R
├── splines_curves.jpeg
└── tratamento_missing.R
```

O repositório contém quatro diretórios principais de análise, além de scripts voltados à preparação, modelagem e exploração analítica.

---

## 📁 Principais arquivos

### `data_prep.R`
Script de preparação da base de dados. O nome do arquivo indica que ele concentra o fluxo inicial de organização dos dados para análise.

### `tratamento_missing.R`
Script voltado ao tratamento de dados faltantes, etapa importante para a construção da base analítica final.

### `calculando_vars.R`
Script destinado ao cálculo e derivação de variáveis analíticas a partir das bases intermediárias.

### `logistic_reg.R`
Implementa **regressão logística penalizada de Firth**, descrita no próprio script como uma estratégia para reduzir viés, lidar com separação completa ou quase completa e garantir estimativas finitas de odds ratios e IC95%. O script ajusta modelos para SB total, MPSB e MASB, com ajuste para idade, gênero, raça, renda, hipertensão, diabetes tipo 2 e obesidade.

### `rcs_analysis.R`
Implementa modelos com **restricted cubic splines (3 nós)** para examinar associações não lineares entre comportamento sedentário e escore de depressão. O script informa explicitamente o uso de nós nos percentis **10, 50 e 90**, com ajuste para idade, sexo, raça/etnia, renda, hipertensão, diabetes tipo 2, obesidade e MVPA.

### `linear_trend_sb.R`, `linear_trend_mpsb.R`, `linear_trend_masb.R`
Scripts destinados à avaliação de tendência linear para comportamento sedentário total, mentalmente passivo e mentalmente ativo.

### `splines_curves.jpeg`
Arquivo de imagem associado às curvas de spline geradas nas análises.

---

## 🧪 Variáveis analíticas centrais

Pelos scripts públicos, o projeto trabalha com as seguintes variáveis principais:

### Exposições
- `total_sb_hday` → comportamento sedentário total em horas/dia;
- `total_sb_mp_hday` → comportamento sedentário mentalmente passivo em horas/dia;
- `total_sb_ma_hday` → comportamento sedentário mentalmente ativo em horas/dia.

### Desfecho
- `depressao_score` → escore de depressão;
- `dep_cat` → variável categórica derivada do escore de depressão (`< 20` vs `≥ 20`).

### Covariáveis
Os modelos usam ajuste para:
- idade/categoria de idade;
- gênero;
- raça;
- renda;
- hipertensão arterial sistêmica (`has`);
- diabetes tipo 2 (`dm2`);
- obesidade;
- MVPA em algumas análises.

---

## 📈 Estratégia estatística

Com base nos scripts disponíveis, o fluxo analítico inclui:

- regressão logística penalizada de **Firth** para desfechos binários;
- cálculo de **odds ratios** e **intervalos de confiança de 95%**;
- análise de associações não lineares com **restricted cubic splines**;
- análises específicas para **SB total**, **MPSB** e **MASB**;
- categorização de exposições sedentárias em `< 4 h/day` e `≥ 4 h/day`;
- inclusão de covariáveis sociodemográficas e metabólicas/clínicas.

---

## 💻 Tecnologias utilizadas

- **R**
- Projeto em **RStudio** com arquivo `.Rproj`
- Pacotes explicitamente visíveis nos scripts:
  - `tidyverse`
  - `patchwork`
  - `rms`
  - `aod`
  - `readr`
  - `logistf`

---

## ▶️ Como usar

### 1. Clone o repositório

```bash
git clone https://github.com/saulosgil/SB_Depression_crossSectionalStudy.git
```

### 2. Abra o projeto no RStudio

```r
SB_Depression_crossSectionalStudy.Rproj
```

### 3. Execute o fluxo analítico

Uma ordem sugerida é:

```r
source("data_prep.R")
source("tratamento_missing.R")
source("calculando_vars.R")
source("logistic_reg.R")
source("rcs_analysis.R")
```

As análises adicionais podem então ser executadas conforme o objetivo:

```r
source("linear_trend_sb.R")
source("linear_trend_mpsb.R")
source("linear_trend_masb.R")
```

---

## 🔁 Reprodutibilidade

Para manter a reprodutibilidade do projeto:

- ✅ preserve a estrutura atual de pastas e arquivos;
- ✅ mantenha os arquivos `.rds` e `.xlsx` no diretório esperado;
- ✅ execute os scripts a partir da raiz do projeto;
- ✅ use o arquivo `.Rproj` para evitar problemas de caminho relativo.

O repositório contém múltiplos objetos intermediários em `.rds`, o que sugere um fluxo em etapas, com preparação e análise separadas.

---

## 👨‍💻 Autor

**Saulo Gil**  
GitHub: [@saulosgil](https://github.com/saulosgil)

---

## 🌟 Observação final

Este repositório é uma base útil para análises epidemiológicas sobre **comportamento sedentário e depressão**, especialmente por combinar modelagem tradicional, regressão penalizada e abordagens flexíveis de não linearidade com splines. A estrutura sugere um projeto voltado à transparência analítica e à organização de diferentes estratégias de modelagem dentro de um mesmo estudo transversal.
