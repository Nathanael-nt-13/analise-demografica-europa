# ==============================================================================
# ARQUIVO: 1_etl_covid_europa.R
# Extrai, para os 27 países da UE (CENSO — sem amostragem), os 4 indicadores
# usados para medir o impacto da COVID-19:
#   1. Óbitos totais anuais       -> base para excesso de mortalidade
#   2. Expectativa de vida ao nascer
#   3. PIB real (variação anual)  -> crescimento do PIB
#   4. Taxa de desemprego anual
# Período: 2014-2022 (cobre a linha de base pré-COVID e o biênio de crise,
# com uma margem para 2022 caso queira comentar a recuperação).
# ==============================================================================
#install.packages(c("eurostat", "dplyr", "countrycode", "tidyr"))

library(eurostat)
library(dplyr)
library(countrycode)
library(tidyr)

clean_eurostat_cache()

ANO_INICIO <- 2014
ANO_FIM    <- 2022

# ==============================================================================
# 1. OS 27 PAÍSES DA UE + CLASSIFICAÇÃO REGIONAL
# Mantida apenas como variável de agrupamento p/ ANOVA e qui-quadrado —
# NÃO é mais usada para nenhum cálculo de amostragem ou peso.
# ==============================================================================
paises_ue27 <- tribble(
  ~geo, ~regiao_estudo,
  "DK","Norte", "EE","Norte", "FI","Norte", "IE","Norte",
  "LV","Norte", "LT","Norte", "SE","Norte",
  "AT","Oeste", "BE","Oeste", "FR","Oeste", "DE","Oeste",
  "LU","Oeste", "NL","Oeste",
  "HR","Sul", "CY","Sul", "GR","Sul", "IT","Sul",
  "MT","Sul", "PT","Sul", "SI","Sul", "ES","Sul",
  "BG","Leste", "CZ","Leste", "HU","Leste", "PL","Leste",
  "RO","Leste", "SK","Leste"
) %>%
  mutate(nome_pais = countrycode(geo, "iso2c", "country.name", warn = FALSE))

stopifnot(nrow(paises_ue27) == 27)

fix_geo <- function(df) df %>% mutate(geo = case_when(geo == "UK" ~ "GB", geo == "EL" ~ "GR", TRUE ~ geo))

# ==============================================================================
# 2. ÓBITOS TOTAIS ANUAIS (base para o excesso de mortalidade)
# ==============================================================================
obitos <- get_eurostat("demo_gind", time_format = "num") %>%
  filter(indic_de == "DEATH", nchar(geo) == 2) %>%
  rename(year = TIME_PERIOD, obitos = values) %>%
  select(geo, year, obitos) %>%
  fix_geo() %>%
  filter(geo %in% paises_ue27$geo, year >= ANO_INICIO - 5, year <= ANO_FIM)
# ANO_INICIO - 5 garante histórico suficiente p/ a baseline 2015-2019

# ==============================================================================
# 3. EXPECTATIVA DE VIDA AO NASCER
# ==============================================================================
expectativa_vida <- get_eurostat("demo_mlexpec", time_format = "num") %>%
  filter(age == "Y1", sex == "T", nchar(geo) == 2) %>%
  rename(year = TIME_PERIOD, expectativa_vida = values) %>%
  select(geo, year, expectativa_vida) %>%
  fix_geo() %>%
  filter(geo %in% paises_ue27$geo, year >= ANO_INICIO, year <= ANO_FIM)

# ==============================================================================
# 4. PIB REAL (para calcular crescimento anual)
# ==============================================================================
pib_real <- get_eurostat("nama_10_gdp", time_format = "num") %>%
  filter(na_item == "B1GQ", unit == "CLV10_MEUR", nchar(geo) == 2) %>%
  rename(year = TIME_PERIOD, pib_real = values) %>%
  select(geo, year, pib_real) %>%
  fix_geo() %>%
  filter(geo %in% paises_ue27$geo, year >= ANO_INICIO - 1, year <= ANO_FIM)

crescimento_pib <- pib_real %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(crescimento_pib_pct = 100 * (pib_real - lag(pib_real)) / lag(pib_real)) %>%
  ungroup() %>%
  filter(!is.na(crescimento_pib_pct), year >= ANO_INICIO) %>%
  select(geo, year, crescimento_pib_pct)

# ==============================================================================
# 5. TAXA DE DESEMPREGO ANUAL
# ==============================================================================
desemprego <- get_eurostat("une_rt_a", time_format = "num") %>%
  filter(age == "Y15-74", sex == "T", unit == "PC_ACT", nchar(geo) == 2) %>%
  rename(year = TIME_PERIOD, taxa_desemprego = values) %>%
  select(geo, year, taxa_desemprego) %>%
  fix_geo() %>%
  filter(geo %in% paises_ue27$geo, year >= ANO_INICIO, year <= ANO_FIM)

# ==============================================================================
# 6. EXPORTAÇÃO
# ==============================================================================
dir.create("dados", showWarnings = FALSE)
write.csv(paises_ue27,       "dados/paises_ue27_regioes.csv", row.names = FALSE)
write.csv(obitos,            "dados/obitos.csv",              row.names = FALSE)
write.csv(expectativa_vida,  "dados/expectativa_vida.csv",    row.names = FALSE)
write.csv(crescimento_pib,   "dados/crescimento_pib.csv",     row.names = FALSE)
write.csv(desemprego,        "dados/desemprego.csv",          row.names = FALSE)

cat("\nETL FINALIZADO — 27 países, 4 indicadores (óbitos, expectativa de vida,\n")
cat("crescimento do PIB, taxa de desemprego), 2014-2022.\n")