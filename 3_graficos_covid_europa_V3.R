# ==============================================================================
# ARQUIVO: 3_graficos_covid_europa.R
# Pré-requisito: rodar 1_etl_covid_europa.R e 2_analise_covid_europa.R antes.
#
# Gráficos para os 4 indicadores (excesso de mortalidade, expectativa de
# vida, crescimento do PIB, taxa de desemprego), com os 27 países da UE
# (CENSO). Para cada indicador: 1) evolução temporal com o período COVID
# sombreado, 2) comparação pareada pré-COVID vs. COVID (boxplot + linhas
# por país). Mais um gráfico de comparação regional (ANOVA) para excesso
# de mortalidade e PIB.
# ==============================================================================
#install.packages(c("dplyr", "tidyr", "ggplot2", "scales"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ==============================================================================
# 0. CARREGAR DADOS
# ==============================================================================
paises_regioes  <- read.csv("dados/paises_ue27_regioes.csv", colClasses = c(geo = "character"))
expectativa_vida <- read.csv("dados/expectativa_vida.csv",   colClasses = c(geo = "character"))
crescimento_pib  <- read.csv("dados/crescimento_pib.csv",    colClasses = c(geo = "character"))
desemprego        <- read.csv("dados/desemprego.csv",         colClasses = c(geo = "character"))
excesso_mortalidade <- read.csv("saida_analise/excesso_mortalidade.csv", colClasses = c(geo = "character"))

classificar_periodo <- function(ano) {
  case_when(
    ano >= 2014 & ano <= 2019 ~ "2014-2019 (pré-COVID)",
    ano >= 2020 & ano <= 2021 ~ "2020-2021 (COVID)",
    TRUE ~ NA_character_
  )
}
NIVEIS_PERIODO <- c("2014-2019 (pré-COVID)", "2020-2021 (COVID)")

tema_padrao <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

sombra_covid <- geom_rect(
  aes(xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf),
  inherit.aes = FALSE, fill = "grey80", alpha = 0.4
)

# ==============================================================================
# 1. FUNÇÃO: EVOLUÇÃO TEMPORAL (27 países + média aritmética entre países)
# CORRIGIDO: a linha de referência é rotulada como "média aritmética entre
# países" (cada um dos 27 pesando igual), não "média UE-27" -- esse nome
# sugeriria o agregado oficial (que seria ponderado por população/PIB, e
# não é o que este cálculo faz).
# ==============================================================================
grafico_evolucao <- function(dados, variavel, titulo, y_label) {
  dados <- dados %>% inner_join(paises_regioes, by = "geo")
  media_simples <- dados %>% group_by(year) %>%
    summarise(media = mean(.data[[variavel]], na.rm = TRUE), .groups = "drop")
  
  ggplot() +
    sombra_covid +
    geom_line(data = dados, aes(x = year, y = .data[[variavel]], group = geo),
              alpha = 0.25, color = "grey40") +
    geom_line(data = media_simples, aes(x = year, y = media),
              linewidth = 1.4, color = "steelblue") +
    labs(title = titulo,
         subtitle = "Linhas finas = países | linha azul = média aritmética entre países | faixa cinza = 2020-2021",
         x = NULL, y = y_label) +
    tema_padrao
}

g_evol_mortalidade  <- grafico_evolucao(excesso_mortalidade, "excesso_mortalidade_anual_pct",
                                        "Excesso de mortalidade (cálculo anual próprio)", "% acima da baseline 2015-2019")
g_evol_expectativa  <- grafico_evolucao(expectativa_vida, "expectativa_vida",
                                        "Expectativa de vida ao nascer", "Anos")
g_evol_pib           <- grafico_evolucao(crescimento_pib, "crescimento_pib_pct",
                                         "Crescimento do PIB real", "% a.a.")
g_evol_desemprego    <- grafico_evolucao(desemprego, "taxa_desemprego",
                                         "Taxa de desemprego", "% da força de trabalho")

print(g_evol_mortalidade); print(g_evol_expectativa); print(g_evol_pib); print(g_evol_desemprego)

# ==============================================================================
# 2. FUNÇÃO: COMPARAÇÃO PAREADA PRÉ-COVID vs. COVID
# ==============================================================================
grafico_comparacao_pareada <- function(dados, variavel, titulo, y_label) {
  medias_pais <- dados %>%
    mutate(periodo = classificar_periodo(year)) %>%
    filter(!is.na(periodo)) %>%
    group_by(geo, periodo) %>%
    summarise(media = mean(.data[[variavel]], na.rm = TRUE), .groups = "drop") %>%
    mutate(periodo = factor(periodo, levels = NIVEIS_PERIODO))
  
  ggplot(medias_pais, aes(x = periodo, y = media)) +
    geom_line(aes(group = geo), alpha = 0.3, color = "grey50") +
    geom_point(alpha = 0.5, color = "grey30") +
    geom_boxplot(width = 0.3, alpha = 0.5, outlier.shape = NA, fill = "steelblue") +
    labs(title = titulo, subtitle = "Cada linha = 1 país (média do período)",
         x = NULL, y = y_label) +
    tema_padrao
}

g_comp_mortalidade <- grafico_comparacao_pareada(excesso_mortalidade, "excesso_mortalidade_anual_pct",
                                                 "Excesso de mortalidade: pré-COVID vs. COVID", "%")
g_comp_expectativa <- grafico_comparacao_pareada(expectativa_vida, "expectativa_vida",
                                                 "Expectativa de vida: pré-COVID vs. COVID", "Anos")
g_comp_pib          <- grafico_comparacao_pareada(crescimento_pib, "crescimento_pib_pct",
                                                  "Crescimento do PIB: pré-COVID vs. COVID", "%")
g_comp_desemprego   <- grafico_comparacao_pareada(desemprego, "taxa_desemprego",
                                                  "Taxa de desemprego: pré-COVID vs. COVID", "%")

print(g_comp_mortalidade); print(g_comp_expectativa); print(g_comp_pib); print(g_comp_desemprego)

# ==============================================================================
# 3. COMPARAÇÃO REGIONAL DURANTE A COVID (acompanha a ANOVA)
# CORRIGIDO: agrega 2020-2021 por país (1 ponto por país, n = 27) em vez de
# plotar os pontos ano-país (n = 54) como se fossem observações
# independentes -- consistente com a ANOVA corrigida no script 2.
# ==============================================================================
grafico_regional_covid <- function(dados, variavel, titulo, y_label) {
  dados_covid <- dados %>%
    inner_join(paises_regioes, by = "geo") %>%
    filter(year %in% c(2020, 2021)) %>%
    group_by(geo, regiao_estudo) %>%
    summarise(media = mean(.data[[variavel]], na.rm = TRUE), .groups = "drop")
  
  ggplot(dados_covid, aes(x = regiao_estudo, y = media, fill = regiao_estudo)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
    labs(title = titulo, subtitle = "2020-2021, média por país (1 ponto = 1 país)", x = NULL, y = y_label) +
    tema_padrao + theme(legend.position = "none")
}

g_regiao_mortalidade <- grafico_regional_covid(excesso_mortalidade, "excesso_mortalidade_anual_pct",
                                               "Excesso de mortalidade por região", "%")
g_regiao_pib          <- grafico_regional_covid(crescimento_pib, "crescimento_pib_pct",
                                                "Crescimento do PIB por região", "%")

print(g_regiao_mortalidade); print(g_regiao_pib)

cat("\n===== GRÁFICOS FINALIZADOS =====\n")