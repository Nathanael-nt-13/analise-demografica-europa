# ==============================================================================
# ARQUIVO: 2_analise_covid_europa.R
# Pré-requisito: rodar 1_etl_covid_europa.R antes.
#
# Compara o período pré-COVID (2014-2019) com o período de crise (2020-2021)
# para os 27 países da UE (CENSO — nenhum peso amostral, nenhuma amostragem).
#
# CORREÇÕES em relação à versão anterior (revisão da professora/revisão própria):
#  1. ANOVA e estimação intervalar agora agregam 2020-2021 por país ANTES do
#     teste -- evita pseudorreplicação (27 países x 2 anos NÃO são 54
#     observações independentes; são 27 unidades medidas duas vezes).
#  2. Indicador renomeado para 'excesso_mortalidade_anual_pct' -- deixa claro
#     que NÃO é a metodologia oficial do Eurostat (que é semanal, baseline
#     2016-2019), e sim um cálculo próprio com dados anuais, baseline 2015-2019.
#  3. Qui-quadrado trocado para Fisher (simulado) -- tabela 4x2 com célula
#     esperada < 5 (região Norte tem só 7 países) viola a premissa do
#     qui-quadrado assintótico.
#  4. Teste de aderência (50/50) REMOVIDO -- era quase tautológico, porque
#     "muito afetado" era definido pela mediana da própria amostra (por
#     construção, ~50% fica acima). Substituído por uma correlação entre
#     excesso de mortalidade e crescimento do PIB durante a COVID -- pergunta
#     nova e não-circular: países mais afetados na saúde sofreram mais
#     economicamente?
#  5. Nota de interpretação sobre desemprego (programas de suspensão de
#     contrato/layoff distorcem a leitura direta da taxa).
# ==============================================================================
#install.packages(c("dplyr", "tidyr", "purrr"))

library(dplyr)
library(tidyr)
library(purrr)

ALPHA <- 0.05

# ==============================================================================
# 1. CARREGAR DADOS
# ==============================================================================
paises_regioes   <- read.csv("dados/paises_ue27_regioes.csv",  colClasses = c(geo = "character"))
obitos            <- read.csv("dados/obitos.csv",               colClasses = c(geo = "character"))
expectativa_vida  <- read.csv("dados/expectativa_vida.csv",     colClasses = c(geo = "character"))
crescimento_pib   <- read.csv("dados/crescimento_pib.csv",      colClasses = c(geo = "character"))
desemprego         <- read.csv("dados/desemprego.csv",           colClasses = c(geo = "character"))

# ==============================================================================
# 2. EXCESSO DE MORTALIDADE (cálculo próprio, anual -- NÃO é o indicador
#    oficial do Eurostat, que é semanal com baseline 2016-2019)
# Baseline = média de óbitos de cada país em 2015-2019 (pré-pandemia).
# Excesso (%) = 100 * (óbitos do ano - baseline) / baseline
# NOTA: por construção, os anos usados para montar a baseline (2015-2019)
# terão excesso_mortalidade_anual_pct oscilando perto de 0 -- isso é
# esperado, não é um erro.
# ==============================================================================
baseline_obitos <- obitos %>%
  filter(year >= 2014, year <= 2019) %>%
  group_by(geo) %>%
  summarise(baseline = mean(obitos, na.rm = TRUE), .groups = "drop")

excesso_mortalidade <- obitos %>%
  inner_join(baseline_obitos, by = "geo") %>%
  mutate(excesso_mortalidade_anual_pct = 100 * (obitos - baseline) / baseline) %>%
  select(geo, year, excesso_mortalidade_anual_pct)

# ==============================================================================
# 3. CLASSIFICAÇÃO DE PERÍODO (só 2 períodos — pré-COVID e COVID)
# ==============================================================================
classificar_periodo <- function(ano) {
  case_when(
    ano >= 2014 & ano <= 2019 ~ "2014-2019 (pré-COVID)",
    ano >= 2020 & ano <= 2021 ~ "2020-2021 (COVID)",
    TRUE ~ NA_character_
  )
}
NIVEIS_PERIODO <- c("2014-2019 (pré-COVID)", "2020-2021 (COVID)")

preparar <- function(dados, coluna_valor) {
  dados %>%
    mutate(periodo = factor(classificar_periodo(year), levels = NIVEIS_PERIODO)) %>%
    filter(!is.na(periodo), !is.na(.data[[coluna_valor]])) %>%
    inner_join(paises_regioes, by = "geo")
}

# Agrega observações ano a ano numa única observação por país (média do
# período). Evita pseudorreplicação: 2020 e 2021 do mesmo país não são
# unidades independentes, são a mesma unidade medida duas vezes.
agregar_por_pais <- function(dados, coluna_valor, periodo_alvo) {
  dados %>%
    filter(periodo == periodo_alvo) %>%
    group_by(geo, regiao_estudo) %>%
    summarise(media = mean(.data[[coluna_valor]], na.rm = TRUE), .groups = "drop")
}

# ==============================================================================
# FUNÇÕES AUXILIARES DE INFERÊNCIA (clássicas — sem pesos)
# ==============================================================================

# --- Teste t PAREADO: médias pré-COVID vs. COVID, país a país (n = 27) ---
rodar_teste_t_pareado <- function(dados, coluna_valor, nome_teste) {
  medias_pais <- dados %>%
    group_by(geo, periodo) %>%
    summarise(media = mean(.data[[coluna_valor]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = periodo, values_from = media) %>%
    filter(!is.na(.data[[NIVEIS_PERIODO[1]]]), !is.na(.data[[NIVEIS_PERIODO[2]]]))
  
  teste <- t.test(
    medias_pais[[NIVEIS_PERIODO[2]]], medias_pais[[NIVEIS_PERIODO[1]]],
    paired = TRUE
  )
  cat(sprintf("\n--- %s (teste t pareado, n = %d países) ---\n", nome_teste, nrow(medias_pais)))
  print(teste)
  cat(sprintf("IC %.0f%% da diferença (COVID - pré-COVID): [%.3f, %.3f] | Significativo a alpha=%.3f? %s\n",
              100 * (1 - ALPHA), teste$conf.int[1], teste$conf.int[2],
              ALPHA, ifelse(teste$p.value < ALPHA, "Sim", "Não")))
  teste
}

# --- ANOVA: indicador durante a COVID, comparado entre as 4 regiões ---
# CORRIGIDO: agrega 2020-2021 por país antes (n = 27), não usa mais os 54
# pontos ano-país como se fossem observações independentes.
rodar_anova_regiao <- function(dados, coluna_valor, nome_teste) {
  agregado <- agregar_por_pais(dados, coluna_valor, "2020-2021 (COVID)")
  
  modelo <- aov(media ~ regiao_estudo, data = agregado)
  resumo <- summary(modelo)
  p_valor <- resumo[[1]][["Pr(>F)"]][1]
  
  cat(sprintf("\n--- %s (ANOVA, 4 regiões, n = %d países) ---\n", nome_teste, nrow(agregado)))
  print(resumo)
  cat(sprintf("Significativo a alpha=%.3f? %s\n", ALPHA, ifelse(p_valor < ALPHA, "Sim", "Não")))
  
  medias_regiao <- agregado %>%
    group_by(regiao_estudo) %>%
    summarise(media_regiao = mean(media, na.rm = TRUE),
              dp = sd(media, na.rm = TRUE), n_paises = n(), .groups = "drop")
  cat("\nMédias por região (2020-2021, 1 valor por país):\n")
  print(medias_regiao)
  
  list(modelo = modelo, medias_regiao = medias_regiao, dados_agregados = agregado)
}

# --- Qui-quadrado (Fisher, simulado): região x "muito afetado" (mediana do
#     excesso de mortalidade durante a COVID, 2020-2021) ---
# CORRIGIDO: chisq.test() assintótico exigia contagem esperada >= 5 em cada
# célula, o que não se sustenta com 4 regiões x 27 países (região Norte tem
# só 7 países). Fisher exato (com simulação, já que a tabela é 4x2) não tem
# essa exigência.
rodar_teste_associacao <- function(nome_teste) {
  afetado <- excesso_mortalidade %>%
    filter(year %in% c(2020, 2021)) %>%
    group_by(geo) %>%
    summarise(excesso_medio = mean(excesso_mortalidade_anual_pct, na.rm = TRUE), .groups = "drop") %>%
    inner_join(paises_regioes, by = "geo") %>%
    mutate(muito_afetado = excesso_medio > median(excesso_medio, na.rm = TRUE))
  
  tabela <- table(afetado$regiao_estudo, afetado$muito_afetado)
  teste <- fisher.test(tabela, simulate.p.value = TRUE, B = 10000)
  
  cat(sprintf("\n--- %s (teste exato de Fisher, simulado) ---\n", nome_teste))
  print(tabela)
  print(teste)
  cat(sprintf("Significativo a alpha=%.3f? %s\n", ALPHA, ifelse(teste$p.value < ALPHA, "Sim", "Não")))
  list(teste = teste, dados = afetado)
}

# --- Correlação: excesso de mortalidade x crescimento do PIB, durante a
#     COVID, ao nível de país (n = 27) ---
# SUBSTITUI o antigo teste de aderência 50/50, que era pouco informativo
# (tautológico, já que "muito afetado" vinha da mediana da própria amostra).
# Pergunta nova: países mais atingidos na saúde sofreram mais economicamente?
rodar_correlacao_saude_economia <- function(dados_mortalidade, dados_pib, nome_teste) {
  mortalidade_pais <- agregar_por_pais(dados_mortalidade, "excesso_mortalidade_anual_pct", "2020-2021 (COVID)") %>%
    rename(excesso_mortalidade = media) %>% select(geo, excesso_mortalidade)
  
  pib_pais <- agregar_por_pais(dados_pib, "crescimento_pib_pct", "2020-2021 (COVID)") %>%
    rename(crescimento_pib = media) %>% select(geo, crescimento_pib)
  
  dados_correlacao <- inner_join(mortalidade_pais, pib_pais, by = "geo")
  
  teste_pearson  <- cor.test(dados_correlacao$excesso_mortalidade, dados_correlacao$crescimento_pib,
                             method = "pearson")
  teste_spearman <- cor.test(dados_correlacao$excesso_mortalidade, dados_correlacao$crescimento_pib,
                             method = "spearman")
  
  cat(sprintf("\n--- %s (n = %d países) ---\n", nome_teste, nrow(dados_correlacao)))
  cat(sprintf("Pearson:  r = %.3f | IC 95%% [%.3f, %.3f] | p = %.4f\n",
              teste_pearson$estimate, teste_pearson$conf.int[1], teste_pearson$conf.int[2], teste_pearson$p.value))
  cat(sprintf("Spearman: rho = %.3f | p = %.4f (mais robusto a outliers/não-linearidade)\n",
              teste_spearman$estimate, teste_spearman$p.value))
  
  list(pearson = teste_pearson, spearman = teste_spearman, dados = dados_correlacao)
}

# --- Estimação pontual/intervalar: média + IC de um indicador num período ---
# CORRIGIDO: agrega por país antes do t.test (n = 27), não usa mais os
# pontos ano-país (n = 54) como observações independentes -- o IC estava
# artificialmente estreito.
estimar_media_intervalo <- function(dados, coluna_valor, periodo_alvo, nome_estimativa) {
  agregado <- agregar_por_pais(dados, coluna_valor, periodo_alvo)
  teste <- t.test(agregado$media)
  cat(sprintf("\n--- Estimação pontual/intervalar: %s (%s, n = %d países) ---\n",
              nome_estimativa, periodo_alvo, nrow(agregado)))
  cat(sprintf("Média estimada: %.3f | IC 95%%: [%.3f, %.3f]\n",
              teste$estimate, teste$conf.int[1], teste$conf.int[2]))
  teste
}

# ==============================================================================
# 4. RODAR OS TESTES PARA OS 4 INDICADORES
# ==============================================================================

dados_mortalidade  <- preparar(excesso_mortalidade, "excesso_mortalidade_anual_pct")
dados_expectativa  <- preparar(expectativa_vida,     "expectativa_vida")
dados_pib          <- preparar(crescimento_pib,       "crescimento_pib_pct")
dados_desemprego   <- preparar(desemprego,            "taxa_desemprego")

# --- 4.1 Excesso de mortalidade: pré-COVID vs. COVID ---
teste_t_mortalidade <- rodar_teste_t_pareado(dados_mortalidade, "excesso_mortalidade_anual_pct",
                                             "Excesso de mortalidade: pré-COVID vs. COVID")

# --- 4.2 Expectativa de vida: pré-COVID vs. COVID ---
teste_t_expectativa <- rodar_teste_t_pareado(dados_expectativa, "expectativa_vida",
                                             "Expectativa de vida: pré-COVID vs. COVID")

# --- 4.3 Crescimento do PIB: pré-COVID vs. COVID ---
teste_t_pib <- rodar_teste_t_pareado(dados_pib, "crescimento_pib_pct",
                                     "Crescimento do PIB: pré-COVID vs. COVID")

# --- 4.4 Taxa de desemprego: pré-COVID vs. COVID ---
teste_t_desemprego <- rodar_teste_t_pareado(dados_desemprego, "taxa_desemprego",
                                            "Taxa de desemprego: pré-COVID vs. COVID")
cat("\n[NOTA DE INTERPRETAÇÃO] Vários países usaram programas de suspensão de\n")
cat("contrato (ex.: Kurzarbeit na Alemanha, layoff em Portugal) durante a\n")
cat("COVID para manter trabalhadores tecnicamente 'empregados'. Isso pode\n")
cat("mascarar o impacto real no mercado de trabalho -- não interprete uma\n")
cat("taxa de desemprego estável/baixa como ausência de efeito da pandemia.\n")

# --- 4.5 ANOVA: excesso de mortalidade por região, durante a COVID ---
anova_mortalidade <- rodar_anova_regiao(dados_mortalidade, "excesso_mortalidade_anual_pct",
                                        "ANOVA: excesso de mortalidade por região (2020-2021)")

# --- 4.6 ANOVA: crescimento do PIB por região, durante a COVID ---
anova_pib <- rodar_anova_regiao(dados_pib, "crescimento_pib_pct",
                                "ANOVA: crescimento do PIB por região (2020-2021)")

# --- 4.7 Associação: região x impacto severo (excesso de mortalidade) ---
resultado_associacao <- rodar_teste_associacao("Região x impacto severo na mortalidade")

# --- 4.8 Correlação: excesso de mortalidade x crescimento do PIB (COVID) ---
resultado_correlacao <- rodar_correlacao_saude_economia(dados_mortalidade, dados_pib,
                                                        "Correlação: excesso de mortalidade x crescimento do PIB")

# --- 4.9 Estimação pontual/intervalar: os 4 indicadores durante a COVID ---
estimativa_mortalidade <- estimar_media_intervalo(dados_mortalidade, "excesso_mortalidade_anual_pct",
                                                  "2020-2021 (COVID)", "Excesso de mortalidade (%)")
estimativa_expectativa <- estimar_media_intervalo(dados_expectativa, "expectativa_vida",
                                                  "2020-2021 (COVID)", "Expectativa de vida (anos)")
estimativa_pib <- estimar_media_intervalo(dados_pib, "crescimento_pib_pct",
                                          "2020-2021 (COVID)", "Crescimento do PIB (%)")
estimativa_desemprego <- estimar_media_intervalo(dados_desemprego, "taxa_desemprego",
                                                 "2020-2021 (COVID)", "Taxa de desemprego (%)")

# ==============================================================================
# 5. EXPORTAÇÃO
# ==============================================================================
dir.create("saida_analise", showWarnings = FALSE)
write.csv(dados_mortalidade %>% distinct(geo, year, .keep_all = TRUE) %>%
            select(geo, year, excesso_mortalidade_anual_pct),
          "saida_analise/excesso_mortalidade.csv", row.names = FALSE)
write.csv(resultado_associacao$dados, "saida_analise/muito_afetados.csv", row.names = FALSE)
write.csv(resultado_correlacao$dados, "saida_analise/correlacao_saude_economia.csv", row.names = FALSE)

cat("\n===== ANÁLISE FINALIZADA — 4 indicadores, testes clássicos, sem amostragem, sem pseudorreplicação =====\n")


