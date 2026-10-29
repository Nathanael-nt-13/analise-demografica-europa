# Análise Estatística dos Impactos da COVID-19 na Europa

## Sobre o projeto

Este projeto tem como objetivo analisar os impactos da pandemia de COVID-19 nos 27 países da União Europeia (UE-27), utilizando dados do Eurostat e técnicas de análise estatística com a linguagem R.

A análise compara indicadores demográficos, sanitários e econômicos entre o período pré-pandemia (2014–2019) e o período pandêmico (2020–2021), investigando mudanças temporais, diferenças regionais e possíveis associações entre os impactos da pandemia na saúde e na economia.

## Objetivos

- Extrair, tratar e organizar dados oficiais do Eurostat.
- Analisar a evolução dos indicadores antes e durante a pandemia.
- Comparar os resultados entre os períodos pré-COVID e COVID-19.
- Investigar diferenças entre quatro regiões de estudo: Norte, Oeste, Sul e Leste.
- Aplicar técnicas de inferência estatística e estimação intervalar.
- Desenvolver visualizações para facilitar a interpretação dos resultados.

## Indicadores analisados

O projeto contempla quatro indicadores principais:

- **Óbitos anuais:** utilizados para calcular uma estimativa própria do excesso de mortalidade.
- **Expectativa de vida ao nascer:** utilizada para investigar mudanças na longevidade.
- **Crescimento do PIB real:** utilizado para avaliar a evolução econômica.
- **Taxa de desemprego:** utilizada para analisar mudanças no mercado de trabalho.

**Período dos dados:** 2014–2022 para a extração, com foco comparativo em 2014–2019 (pré-COVID) e 2020–2021 (COVID).

## Tecnologias e ferramentas

- **R:** processamento e análise estatística.
- **Eurostat:** fonte dos dados.
- **dplyr e tidyr:** manipulação e organização dos dados.
- **countrycode:** identificação dos países.
- **purrr:** apoio às operações com dados.
- **ggplot2 e scales:** construção e apresentação de gráficos.

## Estrutura do projeto

```text
analise-covid-europa/
├── README.md
├── 1_etl_covid_europa_V3.R
├── 2_analise_covid_europa_V3.R
├── 3_graficos_covid_europa_V3.R
├── dados/
│   ├── paises_ue27_regioes.csv
│   ├── obitos.csv
│   ├── expectativa_vida.csv
│   ├── crescimento_pib.csv
│   └── desemprego.csv
└── saida_analise/
    ├── excesso_mortalidade.csv
    ├── muito_afetados.csv
    └── correlacao_saude_economia.csv
```

As pastas `dados/` e `saida_analise/` são geradas e preenchidas durante a execução dos scripts. Os arquivos CSV listados são as saídas esperadas do processamento, não necessariamente arquivos já incluídos no repositório.

## Descrição dos scripts

### 1. Extração e preparação dos dados — ETL

**Arquivo:** `1_etl_covid_europa_V3.R`

Responsável por extrair os dados do Eurostat, selecionar os indicadores de interesse, padronizar códigos geográficos e preparar as bases para análise. Calcula também a variação percentual anual do PIB real e exporta os dados tratados para arquivos CSV.

### 2. Análise estatística e inferencial

**Arquivo:** `2_analise_covid_europa_V3.R`

Realiza as análises estatísticas dos indicadores, incluindo:

- Teste t pareado para comparar os períodos pré-COVID e COVID.
- ANOVA para investigar diferenças entre regiões.
- Teste exato de Fisher com simulação para avaliar associação entre região e classificação de impacto na mortalidade.
- Correlações de Pearson e Spearman entre excesso de mortalidade e crescimento do PIB.
- Estimação de médias e intervalos de confiança.

O script também agrega os dados por país e período em análises que exigem uma observação por unidade, buscando evitar pseudorreplicação.

### 3. Visualização dos dados

**Arquivo:** `3_graficos_covid_europa_V3.R`

Produz gráficos de evolução temporal, comparações pareadas entre períodos e comparações regionais durante a pandemia. As visualizações incluem gráficos de linhas, boxplots e pontos individuais por país, contribuindo para a exploração dos padrões identificados nos dados.

## Metodologia

A análise considera os 27 países da União Europeia definidos no projeto, sem aplicação de pesos amostrais.

Os períodos de comparação são:

- **Pré-COVID:** 2014–2019.
- **COVID:** 2020–2021.

O excesso de mortalidade é calculado a partir da diferença percentual entre os óbitos anuais observados e a média de óbitos utilizada como linha de base histórica.

As análises inferenciais utilizam nível de significância de 5% (\(\alpha = 0{,}05\)). Os resultados devem ser interpretados considerando as características de cada indicador, as limitações dos dados e as premissas dos métodos estatísticos aplicados.

## Como executar o projeto

### Pré-requisitos

Instale o R e, preferencialmente, o RStudio.

Instale os pacotes necessários:

```r
install.packages(c(
  "eurostat",
  "dplyr",
  "countrycode",
  "tidyr",
  "purrr",
  "ggplot2",
  "scales"
))
```

### Ordem de execução

Execute os scripts na seguinte sequência, a partir do diretório raiz do projeto:

1. `1_etl_covid_europa_V3.R` — extrai e prepara os dados.
2. `2_analise_covid_europa_V3.R` — realiza as análises estatísticas.
3. `3_graficos_covid_europa_V3.R` — gera as visualizações.

A execução do primeiro script requer conexão à internet para consultar o Eurostat. Os scripts seguintes dependem dos arquivos produzidos nas etapas anteriores.

## Limitações e considerações

- O excesso de mortalidade utilizado no projeto é um cálculo próprio com dados anuais e linha de base histórica; não corresponde ao indicador oficial semanal de excesso de mortalidade do Eurostat.
- A análise de desemprego deve considerar que políticas de manutenção de empregos e suspensão temporária de contratos podem influenciar a interpretação das taxas observadas.
- Associações estatísticas não demonstram, por si só, relações causais.
- Os resultados dependem da disponibilidade, cobertura e comparabilidade dos dados entre países e anos.

## Resultados

Os resultados numéricos, tabelas e gráficos devem ser documentados após a execução dos scripts e a verificação das saídas. Este README descreve os procedimentos implementados, sem antecipar conclusões empíricas.

## Autor

**Nathanael Melo Galvão**

Graduando em Estatística — Universidade Federal de Goiás (UFG).

## Fonte dos dados

[Eurostat — Official European Statistics](https://ec.europa.eu/eurostat)


