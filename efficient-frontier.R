
# instalar pacotes --------------------------------------------------------

# install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
# install.packages("tidyquant")
# install.packages("tidyverse")
# install.packages("ggrepel")
# install.packages("ggcorrplot")
# install.packages("scales")
# install.packages("knitr")
# install.packages("extrafont")

# carregar pacotes --------------------------------------------------------

library(IntroCompFinR)
library(tidyquant)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)
library(scales)
library(knitr)
library(extrafont)
# font_import()
# loadfonts(device = "win")
# fonts()

options(OutDec = ",")


# tickers -----------------------------------------------------------------

varejo <- c("MGLU3.SA", "VVAR3.SA", "LAME4.SA")
energia <- c("ENBR3.SA", "EGIE3.SA", "ENGI11.SA")
financeiro <- c("ITSA4.SA", "BBDC4.SA", "BBAS3.SA")
siderurgia <- c("GGBR4.SA", "CSNA3.SA", "USIM5.SA")

# juntando tudo

tickers <- c(
  varejo,
  energia,
  financeiro,
  siderurgia
)


# pegando as cotacoes ----------------------------------------------------------------

# pegando cotacoes e salvando em um arquivo csv

# tq_get(
#   tickers,
#   get  = "stock.prices",
#   from = "2012-04-15",
#   to   = "2021-05-15",
#   periodicity = "monthly"
# ) %>%
#   write_csv(file = "cotacoes.csv")


# lendo o csv

cotacoes <- read_csv(file = "cotacoes.csv")



# calculando retornos, desvio padrao, correlacao e covariancia ------------

# retornos

retornos <- cotacoes %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "retornos"
  )

# matriz de covariancia

matriz_cov <- retornos %>%
  pivot_wider(names_from = symbol, values_from = retornos) %>%
  select(-date) %>%
  cov()

matriz_cov

# matriz de correlacao

matriz_cor <- retornos %>%
  pivot_wider(names_from = symbol, values_from = retornos) %>%
  select(-date) %>%
  cor()

matriz_cor

# retorno medio, desvio padrao e acrescentando setores

ret_med_desv_pad <- retornos %>%
  summarise(
    retorno_medio = mean(retornos),
    desvio_padrao = sd(retornos)
  ) %>%
  mutate(
    setor = case_when(
      symbol %in% energia    ~ "Energia",
      symbol %in% siderurgia ~ "Siderurgia",
      symbol %in% varejo     ~ "Varejo",
      symbol %in% financeiro ~ "Financeiro"
    ),
    .after = symbol
  )


# grafico de correlacao ---------------------------------------------------

plt_correl <- ggcorrplot(
  matriz_cor,
  type = "lower",
  hc.order = TRUE,
  #method = "circle",
  lab = TRUE,
  #lab_size = 3,
  colors = c("firebrick", "white", "dodgerblue4"),
  legend.title = "Correlação",
  ggtheme = ggplot2::theme_minimal()
) +
  theme(text = element_text(family = "Times New Roman", size = 16))

# exportar grafico

ggsave(
  filename = "./export-plots/plt_correl.svg",
  plot = plt_correl,
  scale = 1.5,
  device = "svg"
)


# grafico retorno medio e desvio padrao -----------------------------------

plt_acoes <- ret_med_desv_pad %>%
  ggplot() +
  aes(x = desvio_padrao, y = retorno_medio, colour = setor, label = symbol) +
  geom_point() +
  geom_label_repel(
    aes(label = symbol),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  scale_x_continuous(label = percent_format(decimal.mark = ",")) +
  scale_y_continuous(label = percent_format(decimal.mark = ",")) +
  labs(x = "Desvio Padrão", y = "Retorno Esperado", color = "Setor") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 18))

# exportar grafico

ggsave(
  filename = "./export-plots/plt_acoes.svg",
  plot = plt_acoes,
  scale = 1,
  device = "svg"
)


# dados para gerar a fronteira --------------------------------------------

# taxa livre de risco

tx_livre_risco <- 0.00622695561204112


# sem vendas a descoberto

short_selling <- FALSE


# vetor retorno medio
# necessario para visualizar os tickers das acoes em portfolio weights

vec_retorno_medio <- ret_med_desv_pad %>%
  select(symbol, retorno_medio) %>%
  pivot_wider(names_from = symbol, values_from = retorno_medio) %>%
  as_vector()

# carteira de tangencia

carteira_tangencia <- tangency.portfolio(
  er = vec_retorno_medio,
  cov.mat =  matriz_cov,
  risk.free =  tx_livre_risco,
  shorts = short_selling
)


# carteira de minima variancia

carteira_min_var <- globalMin.portfolio(
  er = vec_retorno_medio,
  cov.mat = matriz_cov,
  shorts = short_selling
)


# fronteira eficiente

fronteira_eficiente <- efficient.frontier(
  er = vec_retorno_medio,
  cov.mat = matriz_cov,
  nport = 100,
  shorts = short_selling
)

# juntando os retornos esperados e desvio padrao dos 100 portfolios em um df

df_fronteira <- data.frame(
  retorno_esperado = fronteira_eficiente$er,
  desvio_padrao = fronteira_eficiente$sd
)

# reta tangente (CML)

tangente <- (carteira_tangencia$er - tx_livre_risco) / carteira_tangencia$sd


# grafico fronteira eficiente ---------------------------------------------


plt_fronteira <- df_fronteira %>%
  ggplot() +
  aes(x = desvio_padrao, y = retorno_esperado) +
  geom_line(size = 0.75) +
  scale_x_continuous(
    label = percent_format(decimal.mark = ","),
    expand = c(0, 0)
  ) +
  scale_y_continuous(label = percent_format(decimal.mark = ",")) +
  expand_limits(x = c(0, 0.25), y = c(0, 0.06)) +
  geom_abline(
    slope = tangente,
    intercept = tx_livre_risco,
    color = "red",
    size = 1
  ) +
  geom_point(
    aes(x = carteira_min_var$sd, y = carteira_min_var$er),
    size = 3,
    color = "red"
  ) +
  annotate(
    "text",
    x = 0.038,
    y = 0.014,
    label = "Carteira de \n Mínima Variância",
    size = 4
  ) +
  geom_point(
    aes(x = carteira_tangencia$sd, y = carteira_tangencia$er),
    size = 3,
    color = "black"
  ) +
  annotate(
    "text",
    x = 0.07,
    y = 0.047,
    label = "Carteira de Tangência",
    size = 4
  ) +
  geom_point(
    ret_med_desv_pad,
    mapping = aes(
      x = desvio_padrao,
      y = retorno_medio,
      colour = symbol,
      shape = setor
    ),
    size = 3
  ) +
  scale_shape_manual(values = c(8, 17, 18, 15)) +
  labs(
    x = "Desvio Padrão",
    y = "Retorno Esperado",
    color = "Código da Ação",
    shape = "Setor"
  ) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 18))

# exportar grafico

ggsave(
  filename = "./export-plots/plt_fronteira.svg",
  plot = plt_fronteira,
  scale = 1.5,
  device = "svg"
)


# gerando tabelas em formato LaTeX ------------------------------------------------

## tabela com as acoes, nomes das empresas e setor

# nomes das empresas

nomes_empresas <- c(
  "Magazine Luiza",
  "Via Varejo",
  "Lojas Americanas",
  "Edp - Energias do Brasil",
  "Engie Brasil Energia",
  "Energisa Unt",
  "Itausa",
  "Banco Bradesco",
  "Banco do Brasil",
  "Gerdau",
  "Companhia Siderúrgica Nacional",
  "Usinas Siderúrgicas de Minas Gerais - Usiminas"
)

# nomes das empresas + tickers

nomes_empresas_tickers <-
  bind_cols("symbol" = tickers, "nome_empresa" = nomes_empresas)

# tabela

ret_med_desv_pad %>%
  left_join(nomes_empresas_tickers, by = "symbol") %>%
  select(symbol, nome_empresa, setor) %>%
  arrange(setor) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    align = "c",
    digits = 2,
    table.envir = "table",
    position = "!h",
    format.args = list(big.mark = ".", scientific = FALSE),
    col.names = c(
      "\\textbf{Ativo / Código}",
      "\\textbf{Empresa}",
      "\\textbf{Setor}"
    ),
    escape = FALSE
  ) %>%
  write_file("./export-tables/empresas.tex")


## tabela com os pesos da carteira de tangencia e da carteira de min. var.

# dataframe com os pesos

df_pesos_carteiras <- data.frame(
  pesos_carteira_tangencia = carteira_tangencia$weights*100,
  pesos_carteira_min_var = carteira_min_var$weights*100
) %>%
  rownames_to_column(var = "symbol")


# Tabela

df_pesos_carteiras %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    align = "c",
    digits = 2,
    table.envir = "table",
    position = "!h",
    format.args = list(
      big.mark = ".",
      decimal.mark = ",",
      scientific = FALSE
    ),
    col.names = c(
      "\\textbf{Ativo / Código}",
      "\\textbf{Carteira de Tangência}",
      "\\textbf{Carteira de Mínima Variância}"
    ),
    escape = FALSE
  ) %>%
  write_file("./export-tables/pesos_carteiras.tex")
