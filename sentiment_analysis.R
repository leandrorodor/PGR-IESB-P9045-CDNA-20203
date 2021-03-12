###############################################################################
# Aula baseada nas lições do professor Walmes M. Zeviani, da UFPR
# Base de dados do site http://www.carrosnaweb.com.br/.
###############################################################################

###############################################################################
#    Instalação das bibliotecas necessárias para o exemplo de 
#    Instalações mantidas comentadas para evitar problemas de conflito com ins-
# -talações preexistentes.
#
# install.packages("jsonlite")
# install.packages("tm")
# install.packages("tidytext")
# install.packages("tidyverse")
# install.packages("DT")
# install.packages("wordcloud")
# install.packages("lexiconPT")
# install.packages("leaflet")
###############################################################################
#    Carrega as bibliotecas necessárias para realização exemplo

library(jsonlite)
library(tm)
library(tidytext)
library(tidyverse)
library(DT)
library(wordcloud)
library("leaflet")

# Carrega os dicionários léxicos em português
library(lexiconPT)
ls("package:lexiconPT")

###############################################################################
# Carrega um JSON com exemplos de opiniões sobre veículos 
url <- paste0("https://github.com/leg-ufpr/hackathon/blob/master", "/opinioes.json?raw=true")

txt <- fromJSON(url)
str(txt)

# Transforma opiniões em tabela para análise
colnames(txt) <- c("id", "title", "model", "owner", "condition", "good",
                   "bad", "defect", "general", "ts")
tt <- as_tibble(txt)
glimpse(tt)

tt$product <- tt$model %>%
  str_extract("^([[:alpha:]]+ +[[:alpha:]]+)") %>%
  str_to_upper()


tt$product %>% 
  unique() %>%
  dput()

tt %>%
  count(product, sort = TRUE)

# Aplica filtro para reter apenas um modelo de carro.
mod <- c("CHEVROLET CELTA",
         "CHEVROLET ONIX",
         "FIAT PALIO",
         "FIAT UNO",
         "HYUNDAI HB",
         "RENAULT SANDERO",
         "VOLKSWAGEN FOX",
         "VOLKSWAGEN GOL")[4]

texto <- tt %>%
  filter(str_detect(product, mod)) %>%
  select(id, general)

texto

###############################################################################
# Preprocessamento do texto para análise de sentimentos.

# Faz o preproceamento padrão do texto.
texto$general <- texto$general %>%
  str_replace("Opinião Geral:", "") %>%   # Remove começo.
  str_to_lower() %>%                      # Caixa baixa.
  str_replace_all(" *-+ *", "") %>%       # Remove hífen.
  str_replace_all("[[:punct:]]", " ") %>% # Pontuação por espaço.
  removeNumbers() %>%                     # Remove números.
  trimws()                                # Sem espaços nas bordas.

# Stop words padrão do idioma português.
stopwords(kind = "pt")

# Efeito de remover as stop words.
head(texto$general, n = 1) %>%
  str_wrap(72) %>%
  cat("\n")

head(texto$general, n = 1) %>%
  removeWords(words = stopwords(kind = "pt")) %>%
  str_wrap(72) %>%
  cat("\n")

# Remoção das stop words.
texto$general <- texto$general %>%
  removeWords(words = c("bom", "muito", "pouco",
                        stopwords(kind = "pt")))

# Faz tokenização nas palavras individuais e empilha as palavras.
texto_un <- texto %>%
  unnest_tokens(output = "words", input = general)
texto_un

#-----------------------------------------------------------------------
# Operações para determinar a polaridade.

# Uma amostra do dicionário de termos rotulados.
sample_n(oplexicon_v3.0, size = 20) %>%
  arrange(polarity)

# Contagem por polaridade.
oplexicon_v3.0 %>%
  count(polarity, sort = TRUE)

# Contagem por classe gramatical.
oplexicon_v3.0 %>%
  count(type, sort = TRUE)

# Faz o a junção por interseção.
tb_sen <- inner_join(texto_un,
                     oplexicon_v3.0[, c("term", "polarity")],
                     by = c("words" = "term"))

# Agora o termos tem sua polaridade presente na tabela.
sample_n(tb_sen, size = 20)

# Faz a agregação da polaridade por documento.
tb <- tb_sen %>%
  group_by(id) %>%
  summarise(soma = sum(polarity),
            n = n(),
            sentiment = soma/n)
tb


# Desidade expírica kernel do escore de sentimento.
ggplot(tb, aes(x = sentiment)) +
  geom_density(fill = "orange", alpha = 0.25) +
  geom_rug() +
  labs(x = "Polaridade", y = "Densidade")

# Frequência relativa acumulada.
ggplot(tb, aes(x = sentiment)) +
  stat_ecdf() +
  geom_rug() +
  labs(x = "Polaridade", y = "Frequência")

# As avaliações mais positivas.
tb %>%
  top_n(sentiment, n = 10) %>%
  inner_join(tt[, c("id", "general")]) %>%
  select(sentiment, general)

# As avaliações mais negativas.
tb %>%
  top_n(sentiment, n = -10) %>%
  inner_join(tt[, c("id", "general")]) %>%
  select(sentiment, general)

#-----------------------------------------------------------------------
# Exibição dos resultados.

# Tabela com as avaliações originais sem o preprocessamento.
tb_u <- tb %>%
  inner_join(tt[, c("id", "general")]) %>%
  select(id, sentiment, general) %>%
  mutate(general = str_replace(general, "Opinião Geral:", ""))

# Valores e cores para formatação condicional das cédulas da tabela.
vals <- seq(-1, 1, by = 0.1)
cols <- colorRampPalette(
  RColorBrewer::brewer.pal(n = 6, name = "Spectral"))(length(vals))
# plot(vals, col = cols, pch = 19, cex = 5)

# Define o estilo de formatação condicional.
style <- styleInterval(cuts = head(vals[-1], n = -1),
                       values = cols[-1])

html_table <-
  datatable(tb_u,
            colnames = c("Avaliação",
                         "Sentimento",
                         "Opinião Geral")) %>%
  formatRound(columns = "sentiment", digits = 2) %>%
  formatStyle(columns = names(tb_u),
              valueColumns = "sentiment",
              target = "cell",
              backgroundColor = style)
html_table


#-----------------------------------------------------------------------
# Para poder melhorar o dicionário.

# Determina as frequências dos termos de polaridade não nula.
tb_words <- tb_sen %>%
  count(words, polarity, sort = TRUE) %>%
  filter(polarity != 0)

tb_cloud <- tb_words %>%
  spread(key = "polarity", value = "n", fill = 0) %>%
  rename("negative" = "-1", "positive" = "1")
tb_cloud

tb <- as.data.frame(tb_cloud[, c("negative", "positive")])
rownames(tb) <- tb_cloud$words
head(tb)

# Faz núvem de palavras.
comparison.cloud(tb,
                 colors = c("red", "blue"),
                 max.words = min(nrow(tb), 200))

# Gráfico de barras para as palavras de maior ocorrência.
n_words <- 20
tb_bars <- tb_words %>%
  mutate(score = polarity * n) %>%
  group_by(polarity) %>%
  top_n(n, n = n_words) %>%
  ungroup()

ggplot(data = tb_bars,
       mapping = aes(x = reorder(words, score),
                     y = score,
                     fill = score)) +
  geom_col(color = "black") +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  coord_flip() +
  theme_light() +
  theme(legend.position = c(0.95, 0.5),
        legend.justification = c(1, 0.5)) +
  labs(y = "Frequência de ocorrência",
       x = "Termo",
       fill = "Frequência de\nocorrência")


#-----------------------------------------------------------------------
# Carrega dados com localização dos municípios.

est <- "https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/estados.csv"
mun <- "https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/municipios.csv"
latlon <- inner_join(read_csv(est),
                     read_csv(mun),
                     by = "codigo_uf")

latlon <- latlon %>%
  rename("estado" = "nome.x", "municipio" = "nome.y")
str(latlon)      

tb <- tt %>%
  filter(str_detect(product, mod))
tb



# Extração da localização.
tb$loc <- tb$owner %>%
  str_extract("[^-]*$") %>%
  str_trim()

# Extração da sigla do estado.
tb$uf <- tb$loc %>%
  str_extract("[[:upper:]]{2}$")

# Extração do município.
tb$mun <- tb$loc %>%
  str_replace("^(.*) [[:upper:]]{2}$", "\\1")

# Fazer a junção para incluir lat&lon das cidades.
tb <- inner_join(tb,
                 select(latlon, municipio, uf, latitude.y, longitude.y),
                 by = c("mun" = "municipio", "uf" = "uf"))

# Conta o número de avaliações por cidade.
tb_locs <- tb %>%
  count(uf, mun, latitude.y, longitude.y)

#-----------------------------------------------------------------------
# Gráfico interativo.

library(leaflet)

leaflet(data = tb_locs) %>%
  addProviderTiles(provider = "Stamen.TonerLite") %>%
  addCircleMarkers(
    lng = ~longitude.y,
    lat = ~latitude.y,
    radius = ~3 + 1.0 * abs(n),
    color = "tomato",
    fillOpacity = 0.75,
    label = ~sprintf("%s/%s: %d", mun, uf, n),
    labelOptions = labelOptions(textsize = "15px"),
    stroke = TRUE)

