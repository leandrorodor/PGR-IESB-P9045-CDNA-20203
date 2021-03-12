###############################################################################
#    Instalação das bibliotecas necessárias para o scrape do site Fretebras.
#    Instalações mantidas comentadas para evitar problemas de conflito com ins-
# -talações preexistentes.

# install.packages("rvest")
# install.packages("dplyr")
# install.packages("xml2")
###############################################################################
#    Carrega as bibliotecas necessárias para o scrape

library(xml2)
library(rvest)
library(dplyr)
library(data.table)

link = "https://www.truckpad.com.br/fretes/"

htmlPage = read_html(link)

origem = htmlPage %>%
  html_nodes(".route .information:nth-child(1) .information-content") %>%
  html_text()

destinos = htmlPage %>%
  html_nodes(".route .information+ .information .information-content") %>%
  html_text()

preco = htmlPage %>%
  html_nodes(".information-price .information-content") %>%
  html_text()

peso = htmlPage %>%
  html_nodes(".card-content-detail .information-end") %>%
  html_text()

truckpad = data.frame(origem, destinos, peso, preco)
