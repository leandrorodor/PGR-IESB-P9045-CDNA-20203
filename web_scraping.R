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


###############################################################################
#    Funções para realizar o scrape dos detalhes de cada frete anunciado
coleta_detalhes = function(link_detalhes){
  detalhes = read_html(link_detalhes)
  
  peso = detalhes %>% 
    html_nodes(".details-truckload section+ section .information:nth-child(1) .information-end") %>% 
    html_text()
  
  carga_completa = detalhes %>% 
    html_nodes(".details-truckload section:nth-child(1) .information:nth-child(2) .information-end") %>% 
    html_text()
  
  tipo_carregamento = detalhes %>% 
    html_nodes(".details-truckload section:nth-child(1) .information~ .information+ .information .information-end") %>% 
    html_text()
  
  natureza_carga = detalhes %>% 
    html_nodes(".details-truckload section+ section .information~ .information+ .information .information-end") %>% 
    html_text()
  
  tamanho = detalhes %>% 
    html_nodes(".details-truckload section+ section .information:nth-child(2) .information-end") %>% 
    html_text()
  
  distancia = detalhes %>% 
    html_node(".information-distance .information-end") %>% 
    html_text()
  
  frete_km = detalhes %>% 
    html_nodes(".details-payment section:nth-child(1) .information:nth-child(2) .information-end") %>% 
    html_text()
  
  retorno <- list(c(peso, carga_completa, tipo_carregamento, natureza_carga, tamanho,
                  distancia, frete_km))
  
  return(retorno)
}



###############################################################################
#    Carrega a página a ser digerida e separa seu código HTML. O código está 
# desenhado para coletar as 10 primeiras páginas do site. 

truckpad = data.frame()

for (pagina in seq(from=1, to=10)){
  link = paste0("https://www.truckpad.com.br/fretes/1/?pagina=", pagina)
  
  link = "https://www.truckpad.com.br/fretes/"
  
  htmlPage = read_html(link)

  links_detalhes = htmlPage %>% 
    html_nodes(".button-detail") %>%
    html_attr("href") %>%
    paste("https://www.truckpad.com.br", ., sep="")
  
  origem = htmlPage %>% 
    html_nodes(".route .information:nth-child(1) .information-content") %>% 
    html_text()
  
  destino = htmlPage %>% 
    html_nodes(".route .information+ .information .information-content") %>% 
    html_text()
  
  veiculo = htmlPage %>% 
    html_nodes(".card-content-vehicle .information:nth-child(1) .information-end") %>% 
    html_text()
  
  carroceria = htmlPage %>% 
    html_nodes(".information+ .information .information-content.information-end") %>% 
    html_text()
  
  preco = htmlPage %>% 
    html_nodes(".information-price .information-content") %>% html_text()
  
  preco_tonelada = htmlPage %>% 
    html_nodes(".information-per-ton") %>% html_text()
  
  detalhes = sapply(links_detalhes, FUN = coleta_detalhes)
  
  peso = NULL
  carga_completa = NULL
  tipo_carregamento = NULL
  natureza_carga = NULL
  tamanho = NULL
  distancia = NULL
  frete_km = NULL
  
  for (detalhe in detalhes){
    peso = append(peso, detalhe[1])
    carga_completa = append(carga_completa, detalhe[2])
    tipo_carregamento = append(tipo_carregamento, detalhe[3])
    natureza_carga = append(natureza_carga, detalhe[4])
    tamanho = append(tamanho, detalhe[5])
    distancia = append(distancia, detalhe[6])
    frete_km = append(frete_km, detalhe[7])
  }
  
  truckpad = rbind(truckpad, data.frame(origem, destino, veiculo, carroceria, 
                                        preco, peso, carga_completa, tipo_carregamento,
                                        natureza_carga, tamanho, distancia, frete_km))
}
