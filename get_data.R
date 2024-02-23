# Carregar pacotes
library(httr)
library(jsonlite)
library(tidyverse)
library(stringr)

##

get_proposicoes <- function(year) {
  base_url <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes"
  proposicoes <- data.frame() # Data frame vazio para armazenar os resultados
  
  ano <- year
  pagina_atual <- 1
  itens_por_pagina <- 100 # Ajuste conforme necessário, respeitando os limites da API
  fim <- FALSE
  
  while (!fim) {
    params <- list(
      'ano' = ano,
      'itens' = itens_por_pagina,
      'pagina' = pagina_atual,
      'ordem' = 'ASC'
    )
    
    response <- GET(url = base_url, query = params)
    
    if (status_code(response) == 200) {
      content <- fromJSON(rawToChar(response$content))
      
      if (length(content$dados) == 0) {
        fim <- TRUE
      } else {
        temp_df <- as.data.frame(content$dados)
        proposicoes <- rbind(proposicoes, temp_df)
        pagina_atual <- pagina_atual + 1
      }
    } else {
      stop("Erro na requisição: ", status_code(response))
    }
  }
  
  return(proposicoes)
}

vec_ano <- 2010:2023
lista_proposicoes <- list()
lista_urgencia <- list()
for (i in 1:length(vec_ano)){
  print(i)
  lista_proposicoes[[i]] <- get_proposicoes(year=vec_ano[i])
  lista_urgencia[[i]] <- lista_proposicoes[[i]] %>%
    filter(codTipo %in% c(391, 392, 406, 878))
  Sys.sleep(1)
}

proposicoes <- bind_rows(lista_proposicoes)

urgencia <- bind_rows(lista_urgencia) 

# dput(c(urgencia$ementa[urgencia$id==469773], urgencia$ementa[urgencia$id==469774], urgencia$ementa[urgencia$id==469956]))
saveRDS(proposicoes, file="proposicoes2010-23.rds")
saveRDS(urgencia, file="urgencia2010-23.rds")

proposicao_id <- 500080
url <- "https://dadosabertos.camara.leg.br/api/v2/referencias/proposicoes/codSituacao"
response <- GET(url)
content <- fromJSON(rawToChar(response$content))
# Verificar o status da resposta
if (status_code(response) == 200) {
  # Converter o conteúdo da resposta de JSON para um data frame
  content <- fromJSON(rawToChar(response$content))
  situacao <- content$dados$situacao
  
  # Imprimir a situação da proposição
  print(situacao)
} else {
  cat("Erro na requisição: ", status_code(response), "\n")
}


get_sit_proposicoes <- function(year) {
  base_url <- "https://dadosabertos.camara.leg.br/api/v2/referencias/proposicoes/codSituacao"
  proposicoes <- data.frame() # Data frame vazio para armazenar os resultados
  
  ano <- year
  pagina_atual <- 1
  itens_por_pagina <- 100 # Ajuste conforme necessário, respeitando os limites da API
  fim <- FALSE
  
  while (!fim) {
    params <- list(
      'ano' = ano,
      'itens' = itens_por_pagina,
      'pagina' = pagina_atual,
      'ordem' = 'ASC'
    )
    
    response <- GET(url = base_url, query = params)
    
    if (status_code(response) == 200) {
      content <- fromJSON(rawToChar(response$content))
      
      if (length(content$dados) == 0) {
        fim <- TRUE
      } else {
        temp_df <- as.data.frame(content$dados)
        proposicoes <- rbind(proposicoes, temp_df)
        pagina_atual <- pagina_atual + 1
      }
    } else {
      stop("Erro na requisição: ", status_code(response))
    }
  }
  
  return(proposicoes)
}
