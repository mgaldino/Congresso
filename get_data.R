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

##
proposicao_id <- 500080
get_situacao_proposicoes <- function(proposicao_id) {
  base_url <- paste0("https://dadosabertos.camara.leg.br/api/v2/proposicoes/", proposicao_id)
  response <- GET(url = base_url)
  
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content))
    tmp_status = ifelse(is.null(content$dados$statusProposicao$descricaoSituacao),
                       content$dados$statusProposicao$descricaoTramitacao, content$dados$statusProposicao$descricaoSituacao)
    
    df <- data.frame(id = content$dados$id, data = content$dados$dataApresentacao, status = tmp_status,
                     status_apreciacao = content$dados$statusProposicao$apreciacao)
    } else {
    stop("Erro na requisição: ", status_code(response))
    }
  return(df)
  }

list_df <-list()
n <- nrow(urgencia)
for (i in i:n) {
  proposicao_id <- urgencia$id[i]
  list_df[[i]] <- get_situacao_proposicoes(proposicao_id)
  
  if(i%%100 == 0) {print(i)}
}

df_situacao <- bind_rows(list_df)
saveRDS(df_situacao, file="df_situacao2010-23.rds")


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
