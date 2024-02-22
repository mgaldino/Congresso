# Carregar pacotes
library(httr)
library(jsonlite)
library(tidyverse)


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

vec_ano <- 2017:2023
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

urgencia_total_ano <- urgencia %>%
  group_by(ano) %>%
  summarise(num_urgencia = n_distinct(id))

proposicoes_total_ano <- proposicoes %>%
  group_by(ano) %>%
  summarise(num_prop = n_distinct(id))

df_final <- proposicoes_total_ano %>%
  inner_join(urgencia_total_ano, by = "ano") %>%
  mutate(perc_urgencia = round(num_urgencia/num_prop, 3))

df_final
View(df_final)

write.table(df_final, file = "urgencia_17_23.csv", sep=";", row.names = FALSE)
###
###

# apenas PL
pl_total_ano <- proposicoes %>%
  filter(siglaTipo == "PL") %>%
  group_by(ano) %>%
  summarise(num_prop = n_distinct(id))


df_pl <- pl_total_ano %>%
  inner_join(urgencia_total_ano, by = "ano") %>%
  mutate(perc_urgencia = round(num_urgencia/num_prop, 3))

df_pl


write.table(df_pl, file = "urgencia__pls_17_23.csv", sep=";", row.names = FALSE)
###
###



# Definir a URL para o endpoint de tipos de proposição
url_tipos <- "https://dadosabertos.camara.leg.br/api/v2/referencias/tiposProposicao"

# Fazer a requisição
response_tipos <- GET(url_tipos)

# Checar se a requisição foi bem sucedida
if (status_code(response_tipos) == 200) {
  # Converter o conteúdo da resposta de JSON para um dataframe
  content_tipos <- fromJSON(rawToChar(response_tipos$content))
  tipos_proposicao <- as.data.frame(content_tipos$dados)
  
  # Mostrar os primeiros registros para verificação
  print(head(tipos_proposicao))
} else {
  cat("Erro na requisição: ", status_code(response_tipos), "\n")
}

# 391, 392, 406, 878

urgencia <- proposicoes_2017 %>%
  filter(codTipo %in% c(391, 392, 406, 878))

urgencia %>%
  summarise(num = n_distinct(id))
