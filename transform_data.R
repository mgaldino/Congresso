library(tidyverse)
library(stringr)

# import raw data
proposicoes <- readRDS("proposicoes2010-23.rds")
urgencia <- readRDS("urgencia2010-23.rds")

#
# data wrangling
#

urgencia <- urgencia %>%
  mutate(ementa_simplificada = gsub("(°|º)", "", ementa ),
         ementa_simplificada = tolower(ementa_simplificada),
         ementa_simplificada = str_squish(ementa_simplificada),
         ementa_simplificada = gsub("\\\\", "/", ementa_simplificada),
         ementa_simplificada = gsub("\\.", "", ementa_simplificada ))# Substituir barras invertidas por barras regulares

# 2407464
# Expressão regular ajustada para capturar o ano em diferentes formatos
padrao <- "(projeto de decreto legislativo|recurso|projeto de decreto legislativo \\(pdc\\)|prc|mensagem|pdc|plp|pl|pdl|msc|projeto d[eo] lei|projeoto de lei|projeto d[eo] lei \\(pl\\)|projeto de lei do senado federal|projeto de lei complementar|projeto de lei \\(pl\\)| projeto de resolução \\(prc\\)|projeto de resolução)\\s?(n|no)?\\s?([0-9]+[,]?[0-9]*[A-Za-z]?)(,? de |/)?\\s?(\\d{2,4})"


urgencia <- urgencia %>%
  mutate(apreciacao = str_extract(ementa_simplificada, padrao),
         apreciacao = ifelse(!is.na(apreciacao), apreciacao,
                             ifelse(grepl("apreciação d[ae] proposição", ementa_simplificada), "proposicao sem mais", siglaTipo)),
         e_urgencia = grepl("urgência", ementa_simplificada),
         e_projeto_resolução = grepl("projeto de resolução", ementa_simplificada))


ids_msgs <- c(542817, 2242275, 2242276, 2242277, 2242278, 2242279,
              2242322, 2243019, 2243426, 2248259,2249966, 2249967, 
              2249968, 2249969, 2250596, 2251382, 2252201,2253944,
              2255237, 2266304, 2274128, 2292191, 2388824, 542817,
              544868, 618377, 945919, 1805782, 2028638, 2077903)

 msg_replace <- c("MENSAGEM Nº 145/2012", "Projeto de Lei nº 1089/2020", "Projeto de Lei nº
1142/2020", "Projeto de Lei nº 892/2020", "Projeto de Lei nº 425/2020",
                 "Projeto de Lei nº 987/2020", "Projeto de Lei nº 886/2020", "Projeto de Lei nº 1499/2020", 
                 "Projeto de Lei nº 1531/2020", "Projeto de Lei nº 1069/2020","Projeto de Lei Complementar nº 88/2020",
                 "Projeto de Lei Complementar nº 100/2020", "Projeto de Lei nº 1.868/2020",
                 "Projeto de Lei nº 1.946/2020", "Projeto de Lei nº 1.946/2020",
                 "Projeto de Lei nº 1667 de 2020", "Projeto de Lei nº 2353/2020",
                 "Projeto de Lei nº 2389/2020", "Projeto de Lei nº 3320",
                 "Projeto de Lei nº 5191/2020", "Projeto de Lei nº 582/2021",
                 "Projeto de Lei nº 2.337/2021", "Projeto de Resolução nº 109/2019",
                 "MENSAGEM Nº 145/2012", "Projeto de Decreto Legislativo n.º 3030",
                 "Projeto de Lei nº 3.877 de 2004", "Projeto de Resolução 151 de 2012",
                 "Projeto de Lei nº 3.763, de 2004", "Projeto de Lei nº 8194/2014", 
                 "Projeto de Lei 3636, de 2015")

# urgencia_manual <- read_excel("urgencia_manual.xlsx")

df_replace <- data.frame(id= ids_msgs, apreciacao_replace = msg_replace)

urgencia <- urgencia %>%
  left_join(df_replace, by = "id") %>%
  mutate(apreciacao = ifelse(apreciacao == "REQ", apreciacao_replace, apreciacao))

# tipo urgenica
urgencia <- urgencia %>%
  mutate(tipo_apreciacao = ifelse(grepl("(pl|[pP]rojeto d[eo] [lL]ei|projeoto de lei)", apreciacao), "pl",
                                  ifelse(grepl("(mensagem|MENSAGEM|MSC|msc)", apreciacao), "msg",
                                         ifelse( grepl("[pP]rojeto de [rR]esolução", apreciacao), "Projeto de Resolução", 
                                                 ifelse(grepl("([Pp]rojeto de [Dd]ecreto [Ll]egislativo|pdl)", apreciacao), "pdl",
                                                        ifelse(grepl("pdc", apreciacao), "pdc",
                                                               ifelse(grepl("prc", apreciacao), "prc", apreciacao)))))))


# 488578, 489589, 490056, 506803, 506804, 553474, 562497,
# 577876, 604093, 618787

# 1301189 PDC nº 18
# protetivas de urgência - evitar

# urgencia <- urgencia %>%
#   mutate(apreciacao = ifelse(e_urgencia & !e_projeto_resolução & apreciacao == "REQ" & grepl("projeto de [0-9]+", ementa_simplificada),
#                              str_extract(ementa_simplificada, "projeto de [0-9]+/[0-9]+"), apreciacao))

# save transformed data
saveRDS(urgencia, file="urgencia2010-23_modificado.rds")

# urgencia <- urgencia %>%
#   mutate(ementa = str_replace_all(ementa, "[\r\n\t]" , ""),
#          ementa_simplificada = str_replace_all(ementa_simplificada, "[\r\n\t]" , ""))
# csv for manual adjustment
# write.table(urgencia, file = "urgencia.csv", row.names = FALSE, sep="\t")

tipo_pl_urgencia <- urgencia %>%
  inner_join(select(proposicoes, c(id, siglaTipo, numero, ano)), by="id") %>%
  arrange(id)


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

padrao1 <- "(projeto de decreto legislativo|projeto de decreto legislativo (pdc)|prc|mensagem|pdc|plp|pl|pdl|msc|projeto de lei|projeto d[eo] lei (pl)|projeto de lei do senado federal|projeto de lei complementar|projeto de resolução)\\s?(n|no)?\\.?\\s?([0-9]+[,.\\s]?[0-9]*[A-Za-z]?)(,? de |/|/ )?(\\d{2,4})"

# apenas PL
pl_total_ano <- proposicoes %>%
  filter(siglaTipo %in% c("PL", "PLP", "PLSC", "PLD", "PLN", "PDL", "PLS")) %>%
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
         