#Analysis
library(tidyverse)
library(readxl)
urgencia_manual <- read_excel("urgencia_manual.xlsx")
glimpse(urgencia_manual)
length(unique(urgencia_manual$apreciacao))

padrao1 <- "(prc|plp|pl|projeto de lei)\\s(n|n.)?\\s?([0-9]+[,.\\s]?[0-9]*[A-Za-z]?)(,? de |/)?(\\d{2,4})"

gsub("\\.", "", "requer regime de urgência para apreciação do pl n 5.533")

teste <- urgencia_manual$ementa_simplificada[is.na(urgencia_manual$apreciacao)]
# teste <- gsub("\\.", "", teste)
str_extract(teste, "padrao1")

teste1 <- urgencia$ementa_simplificada[urgencia$id == 2023953]
str_extract(teste1, "(projeto de decreto legislativo \\(pdc\\))")
