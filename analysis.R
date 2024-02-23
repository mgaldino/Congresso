# analysis
library(ggplot2)

urgencia_ano_tipo <- urgencia %>%
  group_by(ano, tipo_apreciacao) %>%
  summarise(qtde = n_distinct(id))

urgencia_ano_tipo %>%
  ggplot(aes(x=ano, y=qtde, group = tipo_apreciacao,
             colour = tipo_apreciacao)) +
  geom_line()

urgencia_ano_tipo %>%
  filter(tipo_apreciacao != "pl") %>%
  ggplot(aes(x=ano, y=qtde, group = tipo_apreciacao,
             colour = tipo_apreciacao)) +
  geom_line()

## proportion

prop_urgencias <- proposicoes %>%
  left_join(select(urgencia, id, apreciacao, tipo_apreciacao), by="id")
