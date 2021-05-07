

cheque <- read.csv("avaliacao/arquivos/brutos/contracheque.csv")

cheque %>%
  select(rendimento_liquido)%>%
  arrange(desc(rendimento_liquido))




cheque %>%
  select(rendimento_liquido)%>%
  filter(rendimento_liquido > 39293.32) %>%
  count()

cheque %>%
  select(rendimento_liquido)%>%
  filter(rendimento_liquido > 100000.00) %>%
  count()

cheque %>%
  select(rendimento_liquido, tribunal) %>%
  group_by(tribunal) %>%
  summarise(
    media_rendimento = mean(rendimento_liquido, na.rm = TRUE),
    desvio = sd(rendimento_liquido, na.rm = TRUE),
    cv = (desvio/media_rendimento)*100) %>%
  arrange(desc(cv))


