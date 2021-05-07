Arquivo das questão sobre peixes_rio
peixes_rio <- read_csv("avaliacao/arquivos/brutos/peixes_rio_madeira.csv")


peixes_rio %>%
  count(ordem) %>%
  arrange(n)

peixes_rio <- read_csv("avaliacao/arquivos/brutos/peixes_rio_madeira.csv")


peixes_rio %>%
  select(bacia, ordem, peso_g) %>%
  filter(bacia == "Rio Guaporé") %>%
  group_by(ordem) %>%
  summarise(
    media_peso = mean(peso_g, na.rm = TRUE),
    desvio = sd(peso_g, na.rm = TRUE),
    cv = (desvio/media_peso)*100) %>%
   arrange(media_peso)



peixes_rio %>%
  group_by(peso_g)%>%
    mutate(
     sexo_recode = recode(
        sexo,
        "Fêmea" = "Fêmea",
        "fêmea" = "Fêmea",
        "Macho" = "Macho")) %>%
  count(sexo_recode) %>%
  summarise(
    sexo_recode = sexo_recode,
    frequência = n,
    porcentagem = (n/49800)*100,
    )%>%
  arrange(desc(peso_g))%>%
    filter(sexo_recode == "Macho" |sexo_recode == "Fêmea")







peixes_rio %>%
  group_by(sexo, habito_alimentar)%>%
    count(habito_alimentar)%>%
    summarise(
    habito_alimentar = habito_alimentar,
    frequencia = n,
    sexo = sexo
    )






























