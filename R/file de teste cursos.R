
cursos_prouni <- read_csv("avaliacao/arquivos/brutos/cursos_prouni.csv")


cursos_prouni %>%
    group_by(turno) %>%
    summarise(
    media_nota = mean(nota_integral_ampla, na.rm = TRUE),
    mediana_nota = median(nota_integral_ampla, na.rm = TRUE))


cursos_prouni %>%
  select(nota_integral_ampla, turno)%>%
  group_by(turno) %>%
  summarise(
    media_nota = mean(nota_integral_ampla, na.rm = TRUE),
    desvio = sd(nota_integral_ampla, na.rm = TRUE),
    cv = (desvio/media_nota)*100)%>%
  arrange(desc(cv))

cursos_prouni%>%
  group_by(uf_busca)%>%
  count()%>%
  arrange(desc(n))

cursos_prouni%>%
  select(nome)%>%
  distinct()

