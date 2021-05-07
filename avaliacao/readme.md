Avaliação I
================
Tiago dos Santos Alves </br>
Introdução a Estatistica 2020.1

------------------------------------------------------------------------

***Questão 1***

**(a)**

``` r
peixes_rio <- read_csv("arquivos/brutos/peixes_rio_madeira.csv")

peixes_rio %>%
  count(ordem) %>%
  arrange(n)
```

    ## # A tibble: 12 x 2
    ##    ordem                  n
    ##    <chr>              <int>
    ##  1 Lepidosireniformes     2
    ##  2 Pleuronectiformes      2
    ##  3 Beloniformes           5
    ##  4 Não identificado      17
    ##  5 Myliobatiformes       41
    ##  6 Osteoglossiformes    433
    ##  7 Gymnotiformes        693
    ##  8 Acanthuriformes     1602
    ##  9 Cichliformes        1947
    ## 10 Clupeiformes        2821
    ## 11 Siluriformes       27451
    ## 12 Characiformes      64356

**(b)** Observando a tabela de freqência, pude perceber que a ordem de
peixe que mais aparece é a Characiformes tendo 64.356 observações.

**(c)** Não foram identificados na variável ordem 17 peixes.

***Questão 2***

``` r
peixes_rio <- read_csv("arquivos/brutos/peixes_rio_madeira.csv")

peixes_rio %>%
  select(bacia, ordem, peso_g) %>%
  filter(bacia == "Rio Guaporé") %>%
  group_by(ordem) %>% 
  summarise(
    media_peso = mean(peso_g, na.rm = TRUE),
    desvio = sd(peso_g, na.rm = TRUE),
    cv = (desvio/media_peso)*100) %>%
   arrange(media_peso)
```

    ## # A tibble: 7 x 4
    ##   ordem           media_peso desvio    cv
    ##   <chr>                <dbl>  <dbl> <dbl>
    ## 1 Characiformes         65.8  106.  162. 
    ## 2 Gymnotiformes         80.6   79.8  99.0
    ## 3 Siluriformes          92.3  206.  223. 
    ## 4 Cichliformes         179.   242.  136. 
    ## 5 Acanthuriformes      198    250.  126. 
    ## 6 Clupeiformes         300.   235.   78.2
    ## 7 Myliobatiformes      NaN     NA    NA

**(a)** O coeficiente de variação é a mais adequada, pois a media do
peso é diferente em cada ordem.

**(b)** De acordo com a tabela, a ordem que possui uma distribuição de
peso mais homogênea é a Clupeiformes.

***Questão 3***

``` r
peixes_rio %>%
      mutate(
      sexo_recode = recode(
        sexo,
        "Fêmea" = "Fêmea",
        "fêmea" = "Fêmea",
        "Macho" = "Macho",
)) %>%
   count(sexo_recode) %>%
  summarise(
    sexo_recode = sexo_recode,
    frequência = n,
    porcentagem = (n/49800)*100) %>%
  filter(sexo_recode == "Macho" |sexo_recode == "Fêmea")
```

    ## # A tibble: 2 x 3
    ##   sexo_recode frequência porcentagem
    ##   <chr>            <int>       <dbl>
    ## 1 Fêmea            28331        56.9
    ## 2 Macho            21469        43.1

**(a)** Realizando a diferença entre a porcentagem de fêmeas pela
porcentagem de macho, ou seja,

``` r
x <- 56.9
y <- 43.1
x-y
```

    ## [1] 13.8

Então, a quantidade de machos que deve ser acrescentada para ficar igual
a quantidade de fêmeas é de 13,8%.

**(b)**

``` r
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
```

    ## `summarise()` has grouped output by 'peso_g'. You can override using the `.groups` argument.

    ## # A tibble: 2,283 x 4
    ## # Groups:   peso_g [1,471]
    ##    peso_g sexo_recode frequência porcentagem
    ##     <dbl> <chr>            <int>       <dbl>
    ##  1  14600 Fêmea                1     0.00201
    ##  2  12405 Fêmea                1     0.00201
    ##  3  11600 Fêmea                1     0.00201
    ##  4  11230 Fêmea                1     0.00201
    ##  5  10770 Fêmea                1     0.00201
    ##  6  10545 Fêmea                1     0.00201
    ##  7  10030 Fêmea                1     0.00201
    ##  8   9696 Fêmea                1     0.00201
    ##  9   9200 Fêmea                1     0.00201
    ## 10   9110 Fêmea                1     0.00201
    ## # ... with 2,273 more rows

Observando esta nova tabela, na qual aparece o peso de cada peixe
organizado de forma decrescente, é possível identificar que o peixe com
o maior peso é Fêmea.

***Questão 4***

``` r
peixes_rio %>%
  group_by(sexo, habito_alimentar)%>%
    count(habito_alimentar)%>%
    summarise(
    habito_alimentar = habito_alimentar,
    frequencia = n,
    sexo = sexo
    )
```

    ## `summarise()` has grouped output by 'sexo'. You can override using the `.groups` argument.

    ## # A tibble: 16 x 3
    ## # Groups:   sexo [4]
    ##    sexo         habito_alimentar frequencia
    ##    <chr>        <chr>                 <int>
    ##  1 fêmea        Carnívoro                 3
    ##  2 Fêmea        Carnívoro             13900
    ##  3 Fêmea        Detritívoro            3588
    ##  4 Fêmea        Herbívoro              1483
    ##  5 Fêmea        Indeterminado           565
    ##  6 Fêmea        Onívoro                8792
    ##  7 Macho        Carnívoro              8552
    ##  8 Macho        Detritívoro            3546
    ##  9 Macho        Herbívoro              1504
    ## 10 Macho        Indeterminado           581
    ## 11 Macho        Onívoro                7286
    ## 12 Não coletado Carnívoro             11476
    ## 13 Não coletado Detritívoro           15196
    ## 14 Não coletado Herbívoro              2081
    ## 15 Não coletado Indeterminado          1298
    ## 16 Não coletado Onívoro               19519

Pela tabela, é possível observar que a quantidade de Machos carnívoros é
de 8552

***Questão 5***

``` r
cheque <- read_csv("arquivos/brutos/contracheque.csv")
```

    ## Warning: 2629 parsing failures.
    ##   row                col   expected              actual                               file
    ## 20914 data_de_publicacao date like  2018-05-11T08:53:36 'arquivos/brutos/contracheque.csv'
    ## 20915 data_de_publicacao date like  2018-05-11T08:53:36 'arquivos/brutos/contracheque.csv'
    ## 20916 data_de_publicacao date like  2018-05-11T08:53:36 'arquivos/brutos/contracheque.csv'
    ## 20917 data_de_publicacao date like  2018-05-11T08:53:36 'arquivos/brutos/contracheque.csv'
    ## 20918 data_de_publicacao date like  2018-05-11T08:53:36 'arquivos/brutos/contracheque.csv'
    ## ..... .................. .......... ................... ..................................
    ## See problems(...) for more details.

``` r
cheque %>%
  select(rendimento_liquido)%>%
  arrange(desc(rendimento_liquido))
```

    ## # A tibble: 161,767 x 1
    ##    rendimento_liquido
    ##                 <dbl>
    ##  1           7267672.
    ##  2           2874516.
    ##  3            683094.
    ##  4            660934.
    ##  5            511424.
    ##  6            477756.
    ##  7            422214.
    ##  8            421106.
    ##  9            398141.
    ## 10            389054.
    ## # ... with 161,757 more rows

Analisando a tabela, é possível perceber que os valores estão
organizados de forma decrescente, logo, o maior rendimento liquido é de
aproximadamente 7.267.672

***Questão 6***

``` r
cheque %>%
  select(rendimento_liquido)%>%
  filter(rendimento_liquido > 39293.32) %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1 37334

Assim, há 37.334 magistrados que recebem acima de 39.293,32 reais

**(a)**

``` r
cheque %>%
  select(rendimento_liquido)%>%
  filter(rendimento_liquido > 100000.00) %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  1136

Há 1136 magistrados que recebem acima de 100.000,00 reais

**(b)**

``` r
cheque %>%
  select(rendimento_liquido, tribunal) %>%
  group_by(tribunal) %>%
  summarise(
    media_rendimento = mean(rendimento_liquido, na.rm = TRUE),
    desvio = sd(rendimento_liquido, na.rm = TRUE),
    cv = (desvio/media_rendimento)*100) %>%
  arrange(desc(cv))
```

    ## # A tibble: 128 x 4
    ##    tribunal                                        media_rendimento desvio    cv
    ##    <chr>                                                      <dbl>  <dbl> <dbl>
    ##  1 Tribunal Regional do Trabalho da 7ª Região (CE)           54659. 3.89e5 713. 
    ##  2 Tribunal Regional do Trabalho da 5ª Região (BA)           40914. 9.82e4 240. 
    ##  3 Conselho Nacional de Justiça                               6333. 1.07e4 169. 
    ##  4 Tribunal Superior Eleitoral                                3738. 5.92e3 158. 
    ##  5 Tribunal Regional do Trabalho da 2ª Região (SP~           28545. 2.73e4  95.6
    ##  6 Tribunal Superior do Trabalho/ Conselho Superi~           75471. 7.04e4  93.3
    ##  7 Tribunal Regional do Trabalho da 12ª Região (S~           39566. 3.28e4  82.8
    ##  8 Tribunal Superior do Trabalho/Conselho Superio~            6450. 5.12e3  79.5
    ##  9 Tribunal Regional do Trabalho da 3ª Região                42668. 3.39e4  79.4
    ## 10 Tribunal Regional Eleitoral do Acre                       23039. 1.83e4  79.2
    ## # ... with 118 more rows

Assim, é possível perceber que o tribunal que tribunal que possui maior
variabilidade é o Tribunal Regonal do Trabalho da 7ª Região do (CE),
cheguei a esse resultando encontrando o coeficiente de variação que é
igual ao desvio padrão do rendimento liquido dividido pela média desse
rendimento e multiplicando por 100, como é possivel identificar no
código acima.

***Questão 7***

**(a)** De acordo com o gráfico, o turno que apresenta a maior mediana
das notas é a integral, pois, a linha dentro de cada caixa do gráfico é
a mediana daquele respectivo conjunto de dados, sendo assim, a maior
dentre as medianas apresentadas é a do turno integral.

**(b)**

``` r
cursos_prouni <- read_csv("arquivos/brutos/cursos_prouni.csv")

cursos_prouni %>%
  filter(turno == "Integral") %>%
  group_by(turno)%>%
    summarise(
    media_nota = mean(nota_integral_ampla, na.rm = TRUE),
    mediana_nota = median(nota_integral_ampla, na.rm = TRUE)) 
```

    ## # A tibble: 1 x 3
    ##   turno    media_nota mediana_nota
    ##   <chr>         <dbl>        <dbl>
    ## 1 Integral       663.         658.

Segundo os resultados encontrados atraves do código acima, a média do
turno integral é 663 e a mediana 658.

**(c)**

``` r
cursos_prouni %>%
  select(nota_integral_ampla, turno)%>%
  group_by(turno) %>%
  summarise(
    media_nota = mean(nota_integral_ampla, na.rm = TRUE),
    desvio = sd(nota_integral_ampla, na.rm = TRUE),
    cv = (desvio/media_nota)*100)%>%
  arrange(desc(cv))
```

    ## # A tibble: 5 x 4
    ##   turno             media_nota desvio    cv
    ##   <chr>                  <dbl>  <dbl> <dbl>
    ## 1 Curso a Distância       545.   53.2  9.77
    ## 2 Integral                663.   58.0  8.75
    ## 3 Matutino                609.   43.5  7.14
    ## 4 Noturno                 602.   41.2  6.85
    ## 5 Vespertino              622.   41.0  6.59

Quanto maior o Coeficiente de variação (cv) menor é a homogeneidade,
desta forma, o curso que possui uma menor homogeneidade na nota integral
ampla é o **Curso a Distância**

***Questão 8***

``` r
cursos_prouni%>%
  group_by(uf_busca)%>%
  count()%>%
  arrange(desc(n))
```

    ## # A tibble: 27 x 2
    ## # Groups:   uf_busca [27]
    ##    uf_busca     n
    ##    <chr>    <int>
    ##  1 SP       11533
    ##  2 MG        4175
    ##  3 PR        3918
    ##  4 RS        3060
    ##  5 BA        2505
    ##  6 SC        2195
    ##  7 RJ        1442
    ##  8 GO        1278
    ##  9 PA        1201
    ## 10 PE        1148
    ## # ... with 17 more rows

Segundo a tabela gerada pelo código acima, é possivel identificar que a
Bahia (BA) ocupa a quinta posição na frequencia absoluta da variável
uf\_busca disposta de forma decescente.

***Questão 9***

``` r
cursos_prouni%>%
  select(nome)%>%
  distinct()
```

    ## # A tibble: 296 x 1
    ##    nome                    
    ##    <chr>                   
    ##  1 Medicina                
    ##  2 Enfermagem              
    ##  3 Psicologia              
    ##  4 Engenharia de Computação
    ##  5 Educação Física         
    ##  6 Direito                 
    ##  7 Engenharia de Produção  
    ##  8 Fisioterapia            
    ##  9 Administração           
    ## 10 Engenharia Civil        
    ## # ... with 286 more rows

Segundo a tabela acima que apresenta os cursos distintos e possui 296
linhas, é possivel dizer que possui 296 cursos distintos na variável
nome
