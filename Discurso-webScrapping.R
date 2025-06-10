# Text mining -------------------------------------------------------------

## Discurso de Posse do Presidente Lula ----

## PAcotes 
library(rvest)
library(httr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(igraph)
library(ggraph)
library(echarts4r)
library(RColorBrewer)

### Pagina que faremos o web scrapping
pagina<-"https://www.gov.br/planalto/pt-br/acompanhe-o-planalto/discursos-e-pronunciamentos/2023/discurso-do-presidente-lula-no-congresso-nacional"

### Obter a pagina web
acesso<-GET(pagina)

acesso

### Selecionar o texto para mining
(discurso<-acesso %>% read_html() %>%
    html_nodes(xpath='//*[@id="content-core"]')%>% ## acessando marcador do texto
    html_text()) 

## Construindo o banco de dados 
discursos_df <- data_frame(
  id_discurso = 1:length(discurso), 
  text = discurso)
glimpse(discursos_df)

## Transformando em Tokens
discursos_token <- discursos_df %>%
  unnest_tokens(word, text)
glimpse(discursos_token)

## Retirando palavras
stopwords_pt <- c(stopwords("pt"), "senhores",
                  "senhoras","é","–")

(stopwords_pt_df <- data.frame(word = stopwords_pt))

## Removendo palavras irrelevantes
(discursos_token <- discursos_token %>%
    anti_join(stopwords_pt_df, by = "word"))

## Ordenando palavras mais repetidas
discursos_token %>%
  count(word, sort = TRUE)

## Organizando e fazendo o grafico palavras mais repetidas
discursos_token %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n,fill=n)) +
  geom_col() +
  scale_fill_gradient(low="blue",high="darkblue")+
  guides(fill="none")+
  xlab(NULL) + ylab("Frequencia")+
  coord_flip()+ 
  theme_classic()

## Bigrams - palavras associadas duas a duas
discurso_bigrams <- discursos_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

discurso_bigrams %>%
  count(bigram, sort = TRUE)

## Bigrams - palavras separadas em colunas
bigrams_separated <- discurso_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

## Bigrams - retirando palavras irrelevantes
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopwords_pt) %>%
  filter(!word2 %in% stopwords_pt)

(bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE))

## Unindo as palavras apos a retirada das palavras irrelevantes  
(bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " "))

## Filtrando palavras
bigrams_filtered %>%
  filter(word1 == "crise") %>%
  count(word2, sort = TRUE)

bigrams_filtered %>%
  filter(word2 == "crise") %>%
  count(word1, sort = TRUE)

# Grafo de palavras -------------------------------------------------------

## Organizando bigrans que mais ocorreram 
bigram_graph <- bigram_counts %>%
  filter(n > 1) %>%
  graph_from_data_frame()
bigram_graph

## Criando um grafo ligando as palavras
## Tipo de seta e tamanho
a <- grid::arrow(type = "closed", 
                 length = unit(.3, "cm"))

## Criando o grafo
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = a, 
                 end_cap = circle(.2, 'cm')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), 
                 vjust = 1, hjust = 1) +
  theme_void()


## 10 palavras mais faladas
palavras<-discursos_token %>%
  count(word, sort = TRUE) %>%
  head(10)

## Criando Grafico de barras
palavras %>%
  e_charts(word) %>%
  e_bar(n, name="Palavras",label=list(show=TRUE)) %>%
  e_flip_coords()%>%
  e_grid(left = "20%")%>%
  e_toolbox_feature(
    feature = "saveAsImage",
    title = "Download")

## 50 palavras mais faladas
palavras2<-discursos_token %>%
  count(word, sort = TRUE) %>%
  head(50)

### Tons de azul
cores <- colorRampPalette(brewer.pal(9, "Blues"))(50)

## Nuvem de palavras
palavras2 %>% 
  e_color_range(n, color,colors= cores) %>%
  e_charts() %>% 
  e_cloud(word, n, color,
          shape = "circle", 
          rotationRange = c(0, 0),
          sizeRange = c(8, 110)) %>%
  e_tooltip() %>% 
  e_title("Nuvem de Palavras",
          "Discurso de posse do presidente Lula - 2023")%>%
  e_toolbox_feature(
    feature = "saveAsImage",
    title = "Download")

