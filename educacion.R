# CHARLA EN EDUCACIÓN # 
#### sobre la base de datos Medium Articles de Kaggle ####
# https://www.kaggle.com/hsankesara/medium-articles #
library(tidyverse)
library(readr)
library(tidytext)

#después de haber descargado el archivo, lo importo a R
articles <- read_csv("articles.csv")

#descomposición en palabras
articles_palabras <- articles %>%
  unnest_tokens(palabras, text)%>%
  anti_join(stop_words, by=c("palabras"="word"))%>%
  mutate(claps= gsub("K", "000", claps),
         claps= gsub("\\.", "", claps),
         claps= as.numeric(claps))

#conteo de palabras más frecuentes
count_palabras <- articles_palabras %>% 
  count(palabras, sort = TRUE)

#graficación de las 20 más frecuentes
count_palabras %>%
  top_n(20, n)%>%
  ggplot(aes(fct_reorder(palabras, n), n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#graficación con variable aplausos
claps_palabras <- articles_palabras %>% 
  count(claps, palabras, sort = TRUE)%>%
  group_by(palabras)%>%
  summarise(total=sum(claps))%>%
  arrange(desc(total))

count_palabras %>%
  top_n(20, n)%>%
  inner_join(claps_palabras, by="palabras")%>%
  ggplot(aes(fct_reorder(palabras, n), n, fill=total))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#relacion de aplausos con tiempo lectura
articles %>%
  select (claps, reading_time)%>%
  mutate(claps= gsub("K", "000", claps),
         claps= gsub("\\.", "", claps),
         claps= as.numeric(claps))%>%
  ggplot(aes(reading_time, claps)) +
  geom_point(alpha=0.3)+
  geom_smooth()+
  xlab("tiempo de lectura")

#relación de aplausos con longitud del título
articles_length_title <- articles %>%
  unnest_tokens(palabras, title, drop = FALSE)%>%
  group_by(title)%>%
  summarise(palabras, long=n())%>%
  select (title, long)%>%
  distinct()

articles %>%
  inner_join(articles_length_title, by="title")%>%
  mutate(claps= gsub("K", "000", claps),
         claps= gsub("\\.", "", claps),
         claps= as.numeric(claps))%>%
  ggplot(aes(long, claps))+
  geom_point(alpha=0.3)+
  geom_smooth()+
  xlab("longitud del título")

#comparativa sentimientos
#obtener los sentimientos, en este caso AFINN
NRC_sent <- get_sentiments("afinn")

#vincular palabras y sentimientos
articles_palabras %>%
  inner_join(NRC_sent, by=c("palabras"="word"))%>%
  group_by(title)%>%
  summarise(claps, sentiment=sum(value))%>%
  distinct()%>%
  ggplot(aes(sentiment, claps))+
  geom_point(alpha=0.4)+
  geom_smooth()

#añadir variable tiempo de lectura
articles_palabras %>%
  inner_join(NRC_sent, by=c("palabras"="word"))%>%
  group_by(title)%>%
  summarise(reading_time, claps, sentiment=sum(value))%>%
  distinct()%>%
  ggplot(aes(sentiment, claps))+
  geom_point(aes(size= reading_time), alpha=0.4)+
  geom_smooth()

#palabras de títulos con más claps
titles_claps <- articles %>%
  unnest_tokens(palabras, title, drop = FALSE)%>%
  group_by(title)%>%
  ungroup()%>%
  anti_join(stop_words, by=c("palabras"="word"))%>%
  count(palabras, sort = TRUE)%>%
  top_n(10, n) 

#graficar palabras de títulos con más claps
titles_claps %>%
  ggplot(aes(fct_reorder(palabras, n), n))+
  geom_col()+
  coord_flip()+
  xlab(NULL)

#cálculo LDA
library(topicmodels)

#contar palabras conservando título
count_palabras_title <- articles_palabras %>% 
  count(title, palabras, sort = TRUE)

#transformación a DTM
corpus_dtm <- count_palabras_title %>%
  cast_dtm(title, palabras, n)

#precálculo de perplejidad
mod_log_lik = numeric(12)
mod_perplexity = numeric(12)
for (i in 2:12) {
  mod = LDA(corpus_dtm, k=i, method="Gibbs",
            control=list(alpha=0.5, iter=1000, seed=12345, thin=1))
  mod_log_lik[i] = logLik(mod)
  mod_perplexity[i] = perplexity(mod, corpus_dtm)
}

#graficación de perplejidad, resultado k=11
mod_perplexity
plot(mod_perplexity, xlab="Valores de k", ylab="Perplejidad", xlim = c(2, 12), ylim = c(2500, 3900))

#aplicación de LDA con k=11
corpus_lda <- LDA (corpus_dtm, k = 11, method="Gibbs", 
                   control =list(alpha = 1, seed=1234, thin=1))

#cálculo de beta
corpus_topics <- tidy(corpus_lda, matrix="beta")

#los 10 términos que más influyen a cada tópico
corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>% 
  top_n (10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

#graficación de términos por tópico
corpus_top_terms %>%
  mutate (palabras =reorder(term, beta)) %>% 
  filter(!palabras == "mas")%>%
  ggplot (aes(palabras, beta, fill = factor(topic))) + 
  geom_col (show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
