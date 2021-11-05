install.packages("rtweet")
library(rtweet)

?search_tweets

tw_covid19 <- search_tweets(q = "covid19", n = 10000)

tw %>% View()

#get_timeline
usuario = "RafaelNadal"

rt_nadal <- get_timeline(user=usuario, n=100)
#get_followers
usuario = "rnadalacademy"

follows <- get_followers(user=usuario, n=75000)
#get_friends

fr_nadal <- get_friends(users = usuario, n=205)
#get_favorites

fv_nadal <- get_favorites(user = usuario, n = 1000)

fv_nadal %>% View()
#users_data

u_nadal <- users_data(tweets = fv_nadal)
u_nadal %>% View()

#Analisis de datos con R desde Twitter Covid19
library(tidyverse)
tw_covid19 %>% glimpse()

tw_c19_es <- tw_covid19 %>%
  filter(lang == "es")
tw_c19_es

tw_c19_es %>%
  group_by(screen_name) %>%
  summarise(n = n())
tw_c19_es %>%
  count(screen_name) %>%
  arrange(desc(n))
#ordenar
dat <- fv_nadal%>% 
  count(screen_name) %>%
  arrange(desc(n)) %>%
  head(5)

dat
#mappgin
p <- ggplot(data = dat,
            mapping = aes(x=reorder(screen_name,n), y = n))
#geom
p <- p + geom_point()
#bar
p <- p + geom_bar(stat = "identity",
                  fill= "red3")
#labels
p <- p + labs(title = "usuarios que le gustan a rafa",
              y="# 'me gusta'",
              x="usuarios")
#dibujar
p <- p + theme_classic()
print(p)
#Analisis de datos

install.packages("lubridate")
library(lubridate)
library(tidyverse)
# #de tw por año
twPorAnio <- fv_nadal %>%
  mutate(fecha = ymd_hms(created_at)) %>%
  mutate(fecha = format(fecha, "%Y")) %>%
  select(created_at, fecha) %>%
  count(fecha)

p <- ggplot(data = twPorAnio,
            mapping = aes(x = fecha,
                          y=n))
p <- p + geom_point()

p <- p + labs(title = "favoritos por año")
p


########hastags
install.packages("tidyr")
install.packages("tidytext")
tw_futbol <- search_tweets("futbol", n = 18000,
                           lang = "es",
                           include_rts = FALSE) 

library(tidyr)
library(tidytext)
library(tidyverse)
#
tw_hashtags_n <- tw_futbol %>% 
  unnest(hashtags) %>% #separa en lineas diferentes
  filter(!is.na(hashtags)) %>%
  mutate(hashtags = toupper(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n)) %>%
  head(20)

#tw_futbol %>% View()
#aux <- tw_futbol %>% filter()

#Dibujar hashtags

p <- ggplot(data = tw_hashtags_n,
            mapping = aes(x=reorder(hashtags,n),
                          y=n))
p <- p + geom_bar(stat = "identity")

p <- p + coord_flip()
p <- p + labs(title = "hashtags mas utilizados",
              x = "hashtags",
              y="# menciones")
p
#wordcloud

install.packages("wordcloud")
library(wordcloud)
tw_hashtags_word <- tw_futbol %>% 
  unnest(hashtags) %>% #separa en lineas diferentes
  filter(!is.na(hashtags)) %>%
  mutate(hashtags = toupper(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n)) %>%
  head(100)
wordcloud(word = tw_hashtags_word$hashtags,
          freq = tw_hashtags_word$n,
          colors = brewer.pal(9,"RdBu"))
#perfil de los usuarios
library(lubridate)
us_data_anio <- u_nadal %>%
  distinct(user_id,account_created_at) %>%
  select(account_created_at) %>%
  mutate(anio = account_created_at %>% 
           format("%Y")) %>%
  select(account_created_at, anio) %>%
  count(anio) %>%
arrange(desc(n))
#dibujar

p <- ggplot(data = us_data_anio,
            mapping = aes(x=anio,
                          y= n))
p <- p + geom_bar(stat = "identity")
+ labs(title = "Cuentas creadas por anio",
       x = "anio",
       y = "# cuentas")
p

#seguidores y amigos
install.packages("VennDiagram")
library(VennDiagram)
library(rtweet)
usuario = "rogerFederer"

fr_federer <- get_friends(user = usuario, n = 100)

vennplot <- venn.diagram(
  x=list(nadal=fr_nadal$user_id,
         federer=fr_federer$user_id),
  filename = "amigos_fed_nad.png",
  fill = c("red3","orange"),
  alpha = 0.50,
  cex = 2.5)
getwd()
