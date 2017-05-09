library(tidyverse)

keolis <- read_csv(file = "./data/Corpus_Mobilité.csv", col_types = cols(
  .default = col_character(),
  id = col_character(),
  time = col_integer(),
  created_at = col_datetime(format = ""),
  possibly_sensitive = col_integer(),
  truncated = col_integer(),
  retweet_count = col_integer(),
  favorite_count = col_integer(),
  in_reply_to_status_id = col_double(),
  lat = col_double(),
  lng = col_double(),
  from_user_id = col_character(),
  from_user_verified = col_integer(),
  from_user_utcoffset = col_integer(),
  from_user_tweetcount = col_integer(),
  from_user_followercount = col_integer(),
  from_user_friendcount = col_integer(),
  from_user_favourites_count = col_integer(),
  from_user_listed = col_integer(),
  from_user_created_at = col_datetime(format = "")
))

glimpse(keolis)

toptwittos <- keolis %>% 
  group_by(from_user_id) %>% 
  summarise(twittos = head(from_user_name, 1), nombre = n()) %>%
  filter(nombre > 50) %>% 
  arrange(desc(nombre))  
  
top100 <- keolis %>% 
  group_by(from_user_id) %>% 
  summarise(twittos = head(from_user_name, 1), nombre = n()) %>%
  arrange(desc(nombre)) %>% 
  slice(1:100)

# library(MonetDBLite)
# library(DBI)

# con <- dbConnect(monetdblite(), "./monetdb/")
# monetdb.read.csv(con, files = "./data/Corpus_Mobilité.csv", tablename = "keolis")
# 
# tbl_keolis <- tbl(con, "keolis")
# 
# tbl_keolis %>% 
#   group_by(from_user_id) %>% 
#   summarise(twittos = head(from_user_name, 1), nombre = n()) %>%
#   arrange(desc(nombre)) %>% 
#   slice(1:100)

keolis <- keolis %>% 
  mutate(longueur = nchar(text))

keolis %>% 
  group_by(longueur) %>% 
  summarise(nombre = n()) %>% 
  ggplot(aes(x = longueur, y = nombre)) +
  geom_bar(stat = "identity")

keolis %>% 
  group_by(longueur) %>% 
  summarise(nombre = n()) %>% 
  arrange(desc(nombre))

keolis %>% 
  group_by(text) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

keolis %>% 
  filter(!stringr::str_detect(text, "Me in the fitting room to make sure I have full mobility")) %>% 
  group_by(longueur) %>% 
  summarise(nombre = n()) %>% 
  ggplot(aes(x = longueur, y = nombre)) +
  geom_bar(stat = "identity")

keolis %>% 
  sample_n(1000)

keolis %>% 
  sample_frac(0.01)


library(rex)

rex_mode()

maregex <- rex(
  start %or% space,
  ("e" %or% "E"), "ni",
  space %or% end
)

keolis %>% 
  filter(str_detect(text, maregex)) %>% 
  select(text)

keolis %>% 
  filter(str_detect(text, "@eni")) %>% 
  select(text)

library(lubridate)

keolis %>% 
  mutate(semaine = week(created_at)) %>% 
  filter(str_detect(text, "[kK]eolis")) %>% 
  group_by(semaine) %>% 
  summarise(n = n())


keolis %>% 
  mutate(semaine = week(created_at)) %>% 
  mutate(keolis = str_detect(text, "[kK]eolis")) %>% 
  mutate(transdev = str_detect(text, "[Tt]ransdev")) %>% 
  mutate(ratp = str_detect(text, "RATP")) %>% 
  gather(operateur, presence, keolis, transdev, ratp) %>% 
  select(semaine, operateur, presence) %>% 
  filter(presence) %>% 
  group_by(semaine, operateur) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = semaine, y = n)) +
  geom_line(aes(color = operateur))


keolis %>% 
  mutate(semaine = week(created_at)) %>% 
  mutate(keolis = str_detect(text, "[kK]eolis")) %>% 
  mutate(transdev = str_detect(text, "[Tt]ransdev")) %>% 
  mutate(ratp = str_detect(text, "RATP")) %>% 
  gather(operateur, presence, keolis, transdev, ratp) %>% 
  select(semaine, operateur, presence) %>% 
  filter(presence) %>% 
  group_by(semaine, operateur) %>% 
  summarise(n = n()) %>% 
  spread(operateur, n)

library(tidytext)
tokens <- keolis %>% 
  select(id, text) %>% 
  unnest_tokens(mots, text)

data("stop_words")

tokens %>% 
  anti_join(stop_words, by = c("mots" = "word")) %>% 
  count(mots, sort = TRUE)

bigrammes <- keolis %>% 
  select(id, text) %>% 
  unnest_tokens(mots, text, token = "ngrams", n = 2)

bigrammes %>% 
  count(mots, sort = TRUE)

library(widyr)

tokens %>% 
  pairwise_count(mots, id, sort = TRUE)
  
