library(tidyverse)
smartcity <- read_csv("./data/tcat_TREND_smartcity-20000508-20170510---------fullExport--3c72eed1c1.csv", 
                      col_types = cols(
  .default = col_character(),
  id = col_character(),
  time = col_integer(),
  created_at = col_datetime(format = ""), # un commentaire
  possibly_sensitive = col_integer(),
  truncated = col_integer(),
  retweet_count = col_integer(),
  favorite_count = col_integer(),
  in_reply_to_status_id = col_character(),
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

# filtrer selon la langue
smartcity %>% 
  filter(lang == "fr")

# utiliser l'opérateur %in% plutôt que l'opérateur ==
smartcity %>% 
  filter(lang %in% c("fr", "it"))

# ajouter une nouvelle variable indiquant si un tweet est en français ou pas

smartcity %>% 
  mutate(français = lang %in% "fr") %>% 
  glimpse

keolis %>% 
  mutate(lang2 = recode(lang,
                        "en" = "anglais",
                        "fr" = "français")) %>% 
  mutate(langue = case_when(lang %in% "fr" & from_user_lang %in% "fr" ~ "français",
                            lang %in% "en" & from_user_lang %in% "en-GB" ~ "britannique",
                            lang %in% "en" & from_user_lang %in% "en" ~ "ricain",
                            lang %in% "it" | from_user_lang %in% "it" ~ "italien",
                            !is.na(lang) ~ NA_character_)) %>% 
  glimpse


# au moins un tweet du twittos en anglais
smartcity %>% 
  group_by(from_user_id) %>% 
  summarise(tweet_en_anglais = any(lang %in% "en"))

# tous les tweets du twittos en anglais
smartcity %>% 
  group_by(from_user_id) %>% 
  summarise(tweet_en_anglais = all(lang %in% "en"))

# au moins un tweet du twittos en anglais
smartcity %>% 
  group_by(from_user_id) %>% 
  mutate(tweet_en_anglais = any(lang %in% "en")) %>% 
  filter(tweet_en_anglais)


### Graphiques

library(lubridate)
smartcity %>%
  mutate(jour = yday(created_at)) %>% 
  ggplot(aes(x = jour)) +
  geom_bar(stat = "count")

smartcity %>% 
  mutate(jour = yday(created_at)) %>% 
  group_by(jour) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = jour, y = n)) +
  geom_bar(stat = "identity")

smartcity %>% 
  mutate(jour = yday(created_at)) %>% 
  group_by(jour, lang) %>%
  summarise(n = n()) %>% 
  group_by(lang) %>% 
  filter(sum(n) > 100) %>% 
  ggplot(aes(x = jour, y = n)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~ lang)

smartcity %>%
  mutate(jour = yday(created_at)) %>% 
  ggplot(aes(x = jour)) +
  geom_line(stat = "count")

library(hrbrthemes)

png(file = "./graphique1.png", width = 800, height = 800, title = "Mon graphique")
smartcity %>% 
  mutate(jour = yday(created_at)) %>% 
  group_by(jour, lang) %>%
  summarise(n = n()) %>% 
  group_by(lang) %>% 
  filter(sum(n) > 2000) %>% 
  ggplot(aes(x = jour, y = n)) +
  geom_line(aes(color = lang, lty = lang)) + 
  scale_y_log10() +
  scale_color_grey() +
  theme_ipsum() +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "./graphique1.pdf", width = 10, height = 10, title = "Mon graphique")
smartcity %>% 
  mutate(jour = yday(created_at)) %>% 
  group_by(jour, lang) %>%
  summarise(n = n()) %>% 
  group_by(lang) %>% 
  filter(sum(n) > 2000) %>% 
  ggplot(aes(x = jour, y = n)) +
  geom_line(aes(color = lang, lty = lang)) + 
  scale_y_log10() +
  scale_color_grey() +
  theme(legend.position = "bottom")
dev.off()

library(svglite)

svglite("./graphique1.svg")
smartcity %>% 
  mutate(jour = yday(created_at)) %>% 
  group_by(jour, lang) %>%
  summarise(n = n()) %>% 
  group_by(lang) %>% 
  filter(sum(n) > 2000) %>% 
  ggplot(aes(x = jour, y = n)) +
  geom_line(aes(color = lang, lty = lang)) + 
  scale_y_log10() +
  scale_color_grey() +
  theme(legend.position = "bottom")
dev.off()


cairo_pdf("./graphique1cairo.pdf")
smartcity %>% 
  mutate(jour = yday(created_at)) %>% 
  group_by(jour, lang) %>%
  summarise(n = n()) %>% 
  group_by(lang) %>% 
  filter(sum(n) > 2000) %>% 
  ggplot(aes(x = jour, y = n)) +
  geom_line(aes(color = lang, lty = lang)) + 
  scale_y_log10() +
  scale_color_grey() +
  theme_ipsum() +
  theme(legend.position = "bottom")
dev.off()

library(forcats)
smartcity %>% 
  count(from_user_timezone) %>% 
  filter(n > 1000) %>% 
  arrange(desc(n)) %>%
  filter(!is.na(from_user_timezone)) %>% 
  mutate(from_user_timezone = fct_inorder(from_user_timezone)) %>% 
  ggplot(aes(x = from_user_timezone, y = n)) +
  geom_bar(stat = "identity")

smartcity %>% 
  count(from_user_timezone) %>% 
  filter(n > 1000) %>% 
  arrange(n) %>%
  filter(!is.na(from_user_timezone)) %>% 
  mutate(from_user_timezone = fct_inorder(from_user_timezone)) %>% 
  ggplot(aes(x = from_user_timezone, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

smartcity %>% 
  count(from_user_timezone) %>% 
  filter(n > 1000) %>% 
  arrange(n) %>%
  filter(!is.na(from_user_timezone)) %>% 
  mutate(from_user_timezone = fct_inorder(from_user_timezone)) %>% 
  ggplot(aes(x = from_user_timezone, y = n)) +
  geom_bar(stat = "identity", fill = "#0000cc") +
  geom_text(aes(label = from_user_timezone), hjust = 1.1, color = "white") +
  coord_flip() +
  theme_ipsum(grid = "X") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y = "Nombre de tweets", x = "") 

keolis %>% 
  count(from_user_timezone) %>% 
  filter(n > 1000) %>% 
  arrange(n) %>%
  filter(!is.na(from_user_timezone)) %>% 
  mutate(from_user_timezone = fct_inorder(from_user_timezone)) %>% 
  ggplot(aes(x = from_user_timezone, y = n)) +
  geom_bar(stat = "identity", fill = "#0000cc") +
  geom_text(aes(label = from_user_timezone), hjust = 1.1, color = "white") +
  coord_flip() +
  theme_ipsum(grid = "X") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y = "Nombre de tweets", x = "") 

bind_rows(keolis, smartcity, .id = "corpus") %>% 
  count(from_user_timezone, corpus) %>% 
  filter(n > 1000) %>% 
  arrange(n) %>%
  filter(!is.na(from_user_timezone)) %>% 
  mutate(from_user_timezone = fct_inorder(from_user_timezone)) %>%
  ggplot(aes(x = from_user_timezone, y = n)) +
  geom_bar(aes(fill = corpus), stat = "identity", position = "dodge") +
  geom_text(aes(label = from_user_timezone, group = corpus), hjust = 1.1, color = "white", position = position_dodge(width = 1)) +
  coord_flip() +
  theme_ipsum(grid = "X") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y = "Nombre de tweets", x = "")

bind_rows(keolis, smartcity, .id = "corpus") %>% 
  count(from_user_timezone, corpus) %>% 
  filter(n > 1000) %>% 
  arrange(n) %>%
  filter(!is.na(from_user_timezone)) %>% 
  mutate(from_user_timezone = fct_inorder(from_user_timezone)) %>%
  ggplot(aes(x = from_user_timezone, y = n)) +
  geom_bar(fill = "#0000cc", stat = "identity") +
  geom_text(aes(label = from_user_timezone), hjust = 1.1, color = "white") +
  facet_wrap(~ corpus) +
  coord_flip() +
  theme_ipsum(grid = "X") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y = "Nombre de tweets", x = "")

bind_rows(keolis, smartcity, .id = "corpus") %>% 
  count(from_user_timezone, corpus) %>% 
  filter(n > 1000) %>% 
  arrange(n) %>%
  filter(!is.na(from_user_timezone)) %>% 
  mutate(from_user_timezone = fct_inorder(from_user_timezone)) %>%
  ggplot(aes(x = from_user_timezone, y = n)) +
  geom_bar(fill = "#0000cc", stat = "identity") +
  geom_text(aes(label = from_user_timezone), hjust = 1.1, color = "white") +
  facet_wrap(~ corpus, scales = "free_x") +
  coord_flip() +
  theme_ipsum(grid = "X") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y = "Nombre de tweets", x = "")

smartcity %>% 
  ggplot(aes(x = created_at)) +
  geom_density()


smartcity %>% 
  filter(from_user_followercount > 0, from_user_friendcount > 0) %>% 
  ggplot(aes(x = from_user_followercount, y = from_user_friendcount)) +
  geom_point(alpha = 0.02) +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10()

p <- smartcity %>% 
  filter(from_user_followercount > 0, from_user_friendcount > 0) %>% 
  ggplot(aes(x = from_user_followercount, y = from_user_friendcount)) +
  geom_point(aes(size = from_user_tweetcount), alpha = 0.02) +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10()

