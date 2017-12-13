library(tidyverse)
vaccins <- read_tsv("./data/BDDvaccins bio.tsv", col_types = cols(ID = col_character()))

library(tidytext)

vaccins <- vaccins %>%  as_tibble()

unigrams_probs <- vaccins %>% 
  unnest_tokens(word, "Tweet") %>% 
  count(word, sort = TRUE) %>% 
  mutate(p = n / sum(n))

library(widyr)

tidy_skipgrams <- vaccins %>% 
  unnest_tokens(ngram, Tweet, token = "ngrams", n = 8) %>% # jouer avec le paramètre n
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, ID, ngramID) %>%
  unnest_tokens(word, ngram)

skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigrams_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigrams_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

library(irlba)

pmi_svd <- irlba(pmi_matrix, 256, maxit = 1e3) # jouer avec la dimension de la matrice (256) 

word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

library(broom)

search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}

vaccins <- search_synonyms(word_vectors, word_vectors["vaccins",])
sanofi <- search_synonyms(word_vectors, word_vectors["sanofi",])
obligatoire <- search_synonyms(word_vectors, word_vectors["obligatoire",])
vaccins %>%
  mutate(selected = "vaccins") %>%
  bind_rows(sanofi %>%
              mutate(selected = "sanofi")) %>%
  group_by(selected) %>%
  top_n(15, similarity) %>%
  ungroup %>%
  mutate(token = reorder(token, similarity)) %>%
  ggplot(aes(token, similarity, fill = selected)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~selected, scales = "free") +
  coord_flip() +
  theme(strip.text=element_text(hjust=0, family="Roboto-Bold", size=12)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, title = "What word vectors are most similar to obligatoire or sanofi?",
       subtitle = "Based on the twitter corpus, calculated using counts and matrix factorization")


mystery_product <- word_vectors["vaccins",] - word_vectors["obligatoires",] + word_vectors["pasteur",]
search_synonyms(word_vectors, mystery_product)
