library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(textdata)
library(SentimentAnalysis)
library(stringr)
library(SnowballC)
library(cluster)
library(DT)
library(topicmodels)

set.seed(123)

txt_files <- list.files(pattern = "\\.txt$", full.names = TRUE)
if (length(txt_files) == 0) stop("Brak plikow .txt w katalogu roboczym.")

extract_field <- function(lines, field) {
  hit <- grep(paste0("^", field, ":"), lines, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) return(NA_character_)
  str_trim(sub(paste0("^", field, ":"), "", hit[1], ignore.case = TRUE))
}

read_song <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  title <- extract_field(lines, "Title")
  artist <- extract_field(lines, "Artist")
  year_raw <- extract_field(lines, "Year")
  place_raw <- extract_field(lines, "Place")
  genre <- extract_field(lines, "Genre")

  sep_idx <- grep("----", lines)[1]
  if (is.na(sep_idx)) {
    lyrics <- lines
  } else {
    lyrics <- lines[(sep_idx + 1):length(lines)]
  }

  lyrics <- lyrics[nchar(str_trim(lyrics)) > 0]
  lyrics_text <- paste(lyrics, collapse = " ")

  tibble(
    file = basename(path),
    title = title,
    artist = artist,
    year = suppressWarnings(as.integer(str_extract(year_raw, "\\d{4}"))),
    place = suppressWarnings(as.integer(str_extract(place_raw, "\\d+"))),
    genre = genre,
    text = lyrics_text
  )
}

songs_raw <- map_dfr(txt_files, read_song)

songs <- songs_raw %>%
  filter(!is.na(year), !is.na(text), nchar(str_trim(text)) > 0) %>%
  mutate(
    song_id = row_number(),
    title = if_else(is.na(title) | title == "", file, title)
  )

if (nrow(songs) < 3) stop("Za malo poprawnych dokumentow do analiz klastrowania (minimum 3).")

custom_stopwords <- tibble(
  word = c(
    stopwords("en"),
    "na", "la", "oh", "ooh", "yeah", "hey", "uh", "woo", "wanna",
    "title", "artist", "year", "place", "genre"
  )
) %>% distinct()

tokens_clean <- songs %>%
  select(song_id, year, title, artist, text) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_replace_all(word, "[^a-z']", "")) %>%
  filter(str_detect(word, "[a-z]")) %>%
  anti_join(custom_stopwords, by = "word") %>%
  filter(nchar(word) > 2)

tokens_stemmed <- tokens_clean %>%
  mutate(stem = wordStem(word, language = "en"))

freq_all <- tokens_stemmed %>%
  count(stem, sort = TRUE)

png("wordcloud_global.png", width = 800, height = 600)
wordcloud(
  words = freq_all$stem,
  freq = freq_all$n,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.15,
  colors = brewer.pal(8, "Dark2")
)
dev.off()

freq_year <- tokens_stemmed %>%
  count(year, stem, sort = TRUE)

for (yy in sort(unique(freq_year$year))) {
  freq_y <- freq_year %>% filter(year == yy)
  if (nrow(freq_y) > 1) {
    png(paste0("wordcloud_", yy, ".png"), width = 800, height = 600)
    wordcloud(
      words = freq_y$stem,
      freq = freq_y$n,
      min.freq = 2,
      max.words = 120,
      random.order = FALSE,
      rot.per = 0.15,
      colors = brewer.pal(8, "Set2")
    )
    title(paste("Wordcloud -", yy))
    dev.off()
  }
}

songs_stem_text <- tokens_stemmed %>%
  group_by(song_id, year, title, artist) %>%
  summarise(stem_text = paste(stem, collapse = " "), .groups = "drop")

sentiment_year <- songs %>%
  transmute(year, sentiment_qdap = SentimentAnalysis::analyzeSentiment(text)$SentimentQDAP) %>%
  group_by(year) %>%
  summarise(avg_sentiment_qdap = mean(sentiment_qdap, na.rm = TRUE), .groups = "drop")

bing_sentiment_year <- tokens_clean %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    bing_net = positive - negative,
    bing_index = (positive - negative) / pmax(positive + negative, 1)
  ) %>%
  select(year, bing_net, bing_index)

sentiment_by_year <- sentiment_year %>%
  left_join(bing_sentiment_year, by = "year") %>%
  arrange(year)

png("sentyment_qdap.png", width = 800, height = 600)
print(
  ggplot(sentiment_by_year, aes(x = year, y = avg_sentiment_qdap)) +
    geom_line(color = "#2c7fb8", linewidth = 1) +
    geom_point(color = "#2c7fb8", size = 2) +
    labs(
      title = "Sentyment roczny (SentimentAnalysis - QDAP)",
      x = "Rok",
      y = "Sredni sentyment"
    ) +
    theme_minimal()
)
dev.off()

png("sentyment_bing.png", width = 800, height = 600)
print(
  ggplot(sentiment_by_year, aes(x = year, y = bing_index)) +
    geom_line(color = "#d95f0e", linewidth = 1) +
    geom_point(color = "#d95f0e", size = 2) +
    labs(
      title = "Sentyment roczny (Bing)",
      x = "Rok",
      y = "Wskaznik sentymentu"
    ) +
    theme_minimal()
)
dev.off()

corp <- VCorpus(VectorSource(songs_stem_text$stem_text))
dtm <- DocumentTermMatrix(corp, control = list(wordLengths = c(3, Inf)))
dtm <- removeSparseTerms(dtm, sparse = 0.98)
dtm_m <- as.matrix(dtm)

if (ncol(dtm_m) < 2) stop("Za malo cech po czyszczeniu DTM. Zmien prog removeSparseTerms.")

dtm_scaled <- scale(dtm_m)
rownames(dtm_scaled) <- songs_stem_text$title

choose_k_silhouette <- function(x, k_min = 2, k_max = 10, nstart = 25) {
  n_docs <- nrow(x)
  k_max_eff <- min(k_max, n_docs - 1)
  if (k_max_eff < k_min) stop("Za malo dokumentow do doboru k.")
  dist_mat <- dist(x)
  k_grid <- seq.int(k_min, k_max_eff)

  sil_tbl <- map_dfr(k_grid, function(k) {
    km <- kmeans(x, centers = k, nstart = nstart, iter.max = 200)
    sil <- silhouette(km$cluster, dist_mat)
    tibble(k = k, silhouette = mean(sil[, 3]))
  })

  best_k <- sil_tbl$k[which.max(sil_tbl$silhouette)]
  list(best_k = best_k, sil_tbl = sil_tbl)
}

k_eval_bow <- choose_k_silhouette(dtm_scaled, k_min = 2, k_max = 10)
best_k_bow <- k_eval_bow$best_k
km_bow <- kmeans(dtm_scaled, centers = best_k_bow, nstart = 50, iter.max = 300)

png("silhouette_bow.png", width = 800, height = 600)
print(
  ggplot(k_eval_bow$sil_tbl, aes(k, silhouette)) +
    geom_line(color = "#1b9e77", linewidth = 1) +
    geom_point(color = "#1b9e77", size = 2) +
    scale_x_continuous(breaks = k_eval_bow$sil_tbl$k) +
    labs(
      title = "Dobor k (BoW) - silhouette",
      x = "k",
      y = "Srednia silhouette"
    ) +
    theme_minimal()
)
dev.off()

bow_clusters <- tibble(
  title = songs_stem_text$title,
  artist = songs_stem_text$artist,
  year = songs_stem_text$year,
  cluster_bow = km_bow$cluster
)

pca_bow <- prcomp(dtm_scaled)
pca_bow_df <- tibble(
  title = songs_stem_text$title,
  cluster = factor(km_bow$cluster),
  PC1 = pca_bow$x[, 1],
  PC2 = pca_bow$x[, 2]
)

png("pca_bow_kmeans.png", width = 900, height = 700)
print(
  ggplot(pca_bow_df, aes(PC1, PC2, color = cluster, label = title)) +
    geom_point(size = 3, alpha = 0.85) +
    geom_text(size = 2, alpha = 0.6, vjust = -0.5) +
    labs(
      title = paste("K-means (BoW), k =", best_k_bow, "- PCA (PC1 vs PC2)"),
      x = paste("PC1 (", round(summary(pca_bow)$importance[2, 1] * 100, 1), "%)"),
      y = paste("PC2 (", round(summary(pca_bow)$importance[2, 2] * 100, 1), "%)"),
      color = "Klaster"
    ) +
    theme_minimal()
)
dev.off()


dtm_tfidf <- weightTfIdf(dtm)
tfidf_m <- as.matrix(dtm_tfidf)
tfidf_scaled <- scale(tfidf_m)
rownames(tfidf_scaled) <- songs_stem_text$title

k_eval_tfidf <- choose_k_silhouette(tfidf_scaled, k_min = 2, k_max = 10)
best_k_tfidf <- k_eval_tfidf$best_k
km_tfidf <- kmeans(tfidf_scaled, centers = best_k_tfidf, nstart = 50, iter.max = 300)

png("silhouette_tfidf.png", width = 800, height = 600)
print(
  ggplot(k_eval_tfidf$sil_tbl, aes(k, silhouette)) +
    geom_line(color = "#7570b3", linewidth = 1) +
    geom_point(color = "#7570b3", size = 2) +
    scale_x_continuous(breaks = k_eval_tfidf$sil_tbl$k) +
    labs(
      title = "Dobor k (TF-IDF) - silhouette",
      x = "k",
      y = "Srednia silhouette"
    ) +
    theme_minimal()
)
dev.off()

tfidf_clusters <- tibble(
  title = songs_stem_text$title,
  artist = songs_stem_text$artist,
  year = songs_stem_text$year,
  cluster_tfidf = km_tfidf$cluster
)

pca_tfidf <- prcomp(tfidf_scaled)
pca_tfidf_df <- tibble(
  title = songs_stem_text$title,
  cluster = factor(km_tfidf$cluster),
  PC1 = pca_tfidf$x[, 1],
  PC2 = pca_tfidf$x[, 2]
)

png("pca_tfidf_kmeans.png", width = 900, height = 700)
print(
  ggplot(pca_tfidf_df, aes(PC1, PC2, color = cluster, label = title)) +
    geom_point(size = 3, alpha = 0.85) +
    geom_text(size = 2, alpha = 0.6, vjust = -0.5) +
    labs(
      title = paste("K-means (TF-IDF), k =", best_k_tfidf, "- PCA (PC1 vs PC2)"),
      x = paste("PC1 (", round(summary(pca_tfidf)$importance[2, 1] * 100, 1), "%)"),
      y = paste("PC2 (", round(summary(pca_tfidf)$importance[2, 2] * 100, 1), "%)"),
      color = "Klaster"
    ) +
    theme_minimal()
)
dev.off()


freq_all_top100 <- arrange(freq_all, desc(n)) %>% slice_head(n = 100)
message("Top 100 najczesciej stosowanych stemow:")
print(freq_all_top100)

message("\nZagrupowanie w klastrach (BoW):")
print(arrange(bow_clusters, cluster_bow, year, title))

message("\nZagrupowanie w klastrach (TF-IDF):")
print(arrange(tfidf_clusters, cluster_tfidf, year, title))

message("\nSentyment roczny:")
print(sentiment_by_year)

message("Analiza zakonczona.")
message("Optymalne k (BoW): ", best_k_bow)
message("Optymalne k (TF-IDF): ", best_k_tfidf)
