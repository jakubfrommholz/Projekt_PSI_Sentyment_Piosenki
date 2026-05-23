#' ---
#' title: "Analiza sentymentu piosenek z list Billboard"
#' author: "Magdalena Rychlewska, Jakub Frommholz"
#' date: "23.05.2026"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable
#'     highlight: kate
#'     toc: false
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false 
#' ---

# 1. Wczytanie bibliotek ----

library(tm)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(cluster)

set.seed(42)


# 2. Wczytanie danych ----

# Funkcja do ekstrakcji pola z nagłówka pliku
extract_field <- function(lines, field) {
  result <- grep(paste0("^", field, ":"), lines, ignore.case = TRUE, value = TRUE)
  if (length(result) == 0) return(NA_character_)
  value <- sub(paste0("^", field, ":"), "", result[1], ignore.case = TRUE)
  trimws(value)
}

# Funkcja do wczytania piosenki
read_song <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # Ekstrakcja metadanych
  title <- extract_field(lines, "Title")
  artist <- extract_field(lines, "Artist")
  year_raw <- extract_field(lines, "Year")
  place_raw <- extract_field(lines, "Place")
  genre <- extract_field(lines, "Genre")

  # Znalezienie separatora
  sep_idx <- grep("^'{0,1}-{4,}'{0,1}$", lines)[1]
  if (is.na(sep_idx)) {
    lyrics <- lines
  } else {
    lyrics <- lines[(sep_idx + 1):length(lines)]
  }

  # Usunięcie pustych linii
  lyrics <- lyrics[nchar(trimws(lyrics)) > 0]
  lyrics_text <- paste(lyrics, collapse = " ")

  data.frame(
    file = basename(path),
    title = title,
    artist = artist,
    year = as.integer(str_extract(year_raw, "\\d{4}")),
    place = as.integer(str_extract(place_raw, "\\d+")),
    genre = genre,
    text = lyrics_text,
    stringsAsFactors = FALSE
  )
}

# Wczytanie wszystkich piosenek
txt_files <- list.files(path = "data", pattern = "\\.txt$", full.names = TRUE)
songs_raw <- do.call(rbind, lapply(txt_files, read_song))
rownames(songs_raw) <- NULL

# Filtracja i przygotowanie danych
songs <- songs_raw[!is.na(songs_raw$year) & !is.na(songs_raw$text), ]
songs <- songs[songs$text != "", ]
songs$song_id <- seq_len(nrow(songs))
songs$title[is.na(songs$title) | songs$title == ""] <- songs$file[is.na(songs$title) | songs$title == ""]

cat("Wczytano", nrow(songs), "piosenek\n")


# 3. Tokenizacja i czyszczenie tekstu ----

# Definicja stopwords
custom_stopwords <- c(
  stopwords("en"),
  "na", "la", "oh", "ooh", "yeah", "hey", "uh", "woo", "wanna",
  "title", "artist", "year", "place", "genre"
)
custom_stopwords <- unique(custom_stopwords)

# Tokenizacja
tokens <- songs %>%
  select(song_id, year, title, artist, text) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_replace_all(word, "[^a-z']", "")) %>%
  filter(str_detect(word, "[a-z]")) %>%
  filter(!(word %in% custom_stopwords)) %>%
  filter(nchar(word) > 2)

# Stemming
tokens$stem <- wordStem(tokens$word, language = "en")

cat("Liczba tokenów:", nrow(tokens), "\n")


# 4. Chmury słów ----

# Częstość wszystkich słów
freq_all <- data.frame(table(tokens$stem))
names(freq_all) <- c("stem", "freq")
freq_all <- freq_all[order(freq_all$freq, decreasing = TRUE), ]

# Globalna chmura słów
par(mar = c(2, 2, 2, 2))
wordcloud(
  words = freq_all$stem,
  freq = freq_all$freq,
  min.freq = 3,
  max.words = 150,
  random.order = FALSE,
  rot.per = 0.15,
  colors = brewer.pal(8, "Dark2")
)
title("Chmura słów - wszystkie piosenki")

# Chmury słów po latach
years <- sort(unique(tokens$year))
for (yy in years) {
  freq_year <- data.frame(table(tokens$stem[tokens$year == yy]))
  names(freq_year) <- c("stem", "freq")
  freq_year <- freq_year[order(freq_year$freq, decreasing = TRUE), ]

  if (nrow(freq_year) > 1) {
    par(mar = c(2, 2, 2, 2))
    wordcloud(
      words = freq_year$stem,
      freq = freq_year$freq,
      min.freq = 2,
      max.words = 100,
      random.order = FALSE,
      rot.per = 0.15,
      colors = brewer.pal(8, "Set2")
    )
    title(paste("Chmura słów -", yy))
  }
}


# 5. Analiza sentymentu ----

# Wczytanie słowników
afinn_dict <- read.csv("dictionaries/afinn.csv", stringsAsFactors = FALSE)
bing_dict <- read.csv("dictionaries/bing.csv", stringsAsFactors = FALSE)

# Analiza AFINN - średni sentyment rocznie
sentiment_afinn_year <- tokens %>%
  inner_join(afinn_dict, by = c("word" = "word"))

sentiment_afinn_yearly <- aggregate(
  value ~ year,
  data = sentiment_afinn_year,
  FUN = mean,
  na.rm = TRUE
)
names(sentiment_afinn_yearly) <- c("year", "avg_sentiment_afinn")

# Analiza Bing - liczba słów pozytywnych i negatywnych
sentiment_bing_year <- tokens %>%
  inner_join(bing_dict, by = c("word" = "word"))

sentiment_bing_count <- aggregate(
  cbind(positive = sentiment == "positive", negative = sentiment == "negative") ~ year,
  data = sentiment_bing_year,
  FUN = sum
)
sentiment_bing_count$positive <- rowSums(sentiment_bing_count[, -1] * (bing_dict$sentiment[match(sentiment_bing_year$word, bing_dict$word)] == "positive"), na.rm = TRUE)
sentiment_bing_count$negative <- rowSums(sentiment_bing_count[, -1] * (bing_dict$sentiment[match(sentiment_bing_year$word, bing_dict$word)] == "negative"), na.rm = TRUE)

# Liczba słów pozytywnych i negatywnych per rok (szybsza metoda)
bing_summary <- sentiment_bing_year %>%
  group_by(year) %>%
  summarise(
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative")
  ) %>%
  mutate(
    net_sentiment = positive - negative,
    sentiment_index = (positive - negative) / pmax(positive + negative, 1)
  )

# Połączenie wyników
sentiment_results <- merge(sentiment_afinn_yearly, bing_summary, by = "year", all = TRUE)
sentiment_results <- sentiment_results[order(sentiment_results$year), ]

cat("\nWyniki analizy sentymentu:\n")
print(sentiment_results)

# Wykres AFINN
print(
  ggplot(sentiment_afinn_yearly, aes(x = year, y = avg_sentiment_afinn)) +
    geom_line(color = "#e74c3c", linewidth = 1) +
    geom_point(color = "#e74c3c", size = 3) +
    labs(
      title = "Średni sentyment rocznie (AFINN)",
      x = "Rok",
      y = "Średni sentyment"
    ) +
    theme_minimal()
)

# Wykres Bing - słowa pozytywne i negatywne
bing_long <- bing_summary %>%
  select(year, positive, negative) %>%
  gather(key = "sentiment", value = "count", -year)

print(
  ggplot(bing_long, aes(x = factor(year), y = count, fill = sentiment)) +
    geom_col(position = "dodge") +
    scale_fill_manual(
      values = c("positive" = "#27ae60", "negative" = "#e74c3c"),
      labels = c("positive" = "Pozytywne", "negative" = "Negatywne")
    ) +
    labs(
      title = "Liczba słów pozytywnych i negatywnych rocznie (Bing)",
      x = "Rok",
      y = "Liczba słów"
    ) +
    theme_minimal()
)

# Wykres Bing - indeks sentymentu
print(
  ggplot(bing_summary, aes(x = year, y = sentiment_index)) +
    geom_line(color = "#d95f0e", linewidth = 1) +
    geom_point(color = "#d95f0e", size = 3) +
    labs(
      title = "Indeks sentymentu rocznie (Bing)",
      x = "Rok",
      y = "Indeks sentymentu"
    ) +
    theme_minimal()
)


# 6. Klastrowanie tekstów ----

# Przygotowanie macierzy DTM
songs_text <- aggregate(
  stem ~ song_id + year + title + artist,
  data = tokens,
  FUN = function(x) paste(x, collapse = " ")
)

corp <- VCorpus(VectorSource(songs_text$stem))
dtm <- DocumentTermMatrix(corp, control = list(wordLengths = c(3, Inf)))
dtm <- removeSparseTerms(dtm, sparse = 0.98)
dtm_matrix <- as.matrix(dtm)

# Skalowanie
dtm_scaled <- scale(dtm_matrix)
rownames(dtm_scaled) <- songs_text$title

# Funkcja do wyboru k metodą łokcia
choose_k_elbow <- function(x, k_min = 2, k_max = 10) {
  wcss <- numeric(k_max - k_min + 1)
  k_seq <- seq(k_min, k_max)

  for (i in seq_along(k_seq)) {
    km <- kmeans(x, centers = k_seq[i], nstart = 25, iter.max = 200)
    wcss[i] <- km$tot.withinss
  }

  # Druga różnica dla znalezienia łokcia
  if (length(wcss) >= 3) {
    second_diff <- diff(wcss, differences = 2)
    elbow_idx <- which.max(second_diff) + 1
    best_k <- k_seq[elbow_idx]
  } else {
    best_k <- k_seq[1]
  }

  list(best_k = best_k, wcss = wcss, k_seq = k_seq)
}

# BoW klastrowanie
elbow_bow <- choose_k_elbow(dtm_scaled, k_min = 2, k_max = 10)
best_k_bow <- elbow_bow$best_k
km_bow <- kmeans(dtm_scaled, centers = best_k_bow, nstart = 50, iter.max = 300)

# Wykres metody łokcia dla BoW
elbow_df_bow <- data.frame(k = elbow_bow$k_seq, wcss = elbow_bow$wcss)
print(
  ggplot(elbow_df_bow, aes(x = k, y = wcss)) +
    geom_line(color = "#1b9e77", linewidth = 1) +
    geom_point(color = "#1b9e77", size = 3) +
    labs(
      title = "Metoda łokcia (BoW)",
      x = "Liczba klastrów (k)",
      y = "WCSS"
    ) +
    theme_minimal()
)

# PCA dla BoW
pca_bow <- prcomp(dtm_scaled)
pca_bow_df <- data.frame(
  title = rownames(dtm_scaled),
  cluster = factor(km_bow$cluster),
  PC1 = pca_bow$x[, 1],
  PC2 = pca_bow$x[, 2]
)

print(
  ggplot(pca_bow_df, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = paste("K-means (BoW), k =", best_k_bow),
      x = paste("PC1 (", round(summary(pca_bow)$importance[2, 1] * 100, 1), "%)"),
      y = paste("PC2 (", round(summary(pca_bow)$importance[2, 2] * 100, 1), "%)")
    ) +
    theme_minimal()
)

# TF-IDF klastrowanie
dtm_tfidf <- weightTfIdf(dtm)
tfidf_matrix <- as.matrix(dtm_tfidf)
tfidf_scaled <- scale(tfidf_matrix)
rownames(tfidf_scaled) <- songs_text$title

elbow_tfidf <- choose_k_elbow(tfidf_scaled, k_min = 2, k_max = 10)
best_k_tfidf <- elbow_tfidf$best_k
km_tfidf <- kmeans(tfidf_scaled, centers = best_k_tfidf, nstart = 50, iter.max = 300)

# Wykres metody łokcia dla TF-IDF
elbow_df_tfidf <- data.frame(k = elbow_tfidf$k_seq, wcss = elbow_tfidf$wcss)
print(
  ggplot(elbow_df_tfidf, aes(x = k, y = wcss)) +
    geom_line(color = "#7570b3", linewidth = 1) +
    geom_point(color = "#7570b3", size = 3) +
    labs(
      title = "Metoda łokcia (TF-IDF)",
      x = "Liczba klastrów (k)",
      y = "WCSS"
    ) +
    theme_minimal()
)

# PCA dla TF-IDF
pca_tfidf <- prcomp(tfidf_scaled)
pca_tfidf_df <- data.frame(
  title = rownames(tfidf_scaled),
  cluster = factor(km_tfidf$cluster),
  PC1 = pca_tfidf$x[, 1],
  PC2 = pca_tfidf$x[, 2]
)

print(
  ggplot(pca_tfidf_df, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = paste("K-means (TF-IDF), k =", best_k_tfidf),
      x = paste("PC1 (", round(summary(pca_tfidf)$importance[2, 1] * 100, 1), "%)"),
      y = paste("PC2 (", round(summary(pca_tfidf)$importance[2, 2] * 100, 1), "%)")
    ) +
    theme_minimal()
)


# 7. Podsumowanie wyników ----

cat("\n========== PODSUMOWANIE ANALIZY ==========\n")
cat("Liczba przeanalizowanych piosenek:", nrow(songs), "\n")
cat("Lata:", paste(sort(unique(songs$year)), collapse = ", "), "\n")
cat("Liczba unikalnych słów (po czyszczeniu):", length(unique(tokens$word)), "\n")
cat("Liczba unikalnych rdzeni:", length(unique(tokens$stem)), "\n")
cat("\nTop 20 najczęstszych słów:\n")
print(head(freq_all, 20))
cat("\nOptymalne k (BoW):", best_k_bow, "\n")
cat("Optymalne k (TF-IDF):", best_k_tfidf, "\n")
cat("\n========== ANALIZA ZAKONCZONA ==========\n")
