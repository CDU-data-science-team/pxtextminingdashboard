clean_text <- function(x) {
  
  x %>% 
    stringr::str_replace_all("[^[:alnum:]]", " ") %>% # Keep only alphanumeric characters
    stringr::str_replace_all("[ \t\n\r\v\f]", "_") %>% # No whitespaces
    tolower()
}

# Plot functions
plotBigramsNetwork <- function(x) {
  
  x %>% 
    experienceAnalysis::plot_bigrams_network()
}

plotBingWordCounts <- function(x) {
  
  x %>%
    experienceAnalysis::plot_bing_word_counts()
}

plotConfusionMatrix <- function(x, ...) {
  
  x %>% 
    experienceAnalysis::plot_confusion_matrix(type = "heatmap", ...)
}

plotLearnerPerformance <- function(x) {
  
  experienceAnalysis::plot_best_estimators(x)
}

plotNetSentiment <- function(x, ...) {
  
  x %>%
    experienceAnalysis::plot_net_sentiment_per_tag(...)
}

plotTfidfNgrams <- function(x, ...) {
  
  x %>% 
    experienceAnalysis::plot_tfidf_ngrams(...)
}