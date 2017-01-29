library(stringr)
library(readr)
library(tidyverse)
library(tm)


# Munge the speech text file and extract some initial features:
#   1. sentence lengths
#   2. mean sentence length
#   3. std sentence length
#   4. min sentence length
#   5. max sentence length
#   6. term frequency
# Then return a tibble
mungeSpeech <- function(filename) {
  filepath <- paste("./data/inaugural_speeches/", filename, sep = "")
  raw.speech <- read_file(filepath)
  speech <- raw.speech %>%
    tolower() %>%
    str_replace_all(",|\"|\n|\r|:", "")
  sentences <- str_split(speech, "\\?|!|\\.")
  sentence_lengths <- vector(mode = "integer", length = lengths(sentences))
  i <- 1
  for (sentence in sentences[[1]]) {
    words <- sentence %>%
      trimws() %>%
      str_split(" ")
    sentence_lengths[i] = lengths(words)
    i <- i + 1
  }
  words <- speech %>%
    removePunctuation() %>%
    removeWords(stopwords()) %>%
    str_split("\\s+")
  doc <- PlainTextDocument(words)
  name <- str_replace_all(filename, "\\.txt", "")
  speech.data <- tibble(
    name = name,
    s_lengths = list(sentence_lengths),
    mean_s_length = mean(sentence_lengths),
    std_s_length = sqrt(var(sentence_lengths)),
    min_s_length = min(sentence_lengths),
    max_s_length = max(sentence_lengths),
    term_freq = list(termFreq(doc))
  )
  return(speech.data)
}

speech.files <- c(
  "trump.txt",
  "obama2.txt",
  "obama1.txt",
  "bush2.txt",
  "bush1.txt",
  "kennedy.txt",
  "roosevelt1.txt",
  "lincoln2.txt"
)

speeches <- tibble()
for (fn in speech.files) {
  speeches <- rbind(speeches, mungeSpeech(fn))
}
