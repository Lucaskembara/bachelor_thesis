require(stringr)

# Used to read a dictionary and create a vector of word occurences
dictionary_read <- function(dataframe, dictionary) {
  
  feature_count <- length(unique(dictionary$cat))
  feature_count <- 14
  
  base <- matrix(0, 1, feature_count)
  
  for (n in seq(length(dataframe))) {
    
    sentence <- dataframe[[n]]
    sentence_length <- length(sentence)
    sentence_score <- matrix(0, 1, feature_count)
    
    for (m in seq(length(dictionary$word))) {
      
      word <- dictionary[m,3]
      cat <- dictionary[m,4]
      
      if (str_sub(word, 1, 1) == "%") {
        word <- stringr::str_replace(word, "\\%", "*")
        score <- exclusive_wildcard(word, sentence = sentence)
      }
      else if ((str_sub(word, 1, 1) == "*") | (str_sub(word, -1, -1) == "*")) {
        
        word <- gsub("\\*", "[a-zA-Z]*", word)
        
        if (substring(word, 1, 1) != "[") {
          word <- paste("\\b", word, sep = "", collapse = NULL)
        } else if (substring(word, -1, -1) != "[") {
          word <- paste(word, "\\b", sep = "", collapse = NULL)
        }
        
        score <- length(grep(word, sentence))
      }
      else {
        score <- length(grep(word, sentence))
      }
      
      sentence_score[cat] <- sentence_score[cat] + score
      
    }
    base <- rbind(base, sentence_score)
  }
  return(base)
} 

#  Used to translate word occurences to a scaled version
calc_scaled_dictionary <- function(dataframe, dict_scores) {
  
  dict_scores <- dict_scores[-1,]
  
  for (n in seq(length(dataframe))) {
    sub <- dict_scores[n,]
    
    for (m in seq(length(sub))) {
      sub[m] <- sub[m] / sapply(strsplit(dataframe[n], " "), length)
    } 
    
    dict_scores[n,] <- sub
  }
  return(dict_scores)
}

# Get all feature values of a list of tokenized sentences
calc_features <- function(dataframe, dictionary) {
  dict_scores <- dictionary_read(dataframe, dictionary = dictionary)
  return(calc_scaled_dictionary(dataframe, dict_scores = dict_scores))
} 

# Wildcard script
exclusive_wildcard <- function(string, sentence) {
  
  if (str_sub(string, -1, -1) == "*") {
    string <- str_replace_all(string, "[^[:alnum:]]", "")
    string <- paste("[a-zA-Z]*", string, "[a-zA-Z]*", sep = "", collapse = NULL)
  } else {
    string <- str_replace_all(string, "[^[:alnum:]]", "")
    string <- paste("[a-zA-Z]*", string, "\\b", sep = "", collapse = NULL)
  }
  
  string_trimmed <- substr(string, 10, nchar(string))
  string_trimmed <- paste("\\b", string_trimmed, sep = "", collapse = NULL)
  x <- str_locate_all(sentence, string)
  
  result <- 0
  
  if (all(is.na(x[[1]]))) {
    return(result)
  } else {
    for(n in seq(nrow(x[[1]]))) {
      
      word <- substr(sentence, start = x[[1]][n,1], stop = x[[1]][n,2])
      
      if (length(grep(string_trimmed, word)) == 0) {
        result <- result + 1
      }
    }
  }
  return(result)
}
