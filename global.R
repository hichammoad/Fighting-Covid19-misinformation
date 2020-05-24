.libPaths( c(.libPaths(), "/home/rstudio/R/x86_64-pc-linux-gnu-library/3.6") )
  
library(shiny)
library(shinyjs)
library(shinydashboard)
#library(ruimtehol)
library(ngram)
library(fastrtext)
library(text2vec)

#model = ruimtehol::starspace_load_model("model")
if(!exists("model")){
  model <- load_model("model")
}
kb = readRDS("data/facts_dataset.RDS")
load('data/ngrams.RData')
frequently_questions = readRDS("data/frequently_questions.RDS")

# processig text: removing special characters and numbers, then steming: 
process = function(strg){
  strg = tolower(strg)
  strg = gsub("[[:punct:]]", " ", strg)
  strg = gsub("\\d"," ", strg)
  strg  = trimws(strg)
  strg = gsub("\\s+"," ",strg)
  #strg = stemmer(strg)
  return(strg)
}

sentence_similarity = function(sentence1, sentence2){
  1 - crossprod(sentence1[1,],sentence2[1,])/sqrt(crossprod(sentence1[1,]) * crossprod(sentence2[1,]))
}

get_answer = function(question){
  question = process(question)
  query = get_sentence_representation(model,question)
  dist_vect=c()
  for(entry in 1:nrow(kb)){
    dist_vect[entry] = sentence_similarity(query, kb[entry,"embd"])
  }
  print(min(dist_vect, na.rm = T))
  if( min(dist_vect, na.rm = T) > 0.95){
    answer = "I didn't understand the question, can you re-formulate it please?"
  }else{
    answer = kb[which.min(dist_vect) ,"V3" ]
  }
  return(answer)
}  

stemmer <- function(x) {
  tokens = word_tokenizer(x)
  res= unlist(lapply(tokens, SnowballC::wordStem, language = "en"))
  paste0(res,collapse = " ")
}



get_last_n_words <- function(input_string, n){
  words <- unlist(strsplit(input_string, " "))
  if(n > length(words)){return(input_string)}
  nwords = paste(c(words[(length(words) - n + 1):(length(words))], ""), collapse = " ")
  nwords = trimws(nwords)
  nwords
}

get_punch <- function(ngramX, input_string){
  punch_of_setup <- ngramX[ngramX$firstw == input_string, ]$nextw[1]
  return(c(punch_of_setup))
}

get_prediction <- function(input_string) {
  
  res <- data.frame(current_string = process(input_string) , 
                   suggestion = " ", 
                   stringsAsFactors = FALSE)
  
  
  if (res$current_string == ""){
    return(res)
  }
  
  # get ngram suggestions, try using 4-gram, then 3-gram, ..etc
  res$current_string <- get_last_n_words(res$current_string, 4)
  better_suggestion <- get_punch(ngram5, res$current_string)
  if (!is.na(better_suggestion)){
    res$suggestion <- better_suggestion
    return(res)
  }
  
  res$current_string <- get_last_n_words(res$current_string, 3)
  better_suggestion <- get_punch(ngram4, res$current_string)
  if (!is.na(better_suggestion)){
    res$suggestion <- better_suggestion
    return(res)
  }
  
  res$current_string <- get_last_n_words(res$current_string, 2)
  better_suggestion <- get_punch(ngram3, res$current_string)
  if (!is.na(better_suggestion)){
    res$suggestion <- better_suggestion
    return(res)
  }
  
  res$current_string <- get_last_n_words(res$current_string, 1)
  better_suggestion <- get_punch(ngram2, res$current_string)
  if (!is.na(better_suggestion)){
    res$suggestion <- better_suggestion
    return(res)
  }
  
  return(res)
  
}


