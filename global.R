
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ruimtehol)


model = ruimtehol::starspace_load_model("model")
facts_dataset = readRDS("facts_dataset.RDS")
    
  
get_answer = function(question){
  question = tolower(question)
  question = gsub("[[:punct:]]", " ", question)
  scores = predict(model, question ,basedoc = facts_dataset)
  answer = scores[[1]]$prediction$label[1]
  return(answer)
}  


frequently_questions = data.frame(questionid=1:2, question=c(
  "Is there a vaccine for covid19?", "How does COVID-19 spread ?"
), stringsAsFactors = F)
