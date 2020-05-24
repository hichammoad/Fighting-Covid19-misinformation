
#fighting-covid19-misinformation

source('global.R')

ui  = dashboardPage( skin = "green",
                     dashboardHeader(title = "Data Veracity"),
                     dashboardSidebar(
                       useShinyjs(),
                       sidebarMenu(id="mainmenu",
                                   menuItem(text="Ask me", tabName = "ask_me"),
                                   menuItem(text="About", tabName = "about")
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem( tabName = 'ask_me',
                                   box( status = "primary", width = 12
                                       , uiOutput("chathistory")
                                       , textInput("questioninput", label="", placeholder = "Type your question here, like: Is there a vaccin for  covid-19?")
                                       , htmlOutput("predicted_nextword")
                                       , actionButton("sendbutton", label = "Send")
                                       , tags$br(),tags$br(), tags$hr()
                                       , uiOutput("frequentlyasked_output")
                                  )
                                  
                                  
                         ),
                         tabItem( tabName = 'about',
                                   box( id="Introduction",  status = "primary",  title = strong( 'Disclaimer' ), collapsible = T, width = 6, solidHeader = T,
                                         h5("Myriad of data is generated every minute from everywhere, and in different formats offering the consumers a large availability of information . However, the reliability of this information differ highly across sources. Unreliable sources provide misleading and biased information, which might lead to dangerous consequences especially in the case of epidemics, like covid-19 in which falsehoods are a matter of life-and death. Users find it difficult to manually check all information, therefore, in this project we propose a question answering tool that can automatically answers the user using a pre-trained model based on reliable data sources. ")
                                  )
                                  
                                  , box( id="disclaimer",  status = "primary",  title = strong( 'Disclaimer' ), collapsible = T, width = 6, solidHeader = T,
                                         h5("We only provide guidance and information about the Covid Epidemic 19. We do not provide medical advice, in addition, the form is automatically trained from the data, and can sometimes provide different answers to that required.")
                                  )
                                  , box( status = "primary", width = 12
                                       , tags$h4("First version of a question/answering system (charbot), trained using raw text about covid-19 facts")
                                       , tags$h4("Current version uses data from Who website, FAQ section")
                                  )
                                  
                         )
                       )
                       
                       
                     )
)



server <- function(input, output, session) {
  
  observeEvent(input$sendbutton, {
    req(nchar(input$questioninput) != 0)
    question = input$questioninput
    answer = get_answer(question)
    
    output$chathistory = renderUI({
      HTML(paste0(
        tags$b("You: "), tags$b(question) , tags$br(),
        tags$b("Answer: "), answer
      )  )
    })
    updateTextInput(session, inputId = "questioninput", value = "")
  })
  
 
  output$predicted_nextword = renderText({
    req(nchar(input$questioninput) != 0)
    get_prediction(input$questioninput)$suggestion
  })
  
  
  ## frequently_questions is dynamically generated
  output$frequentlyasked_output = renderUI(
    {
      questions_ui= tagList( tags$h4("Frequently asked questions:") , tags$br())
      for(questionid in 1:nrow(frequently_questions)){
        questions_ui = tagList(questions_ui
                               , actionButton(paste0("hint",frequently_questions[questionid, "questionid"])
                                              , label = frequently_questions[questionid, "question"])
                               , tags$br(), tags$br()
        )
      }
      
      return(questions_ui)
    }
  )
  
  
  observeEvent(input$hint1,{
    question(frequently_questions[1, "question"])
  })  
  
  observeEvent(input$hint2,{
    question(frequently_questions[2, "question"])
  })  
  
}


myApp=shinyApp(ui, server)
myApp
#runApp(myApp,launch.browser=F, host="0.0.0.0", port=5081 )


