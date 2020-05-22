
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
                    , actionButton("sendbutton", label = "Send")
                    , tags$br(),tags$br(), tags$hr()
                    , uiOutput("frequentlyasked_output")
               )     
      ),
      tabItem( tabName = 'about',
               box( status = "primary", width = 12
                   , tags$h4("First version of a question/answering system (charbot), trained using raw text about covid-19 facts")
                   , tags$h4("Current version uses data from Who website, FAQ section")
               )
      )
      )
    
    
  )
)
    


server <- function(input, output, session) {
  
    question = reactiveVal("")
    
    answer = reactive({
      req(nchar(question()) != 0)
      get_answer(question())
    })
    
    output$chathistory = renderUI({
      HTML(paste0(
        tags$b("You: "), tags$b(question()) , tags$br(),
        tags$b("Answer: "), answer()
      )  )
    })
    
      
    observeEvent(input$sendbutton, {
      question(input$questioninput)
      updateTextInput(session, inputId = "questioninput", value = "")
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
  

shinyApp(ui, server)

  