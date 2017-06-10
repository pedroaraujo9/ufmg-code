library(data.table)
library(shiny)
library(dplyr)
library(DT)
dd <<- data.frame()
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Interface para a função tlmec"),
  
  # lendo os dados 
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Escolha um arquivo:',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv',
                         'text/txt',
                         '.txt')),
      radioButtons('distri', 'Distribuição', c('Normal' = 'norm',
                                               't-student' = 't')),
      radioButtons('criterio', 'Calcular critério de comparação?',
                   c('Sim' = TRUE,
                   'Não' = FALSE)),
      uiOutput('resposta_ui'),
      uiOutput('censura_ui'),
      uiOutput('explicativas_ui'),
      uiOutput('do_ui')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput('texto')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #lendo o input com o arquivo
  inFile <- reactive({input$file1})
  
  #var resposta
  output$resposta_ui <- renderUI({
    
    if(is.null(input$file1))
      return()
    
    
    df <- fread(inFile()$datapath)
    variaveis <- colnames(df)
    
    radioButtons("y", "Variável resposta", choices = variaveis)
    
  })
  #var censura
  output$censura_ui <- renderUI({
    if(is.null(input$file1))
      return()
    df <- fread(inFile()$datapath)
    variaveis <- colnames(df)
    
    variaveis <- variaveis[!(variaveis==input$y)]
    
    radioButtons("cens", "Variável de censura", choices = variaveis)
    
    
  })
  
  #var explicativa
  output$explicativas_ui <- renderUI({
    if(is.null(input$file1))
      return()
    df <- fread(inFile()$datapath)
    variaveis <- colnames(df)
    
    variaveis <- variaveis[!(variaveis==input$y)]
    variaveis <- variaveis[!(variaveis==input$cens)]
    
    
    radioButtons("x", "Variável explicativa", choices = variaveis)
    
  })
  
  #iniciando o modelo
  output$do_ui <- renderUI({
    actionButton('do', 'Ajustar')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
