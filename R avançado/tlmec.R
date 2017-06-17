library(data.table)
library(shiny)
library(dplyr)
library(DT)
library(tlmec)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme('united'),
  
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
      radioButtons('distri', 'Distribuição', c('Normal' = 'Normal',
                                               't-student' = 't')),
      radioButtons('criterio', 'Calcular critério de comparação?',
                   c('Sim' = TRUE,
                   'Não' = FALSE)),
      uiOutput('resposta_ui'),
      uiOutput('censura_ui'),
      uiOutput('explicativas_ui'),
      actionButton('do', 'Ajustar')
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
  observeEvent(input$do, {
    #lendo os dados
    df <- fread(inFile()$datapath)
    df <- as.data.frame(df)
    #ordenando
    o <- order(df[,1], df[,input$x])
    df <- df[o,]
    #vetor com censura
    try(censura <- (df[,input$cens]==1)+0)
    #resposta
    try(y <- log10(df[,input$y]), silent = T)
    try(aa <- y[censura==0])
    #design matrix - explicativa
    try(explicativa <- df[,input$x])
    try(obs <- unique(explicativa))
    x <- vector();
    
    for(i in obs){
      x <- cbind(x, (explicativa==i)+0)
    }
    
    try(z <- matrix(rep(1, length(y)), ncol=1))
    
    cluster <- as.numeric(df[,1] %>% as.factor() %>% as.numeric())
    k = df[,1] %>% unique() %>% length()
    nj <- matrix(0,k , 1)
    for (j in 1:k) {
      nj[j]=sum(cluster==j)
    }
    
    out <- tlmec(cens,y,x, z,nj, family = input$distri, criteria = input$criterio)
    
    output$texto <- renderPrint(out)
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
