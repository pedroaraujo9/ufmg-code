library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Iris Análise"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
             selectInput(inputId = 'est', label = 'Estatistica',
                          c('Mediana' = 'median',
                            'Mínimo' = 'min',
                            'Maximo' = 'max',
                            'Média' = 'mean',
                            'Desvio padrão' = 'sd'
                                        )),
             selectInput(inputId='var', labe='Variável', names(iris)[1:4]),
             checkboxInput('check', 'Agrupor por espécie', value = TRUE, width = NULL),
             
             verbatimTextOutput("estshow", placeholder = TRUE),
             dataTableOutput('grupos')
       ),
     mainPanel(
       dataTableOutput('tabela')
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$tabela <- renderDataTable({
     iris
   }, options=list(lengthMenu = c(10,20,30), pageLength = 10))
   out1 <- function(){
     fun <<- eval(parse(text=input$est))
     return(iris[,input$var] %>% fun)
   }
   out2 <- function(){
     tab <- tapply(iris[,input$var], FUN=fun, iris$Species)
     texto <- paste(
       'Setosa', tab[1],'|', 
       'Versicolor', tab[2],'|',
       'Virginica', tab[3]
     )
     return(texto)
   }
   
   output$estshow <- renderText({
     if(input$check){
       out2()
     }else{
       out1()
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)