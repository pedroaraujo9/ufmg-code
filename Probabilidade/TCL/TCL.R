library(shiny)
library(dplyr)
library(shinythemes)

ui <- fluidPage(theme = shinytheme('flatly'),
   
   titlePanel("Teorema central do limite"),
   
   sidebarLayout(
      sidebarPanel(
        #barra 1
        sliderInput('quantidade_amostras', 'Número de amostras da média amostral:',
                    min = 1, max = 1000, value = 100),
        #barra 2
        sliderInput("tamanho_amostra",
                     "Tamanho das amostras das observações:",
                     min = 1,
                     max = 1000,
                     value = 100),
        #distribuições
        selectInput('distribuicao', 'Distribuição',
                    choices = c('Exponencial' = 'rexp',
                                'Weibull' = 'rweibull',
                                'Gamma' = 'rgamma',
                                'Qui-quadradro' = 'rchisq',
                                'Geométrica' = 'rgeom',
                                'Binomial' = 'rbinom',
                                'Binomial negativa' = 'rnbinom',
                                'Poisson' = 'rpois')),
        #argumentos
        uiOutput('argumento_ui_1'),
        uiOutput('argumento_ui_2'),
        uiOutput('argumento_ui_3'),
        #texto com o p-valor
        textOutput('p_valor')
      ),
      
      mainPanel(
        #painel com os gráficos
        tabsetPanel(
          #qqnorm
          tabPanel('qqplot', plotOutput('qqplot')),
          #histograma
          tabPanel('Histograma', plotOutput('histo'))
        )
      )
   )
)
#server
server <- function(input, output) {
  #gerando o primeiro input do argumento
  output$argumento_ui_1 <- renderUI({
    #transformando a string da barra de seleção com as distribuiçõs em uma função
    funcao <- eval(parse(text=input$distribuicao))
    #criando o input com o argumento
    numericInput('argumento_1', label=formalArgs(funcao)[2], value=1, min=0)
    
  })
  output$argumento_ui_2 <- renderUI({
    funcao <- eval(parse(text=input$distribuicao))
    #se não há um segundo argumento, não há retorno
    if(is.na(formalArgs(funcao)[3]))
      return()
    if(input$distribuicao=='rchisq'){
      return()
    }
    #caso contrário, retorno do segundo argumento
    numericInput('argumento_2', label=formalArgs(funcao)[3], value=1)
  })
  #gerando o qqnorm
  output$qqplot <- renderPlot({

    set.seed(28)
    funcao <- eval(parse(text=input$distribuicao))
    x_barra <- vector()
    amostra = numeric()
    
    #caso com dois argumentos
    if(input$distribuicao %in% c('rgamma', 'rweibull', 'rnbinom', 'rbinom')){
      for(i in 1:input$quantidade_amostras){
        amostra = funcao(input$tamanho_amostra, input$argumento_1,
                            input$argumento_2)
        
        x_barra <- c(x_barra, mean(amostra))
      }
    }else{
      #caso com um argumento
      for(i in 1:input$quantidade_amostras){
        amostra = funcao(input$tamanho_amostra, input$argumento_1)
        x_barra <- c(x_barra, mean(amostra))
      }
    }
    qqnorm(x_barra, col ='royalblue')
    qqline(x_barra)
  })
  #gerando o histograma
  output$histo <- renderPlot({
    set.seed(28)
    funcao <- eval(parse(text=input$distribuicao))
    x_barra <- vector()
    amostra = numeric()
    
    if(input$distribuicao %in% c('rgamma', 'rweibull', 'rnbinom', 'rbinom')){
      for(i in 1:input$quantidade_amostras){
        amostra = funcao(input$tamanho_amostra, input$argumento_1,
                         input$argumento_2)
        
        x_barra <- c(x_barra, mean(amostra))
      }
    }else{
      for(i in 1:input$quantidade_amostras){
        amostra = funcao(input$tamanho_amostra, input$argumento_1)
        x_barra <- c(x_barra, mean(amostra))
      }
    }
    hist(x_barra, main='Hostograma da média amostral', col='royalblue',
         border='white')
  })
  #gerando o texto
  output$p_valor <- renderText({
    set.seed(28)
    funcao <- eval(parse(text=input$distribuicao))
    x_barra <- vector()
    amostra = numeric()
    
    if(input$distribuicao %in% c('rgamma', 'rweibull', 'rnbinom', 'rbinom')){
      for(i in 1:input$quantidade_amostras){
        amostra = funcao(input$tamanho_amostra, input$argumento_1,
                         input$argumento_2)
        
        x_barra <- c(x_barra, mean(amostra))
      }
    }else{
      for(i in 1:input$quantidade_amostras){
        amostra = funcao(input$tamanho_amostra, input$argumento_1)
        x_barra <- c(x_barra, mean(amostra))
      }
    }
    p <- shapiro.test(x_barra)
    #texto
    paste('O p-valor do teste Shapiro-Wilk para a amostra é:',
          round(shapiro.test(x_barra)$p.value, 4))
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

