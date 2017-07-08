library(shiny)
library(shinydashboard)
##sidebar

sidebar <- dashboardSidebar(
  menuItem('Medidas protetivas', tabname='medidas', icon = icon("bar-chart-o")),
  menuItem('Fatores', tabname='fatores', icon = icon("bar-chart-o"))
)

#body
body <- dashboardBody(
  tabItem(tabName='medidas',
          fluidRow(
            box(
              selectInput(inputId='medidas', label='Medida protetiva',
                          c('kit de emergência' = 'kit',
                            'informação' = 'info',
                            'lista com indicações' = 'lista',
                            'planejamento com parentes' = 'prevencao_conjunta',
                            'material de proteção' = 'mat_barreira',
                            'seguro de vida' = 'seguro_vida')),
              radioButtons("art", "Artigo:",
                           c("Gilvan" = "gilvan",
                             "Lindell" = "lindell",
                             "Comparação" = "comparacao",
                             'Área com risco - Comum' = 'risco')
            )
          )
    )
  ),
  tabItem(tabName = 'fatores',
          fluidRow(
            box(
              
            )
          )
          )
)

ui <- dashboardPage(
  dashboardHeader(title='Análise Descritiva'),
  sidebar,
  body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

