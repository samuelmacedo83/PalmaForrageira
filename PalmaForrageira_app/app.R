library(shiny)
library(shinyWidgets)


tipo_espacamento <- c("Simples", "Fileiras Duplas", "Três Fileiras", "Quatro Fileiras")

ui <- fluidPage(
    navbarPage("Palma Forrageira v0.01",
               tabPanel("Produtividade",
                        sidebarLayout(
                            sidebarPanel(width = 4,
                                         strong(p("Área", align = "center")),
                                         fluidRow(
                                             column(12,
                                             splitLayout(
                                                    numericInput("comp", "Comprimento (m)", value = 30),
                                                    numericInput("larg", "Largura (m)", value = 10)
                                                 )
                                             )
                                         ),
                                         tags$hr(),
                                         strong(p("Espaçamento", align = "center")),
                                         selectInput("espacamento",
                                                     label = NULL,
                                                     choices = tipo_espacamento
                                                     ),
                                         tags$hr(),
                                         strong(p("Cladódios por Planta", align = "center")),
                                         fluidRow(
                                             column(11,
                                             splitLayout(
                                                 numericInput("p1", "P1", value = 10),
                                                 numericInput("p2", "P2", value = 8),
                                                 numericInput("p3", "P3", value = 9),
                                                 numericInput("p4", "P4", value = 9)
                                             ),
                                             splitLayout(
                                                 numericInput("p5", "P5", value = 10),
                                                 numericInput("p6", "P6", value = 11),
                                                 numericInput("p7", "P7", value = 12),
                                                 numericInput("p8", "P8", value = 13)
                                             )
                                             )
                                         ),
                                         tags$hr(),
                                         strong(p("Animais", align = "center")),
                                         fluidRow(
                                             column(12,
                                             splitLayout(
                                                 numericInput("qtd_animais", "Quantidade", value = 30),
                                                 numericInput("demanda_animal", "Demanda por animal", value = 20)
                                             ))
                                         )
                            ),
                            mainPanel(
                                column(5,
                                    DT::dataTableOutput("fornecimento")
                                ))
                        )
               ),
               tabPanel("Predição",
                        sidebarLayout(
                            sidebarPanel(width = 3,
                                fileInput("file1", "Arquivo",
                                           multiple = TRUE,
                                           accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"))
                                ),
                                mainPanel(
                                    DT::dataTableOutput("contents")
                                )
                        )
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$contents <- DT::renderDataTable(DT::datatable({
        if(is.null(input$file1)){
          NULL
        } else{
          dados <- read.table(input$file1$datapath, header = TRUE)
          resultados <- PalmaForrageira:::Produtividade1(dados)
          round(data.frame(dados, Predito = resultados$Predito),
                digits = 2)

        }
        }))

    output$fornecimento <- DT::renderDataTable(DT::datatable({
        cladodios <- c(input$p1, input$p2,
                       input$p3, input$p4,
                       input$p5, input$p6,
                       input$p7, input$p8)

        PalmaForrageira:::fornecimento(input$qtd_animais, input$demanda_animal,
                                 cladodios,
                                 input$comp, input$larg,
                                 input$espacamento)
    }))
}

# Run the application
shinyApp(ui = ui, server = server)
