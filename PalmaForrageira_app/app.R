library(shiny)
library(shinyWidgets)

tipo_espacamento <- c("Simples", "Fileiras Duplas", "Três Fileiras", "Quatro Fileiras")

ui <- fluidPage(
    navbarPage("Palma Forrageira v0.0.1",
               tabPanel("Regressão",
                       img(src = "fig.gif", style = "position: absolute; width: 65%; height: 65%;",
                           hspace = "450px", vspace = "150px"
                          # ,  width = "65%", height = "65%"
                           ),
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
               tabPanel("Redes neurais",
                        img(src = "fig.gif", style = "position: absolute",
                            hspace = "450px", vspace = "150px",
                            width = "65%", height = "65%"),
                        sidebarLayout(
                            sidebarPanel(width = 4,
                                         numericInput("CRq", "CRq (cm)", value = 29.91),
                                         numericInput("LRq", "LRq (cm)", value = 14.89),
                                         numericInput("Erq", "Erq (mm)", value = 8.94),
                                         numericInput("ALT", "ALT (m)", value = 1.05),
                                         numericInput("NRQ", "NRQ (un)", value = 34),
                                         numericInput("ATC", "ATC (m²)", value = 2.09)
                                ),
                                mainPanel(
                                    column(5,
                                    DT::dataTableOutput("contents")
                                    )
                                )
                        )
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$contents <- DT::renderDataTable(DT::datatable({
        dados <- matrix(c(input$CRq, input$LRq, input$Erq, input$ALT, input$NRQ, input$ATC), ncol = 6)
        produtividade <- PalmaForrageira:::Produtividade1(dados)
        round(data.frame('Produtividade (kg/Planta)' = produtividade, check.names = FALSE),
             digits = 2)
    }, rownames = FALSE,  options = list(dom = 't') ))

    output$fornecimento <- DT::renderDataTable(DT::datatable({
        cladodios <- c(input$p1, input$p2,
                       input$p3, input$p4,
                       input$p5, input$p6,
                       input$p7, input$p8)

        dados <- PalmaForrageira:::fornecimento(input$qtd_animais, input$demanda_animal,
                                       cladodios,
                                       input$comp, input$larg,
                                       input$espacamento)
        round(dados, digits = 2)
    }, rownames = FALSE, options = list(dom = 't') ))
}

# Run the application
shinyApp(ui = ui, server = server)
