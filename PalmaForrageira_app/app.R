library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Palma Forrageira v0.0.1"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Arquivo",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Predição", DT::dataTableOutput("contents")),
                tabPanel("Produtividade", verbatimTextOutput("summary"))
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
}

# Run the application
shinyApp(ui = ui, server = server)
