
library(shiny)
library(plotly)
library(readxl)
source("Punto2.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Aplicación trenes"),
    
    # Sidebar with a slider input for number of bins 
    splitLayout(
        wellPanel(
            sliderInput("Costofallas",
                        "Costo fallas graves:",
                        min = 30000000,
                        max = 100000000,
                        value = 76500000),
            sliderInput("Costokm",
                        "Costo km:",
                        min = 30000000,
                        max = 100000000,
                        value = 85000000),
            sliderInput("Costooptimo",
                        "Costo condiciones óptimas:",
                        min = 10000000,
                        max = 100000000,
                        value = 10000000),
            sliderInput("Costoregulares",
                        "Costo condiciones regulares:",
                        min = 10000000,
                        max = 100000000,
                        value = 12500000),
            fileInput(inputId ='ArchivoEntrada', label='Cargue el archivo', accept=c('.xlsx')),
            submitButton("Aplicar cambios"),
            column(width=12)
            
        ),
        wellPanel(titlePanel("Costos"), 
                  verticalLayout( titlePanel("Costo chatarrización"), textOutput(outputId = "Costo1")), 
                  verticalLayout(titlePanel("Costo operación"), textOutput(outputId = "Costo2"))),
        
        #Show a plot of the generated distribution
        wellPanel(
            titlePanel("Gráfica probabilidad"),
            plotlyOutput(outputId = "Graf")
        )
    )
)

# Define server logic required to draw a histogram
#' Title
#'
#' @param input 
#' @param output 
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output) 
{
    datosEstado<-eventReactive(input$ArchivoEntrada, {
        Archivo1<-input$ArchivoEntrada
        if(is.null(Archivo1))
        {
            return(NULL)
        }
        dataFile<-read_excel(Archivo1$datapath, sheet=1, col_names=TRUE)
        estado_tren<-dataFile[[3]]
    })
    
    output$Graf<-renderPlotly({
    if(is.null(datosEstado()))
    {
        return(NULL)
    }
        Info3<- shiny_function_3(datosEstado())
        plot_ly(Info3, x=~Info3$estados_graf, y=~Info3$X.vector_graf., name="Probabilidad vida útil", type="scatter",
                mode="line+markers")%>%
        layout(title="Probabilidad vida útil vs km", xaxis=list(title="Km acumulados"),
        yaxis=list(title="Probabilidad"))
    })
    output$Costo1<-renderText({
        Archivo1<-input$ArchivoEntrada
        if(is.null(datosEstado()))
        {
            return(NULL)
        }
        shiny_function_1(datosEstado(), {input$Costofallas}, {input$Costokm})
    })
    output$Costo2<-renderText({
        Archivo1<-input$ArchivoEntrada
        if(is.null(datosEstado()))
        {
            return(NULL)
        }
        shiny_function_2(datosEstado(), {input$Costooptimo}, {input$Costoregulares})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

