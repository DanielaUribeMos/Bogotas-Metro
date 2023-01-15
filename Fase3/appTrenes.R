library(shinydashboard)
library(shiny)
library(plotly)
library(readxl)
setwd("/Users/DaniU/Documents/Universidad/Modelos/Proyecto/Fase3")
source("Fase2.R")

ui <- dashboardPage(
    dashboardHeader(title="Metro"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Fase1", tabName = "F1"),
            menuItem("Fase2", tabName = "F2"),
            menuItem("Fase3", tabName = "F3")
        )
    ),
    dashboardBody
    (
        tabItems
        (
            tabItem
            (
                tabName = "F2",
                sidebarLayout
                (
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
                        wellPanel(
                            titlePanel("Gráfica probabilidad"),
                            plotlyOutput(outputId = "Graf")
                        )
                    ),
                    mainPanel(),
                )
            ),
            
            tabItem
            (
                tabName = "F1",
                sidebarLayout
                (
                    splitLayout
                    (
                        wellPanel
                        (
                            sliderInput("kNQS",
                                        "Capacidad NQS:",
                                        min = 1,
                                        max = 500,
                                        value = 256),
                            sliderInput("kCaracas",
                                        "Capacidad Caracas:",
                                        min = 1,
                                        max = 500,
                                        value = 213),
                            sliderInput("kMayo",
                                        "Capacidad Mayo:",
                                        min = 1,
                                        max = 500,
                                        value = 57),
                            submitButton("Aplicar cambios")
                            
                        ),
                        wellPanel(
                            titlePanel("Valor esperado y varianza"),
                            textOutput(outputId = "etiqueta"),
                            plotlyOutput(outputId = "Grafica1"),
                            plotlyOutput(outputId = "Grafica4")
                        ),
                        wellPanel(
                            titlePanel("Largo plazo"),
                            plotlyOutput(outputId = "Grafica7"),
                            plotlyOutput(outputId = "Grafica8"),
                            plotlyOutput(outputId = "Grafica9")
                        )
                    ),
                    mainPanel()
                )
            ),
            tabItem
            (
                tabName = "F3",
                sidebarLayout
                (
                    splitLayout
                    (
                        wellPanel
                        (
                            sliderInput("kenvio",
                                        "Costo envío vagón:",
                                        min = 1,
                                        max = 500000,
                                        value = 150000),
                            sliderInput("kfaltante",
                                        "Costo no recoger un pasajero:",
                                        min = 1,
                                        max = 30000,
                                        value = 6000),
                            sliderInput("epoca",
                                        "Época:",
                                        min = 1,
                                        max = 72,
                                        value = 72),
                            sliderInput("estado",
                                        "Estado:",
                                        min = 0,
                                        max = 150,
                                        value = 0),
                            submitButton("Aplicar cambios o calcular")
                            
                        ),
                        wellPanel(
                            titlePanel("Costo vagones"),
                            textOutput(outputId = "Costovagones")
                        )
                        ,
                        wellPanel(
                            titlePanel("Mapa de calor"),
                            plotOutput(outputId = "mapa")
                        )
                    ),
                    mainPanel()
                )
            )
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
    output$Grafica1<-renderPlotly({
        Info4<- shiny_function_4({input$kNQS}, {input$kCaracas}, {input$kMayo})
        Info5<- shiny_function_5({input$kNQS}, {input$kCaracas}, {input$kMayo})
        Info6<- shiny_function_6({input$kNQS}, {input$kCaracas}, {input$kMayo})
        ggplot(title="Valor esperado") +
            geom_point(data = Info4, aes(x = Info4$minutos, y = Info4$X.esperado1.), color = "blue") + # must include argument label "data"
            geom_point(data = Info5, aes(x = Info5$minutos, y = Info5$X.esperado2.), color="green")+
            geom_point(data = Info6, aes(x = Info6$minutos, y = Info6$X.esperado3.))+
            ggtitle("Valor esperado")+ labs(x = "Minutos", y="Valor")
        
    })
    output$Grafica4<-renderPlotly({
        Info7<- shiny_function_7({input$kNQS}, {input$kCaracas}, {input$kMayo})
        Info8<- shiny_function_8({input$kNQS}, {input$kCaracas}, {input$kMayo})
        Info9<- shiny_function_9({input$kNQS}, {input$kCaracas}, {input$kMayo})
        ggplot(title="Varianza") +
            geom_point(data = Info7, aes(x = Info7$minutos, y = Info7$X.varianza1.), color = "blue") + # must include argument label "data"
            geom_point(data = Info8, aes(x = Info8$minutos, y = Info8$X.varianza2.), color="green")+
            geom_point(data = Info9, aes(x = Info9$minutos, y = Info9$X.varianza3.))+
            ggtitle("Varianza")+ labs(x = "Minutos", y="Valor")
    })
    
    output$Grafica7<-renderPlotly({
        Info10<- shiny_function_10({input$kNQS}, {input$kCaracas}, {input$kMayo})
        plot_ly(Info10, x=~Info10$estados1, y=~Info10$X.estable1., name="Prob largo plazo", type="scatter",
                mode="line+markers")%>%
            layout(title="Prob mayo", xaxis=list(title="Estados"),
                   yaxis=list(title="Valor"))
    })
    
    output$Grafica8<-renderPlotly({
        Info11<- shiny_function_11({input$kNQS}, {input$kCaracas}, {input$kMayo})
        plot_ly(Info11, x=~Info11$estados2, y=~Info11$X.estable2., name="Prob largo plazo", type="scatter",
                mode="line+markers")%>%
            layout(title="Prob NQS", xaxis=list(title="Estados"),
                   yaxis=list(title="Valor"))
    })
    
    output$Grafica9<-renderPlotly({
        Info12<- shiny_function_12({input$kNQS}, {input$kCaracas}, {input$kMayo})
        plot_ly(Info12, x=~Info12$estados3, y=~Info12$X.estable3., name="Prob largo plazo", type="scatter",
                mode="line+markers")%>%
            layout(title="Prob Caracas", xaxis=list(title="Estados"),
                   yaxis=list(title="Valor"))
    })
    
    output$Costovagones<-renderText({
        shiny_function_13({input$kenvio}, {input$kfaltante}, {input$epoca}, {input$estado})
    })
    
    output$mapa<-renderPlot({
        Info14<-shiny_function_14({input$kenvio}, {input$kfaltante}, {input$epoca}, {input$estado})
        ggplot(data = Info14, aes(x = Epoca, y = Estado, fill = Decision)) +   geom_tile()
    })
    
    output$etiqueta<-renderText({
        shiny_function_15()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

