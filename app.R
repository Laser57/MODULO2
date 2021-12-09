library("shiny")
library("shinydashboard")
library("shinythemes")
library("ggplot2")
library("plotly")
library("dplyr")
library("data.table")
library("tseries")
library("zoo")
library("forecast")

ui <- 
  
  fluidPage(
    
    dashboardPage(
      
      dashboardHeader(title = "Análisis Acciones"),
      
      dashboardSidebar(
        
        sidebarMenu(
          menuItem("Historico Accion", tabName = "Dashboard", icon = icon("angellist")),
          menuItem("Analisis TS", tabName = "graph", icon = icon("area-chart")),
          menuItem("Data", tabName = "data_table", icon = icon("table"))
        )
        
      ),
      
      dashboardBody(
        
        tabItems(
          
          # Histograma
          tabItem(tabName = "Dashboard",
                  fluidRow(
                    titlePanel("Histograma de las variables del data set mtcars"), 
                    selectInput("COMPAÑIA", "Seleccione el valor de X",
                                choices =  c("AAPL")),
                    
                    
                    
                    box(plotlyOutput("plot1", height = "auto", width =500)),
                    
                    
                  )
          ),
          
          # Dispersión
          tabItem(tabName = "graph", 
                  fluidRow(
                    titlePanel(h3("Analisis TS")),
                    
                    (plotOutput("output_plot", height = 500, width = 760) )
                    
                  )
          ),
          
          
          
          tabItem(tabName = "data_table",
                  fluidRow(        
                    titlePanel(h3("Data Table")),
                    dataTableOutput ("data_table")
                  )
          ) 
          
          
        )
      )
    )
  )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
  
  #read.csv(data, "../data/file.csv")
  #Gráfico de Histograma
  
  
  
  
  output$plot1 <- renderPlotly({
    
    file <- read.csv("DATA/AAPL.csv")  
    ventas=as.data.frame(file%>%
                           filter(Tiker==input$COMPAÑIA) %>%
                           group_by(Tiker,Date) %>%
                           summarise(SUMMARY=sum(Close,na.rm = TRUE)))
    ventas$Tiker<-NULL
    
    td2 = as.Date(ventas$Date, format="%Y-%m-%d") 
    #datosts1 <- ts(data = ventas$SUMMARY, start = 2010, freq = 360)
    datosTS <- zoo(x=ventas$SUMMARY, order.by=td2) 
    arima<-auto.arima(datosTS)
    
    fore = forecast(datosTS,h=10,level = c(80, 95))
    plot <-plot_ly() %>%
      add_lines(x = time(datosTS), y = datosTS,
                color = I("black"), name = "datos conocidos")
    

    
    
  })
  
  # Gráficas de dispersión
  output$output_plot <- renderPlot({ 
    file <- read.csv("DATA/AAPL.csv")  
    ventas=as.data.frame(file%>%
                           filter(Tiker==input$COMPAÑIA) %>%
                           group_by(Tiker,Date) %>%
                           summarise(SUMMARY=sum(Close,na.rm = TRUE)))
    ventas$Tiker<-NULL
    
    datosts1 <- ts(data = ventas$SUMMARY, start = 2010, freq = 360)
    componentes = decompose(datosts1)
    plot(componentes)
    
    
  })   
  
  #Data Table
  output$data_table <- renderDataTable({
    file <- read.csv("DATA/AAPL.csv")  
    ventas=as.data.frame(file%>%
                           filter(Tiker==input$COMPAÑIA) %>%
                           group_by(Tiker,Date) %>%
                           summarise(SUMMARY=sum(Close,na.rm = TRUE)))
    ventas$Tiker<-NULL
    
    td2 = as.Date(ventas$Date, format="%Y-%m-%d")
    datosTS <- zoo(x=ventas$SUMMARY, order.by=td2) 
    fore = forecast(datosTS)
    return(fore$upper)
    
  })
  
  
}


shinyApp(ui, server)
