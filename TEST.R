library(tidyverse)
library(dygraphs)
library(zoo)
library(forecast)
library(xts)
library(plotly)
#Pendiente subir a base de datos modulo 1
#Dato prueba AAPL 2010-2021
file <- read.csv("AAPL.csv")  
ventas=as.data.frame(file %>%
                       filter(Tiker=="AAPL") %>%
                       group_by(Tiker,Date) %>%
                       summarise(SUMMARY=sum(Close,na.rm = TRUE)))
ventas$Tiker<-NULL

td2 = as.Date(ventas$Date, format="%Y-%m-%d") 
datosts1 <- ts(data = ventas$SUMMARY, start = 2010, freq = 360)
#Clase zoo para manejo eficiente de linea de tiempo
datosTS <- zoo(x=ventas$SUMMARY, order.by=td2) 
arima<-auto.arima(datosTS)
class(arima) 
#PARAMETRO PARA FORECAST
#Proyeccion a 10 Días H
#Level niveles confianza 80 y 95
fore = forecast (arima,h=10,level = c(80, 95))

#Analizar la serie de tiempo para ver estacionalidad, residuos
componentes = decompose(datosts1)
plot(componentes, name="TS APPLE")

#Grafico Intreactivo por accion
dygraph(datosTS, "Historico Acciones APPLE") 

autoplot(fore)
#Resultados del forecast
summary(fore)




