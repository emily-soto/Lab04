library(readr)
library(dplyr)
library(highcharter)
library(tidyverse)
library(ggplot2)
library(tm)
library(qqc)
df<- read_csv("tabla_completa.csv", locale = locale(encoding = "Latin1"), col_types = cols(MES = col_number()))

##Asumptions

#1.El df es de un solo producto
df$Q/df$CANTIDAD
#2.Los clientes con "faltante" son ordenes que faltaron en despachos anteriores
#3. Los clientes con "Devolución" representan salida de dinero ya que son devoluciones
df$CANTIDAD=ifelse(test = str_detect(df$CLIENTE, "DEVOLUCION")==TRUE, yes = df$CANTIDAD*-1, no=df$CANTIDAD*1)
df$Q=ifelse(test = str_detect(df$CLIENTE, "DEVOLUCION")==TRUE, yes = df$Q*-1, no=df$Q*1)


##Analisis

df %>% 
  group_by(CLIENTE) %>% 
  summarise(n_distinct())

(df[str_detect(df$CLIENTE, "Faltante|Despacho a cliente |DEVOLUCION"),])

df %>%
  group_by(CREDITO) %>%
  summarise(n())

##Entrada de dinero mensual
df$FlujoMes=df$MES+df$CREDITO/30

FCmensual=df %>% 
  group_by(FlujoMes) %>% 
  summarise(FlujoMensual=sum(Q))
FCmensual %>% 
  filter(FlujoMes<13) %>% 
  hchart("line",hcaes(x=FlujoMes, y=FlujoMensual)) %>% 
  hc_title(text="<b>Flujo neto durante el año<b>")


#"Ingresos no recibidos en el año
Flujo2018= df %>% 
  group_by(SiguienteAnio=FlujoMes>12) %>%
  summarise(FlujoMensual=sum(Q), Porcentaje=100*(FlujoMensual/sum(df$Q))) 

Flujo2018$SiguienteAnio=ifelse(test = Flujo2018$SiguienteAnio==TRUE, yes = 2018, no = 2017)

Flujo2018 %>% 
  hchart("column",hcaes(x=SiguienteAnio, y=Porcentaje)) %>% 
  hc_title(text="<b>Flujo neto por anio<b>")

df %>%
  group_by(Dias=as.character(CREDITO)) %>%
  summarise(Porcentaje=100*n()/nrow(df)) %>% 
  hchart("column",hcaes(x=Dias, y=Porcentaje)) %>% 
  hc_title(text="<b>Dias de credito<b>")

## Transportistas

df %>% 
  group_by(FlujoMes, PILOTO) %>% 
  summarise(FlujoMensual=sum(Q)) %>% 
  hchart("scatter",hcaes(FlujoMes, y=FlujoMensual, group=PILOTO)) 

df %>%
  group_by(PILOTO) %>%
  summarise(QenOrdenes=sum(Q)) %>% 
  hchart("column",hcaes(x=PILOTO, y=QenOrdenes)) %>% 
  hc_title(text="<b>Q por Piloto<b>")

ordenesMes=df %>%
  group_by(PILOTO) %>%
  summarise(OrdenesMensuales=n()/12) 
ggplot(ordenesMes) +
  geom_col(aes(x=PILOTO, y=OrdenesMensuales,fill=PILOTO ))+
  geom_hline(data = ordenesMes, aes(yintercept = mean(OrdenesMensuales), color = "red"))+
  scale_fill_brewer(palette="Spectral")+
  ggtitle("Entregas mensuales por piloto")
  


#Vehiculos
df %>% 
  group_by(UNIDAD) %>% 
  summarise(activos=n())
df %>% 
  group_by(UNIDAD) %>% 
  summarise(Porc_entregas=100*n()/nrow(df)) %>% 
  hchart("column", hcaes(x = UNIDAD, y = Porc_entregas)) %>%
  hc_title(text = "Distribución entregas por Vehículos")

faltantes=df %>% group_by(CLIENTE,UNIDAD) %>%
  summarise(Monto=sum(Q)) %>% 
  filter(str_detect(CLIENTE, "Faltante")==TRUE)

faltantes %>% 
  hchart('column', hcaes(x = 'CLIENTE', y = 'Monto', group = "UNIDAD")) %>% 
  hc_title(text = "Distribución entregas faltantes por tipo de vehículo")

##PARETO
clientes=df %>% 
  group_by(CLIENTE) %>% 
  summarise(monto=sum(Q),pedidos=n())
cliente=clientes$CLIENTE
cliente=gsub("\\|.*","",cliente)
cliente=gsub("\\/.*","",cliente)
clientes$CLIENTE=cliente
clientes=clientes %>% 
  group_by(CLIENTE) %>% 
  summarise(monto=sum(monto), pedidos=sum(pedidos))
clientes$monto=clientes$monto/100

ggplot(clientes) +
  geom_bar(aes(x=CLIENTE,y=pedidos), fill='blue', stat="identity") +
  geom_point(aes(x=CLIENTE,y=monto), color = rgb(0, 1, 0), pch=16, size=1) +
  geom_path(aes(x=CLIENTE,y=monto, group=1), colour="slateblue1", lty=3, size=0.9)+theme(axis.text.x = element_text(angle=90, vjust=0.6)) 
