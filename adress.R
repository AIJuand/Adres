#Importación de librerías y datos
setwd("C:\\Users\\juanc\\OneDrive\\Documents\\adrees")
getwd()
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
Municipios<- read_excel("Municipios.xlsx",col_names=TRUE)
Prestadores <- read_excel("Prestadores.xlsx",col_names=TRUE)
library(stringr)
library(tidyr)
library(lmtest)
library(car)

#-----------------------Exploración de datos-----------------------------------#
#-----------------------------Municipios---------------------------------------#
str(Municipios)
head(Municipios,n=10)
tail(Municipios,n=10)
summary(Municipios)
#limpieza de Municipios
Municipios <- within(Municipios,{
  Municipio2 <- str_to_lower(Municipio)
  Municipio2 <- str_replace_all(Municipio2,"[^a-záéíóúüñ ]","")
  Municipio2 <- str_squish(Municipio2)
  Municipio2 <- str_to_title(Municipio2)
})
head(Municipios[,c("Municipio","Municipio2")],n=10)

Municipios <- within(Municipios,{
  Municipio <- Municipio2
  rm(Municipio2)
})
#----------------------#
Municipios <- within(Municipios,{
  Departamento2 <- str_to_lower(Departamento)
  Departamento2 <- str_replace_all(Departamento2,"[^a-záéíóúüñ ]","")
  Departamento2 <- str_squish(Departamento2)
  Departamento2 <- str_to_title(Departamento2)
})
head(Municipios[,c("Departamento","Departamento2")],n=10)

Municipios <- within(Municipios,{
  Departamento <- Departamento2
  rm(Departamento2)
})
#------------------------missings------------------------------------------#
missings <- function(x) return(sum(is.na(x)))
apply(Municipios,2,missings)
#------Hay uno, se busca el valor y se remplaza----------#
Municipios %>%
  filter(is.na(Superficie))
Municipios <- Municipios %>%
  mutate(Superficie = if_else(Municipio == "Mapiripana", 6457, Superficie))
#-----------se verifica el cambio-------------------#
print(Municipios %>% filter(Municipio == "Mapiripana"))
#------------------------------------Prestadores-------------------------------#
str(Prestadores)
head(Prestadores,n=10)
tail(Prestadores,n=10)
summary(Prestadores)
apply(Prestadores,2,missings)
#hay varias columnas que tienen un alto porcentaje de missings, por lo tanto:
#aquellas que tengan la mitad o más de missings se eliminan
missing_pct <- colMeans(is.na(Prestadores)) * 100
Prestadores <- Prestadores[, missing_pct <= 50]
#dv es una variables con un gran porcentaje de ceros y NA y no parece aportar
#por tanto se elimina esa variable
Prestadores <- subset(Prestadores, select = c(-dv, -habilitado))
names(Prestadores)

# -------------------------------Análisis descriptivos---------------------#
#-------------Municipios----------------#
source("mismacros.txt")
#tabla de frecuencias de los departtamentos y sus municipios
mytable(~ Departamento,data=Municipios,ord="freq" )
mytable(~ depa_nombre,data=Prestadores,ord="freq" )
names(Municipios)
names(Prestadores)
#------------usando SQLite en R-------------------------#
#install.packages("RSQLite")
library(RSQLite)
# Establecer conexión
Adrees <- dbConnect(RSQLite::SQLite(), "Adrees_Prueba.sqlite")
# Tabla municipios
dbWriteTable(Adrees, "municipios", Municipios)
# Estadísticas descriptivas municipios
consulta_municipios <- "SELECT Departamento, COUNT(*) as Num_Municipios, AVG(Superficie) as Promedio_Superficie,MIN(Superficie) as Min_Superficie ,MAX(Superficie) as Max_Superficie
                        FROM municipios
                        GROUP BY Departamento
                        ORDER BY Num_Municipios DESC"

resultados_municipios <- dbGetQuery(Adrees, consulta_municipios)
print(resultados_municipios)

# Tabla prestadores
dbWriteTable(Adrees, "prestadores", Prestadores)
# Estadísticas descriptivas prestadores
consulta_prestadores <- "SELECT depa_nombre, COUNT(*) as Num_Prestadores,
                                 SUM(CASE WHEN clase_persona = 'NATURAL' THEN 1 ELSE 0 END) as Persona_Natural,
                                 SUM(CASE WHEN clase_persona = 'JURIDICO' THEN 1 ELSE 0 END) as Persona_Juridica
                         FROM prestadores
                         GROUP BY depa_nombre
                         ORDER BY Num_Prestadores DESC"
resultados_prestadores <- dbGetQuery(Adrees, consulta_prestadores)
#reemplazamos absolutos por porcentajes
resultados_prestadores$Persona_Natural <- (resultados_prestadores$Persona_Natural / resultados_prestadores$Num_Prestadores) * 100
resultados_prestadores$Persona_Juridica <- (resultados_prestadores$Persona_Juridica / resultados_prestadores$Num_Prestadores) * 100
print(resultados_prestadores)
#Desconectar
dbDisconnect(Adrees)

#-------------------------------Visualización de datos Municipios-------------------------#
#porcentaje de la superficie en cada categoria de la variable Region.
Supe <- Municipios %>%
  group_by(Region) %>% 
  summarise(freq=sum(Superficie,na.rm=TRUE)) %>% 
  mutate(percent=round(100*freq/sum(freq),digits=1),
         labels=paste0(str_replace(Region,"Región ",""),"\n (",percent,"%)"))


ggplot(Supe,aes(x="",y=percent,fill=Region)) +
  geom_col(color="black",linetype="solid") +
  geom_text(aes(label=labels),position=position_stack(vjust=0.5),size=5.2,fontface="bold") +
  coord_polar(theta="y") +
  labs(title="Distribución de la Superficie por Región") + 
  scale_fill_brewer(palette="RdBu") + 
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        panel.background=element_rect(fill="gray92"),legend.position="none",
        plot.title=element_text(family="sans",face="bold",size=22,
                                vjust=0.5,hjust=0.5,color="black"))
#------------------------------------------------------------------------------#
Pobl <- Municipios %>% 
  group_by(Region) %>% 
  summarise(freq=sum(Poblacion)) %>% 
  mutate(percent=round(100*freq/sum(freq),digits=1),
         labels=paste0(str_replace(Region,"Región ",""),"\n (",percent,"%)"))
ggplot(Pobl,aes(x="",y=percent,fill=Region)) +
  geom_col(color="black",linetype="solid") +
  geom_text(aes(label=labels),position=position_stack(vjust=0.5),size=5.2,fontface="bold") +
  coord_polar(theta="y") +
  labs(title="Distribución de la Población por Región") + 
  scale_fill_brewer(palette="RdBu") + 
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        panel.background=element_rect(fill="gray92"),legend.position="none",
        plot.title=element_text(family="sans",face="bold",size=22,
                                vjust=0.5,hjust=0.5,color="black"))


# Gráfico de Población Total por Departamento y Región
ggplot(Municipios, aes(x = reorder(Departamento, -Poblacion), y = Poblacion, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Población Total por Departamento y Región", x = "Departamento", y = "Población Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------Visualización de datos prestadores-------------------------#


# Calcular el número total de prestadores por departamento
total_prestadores <- aggregate(Prestadores$clase_persona, by = list(Prestadores$depa_nombre), FUN = length)

# Ordenar los departamentos de mayor a menor según el número total de prestadores
departamentos_ordenados <- total_prestadores[order(-total_prestadores$x), ]

# Convertir los nombres de los departamentos a factor y ordenarlos según la nueva clasificación
Prestadores$depa_nombre <- factor(Prestadores$depa_nombre, levels = departamentos_ordenados$Group.1)

# Graficar el número de prestadores por departamento, ordenados de mayor a menor
ggplot(Prestadores) + 
  geom_bar(aes(x = depa_nombre, fill = clase_persona), position = position_dodge(), width = 0.75) + 
  scale_fill_manual(values = c("yellow", "blue")) + 
  labs(title = "Número de Prestadores por Departamento", y = "Número", x = "Departamento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(color = "red"))



#_-------------------------------------------------------------------------------#
# Calcular el número total de prestadores por departamento
total_prestadores <- aggregate(Prestadores$clase_persona, by = list(Prestadores$depa_nombre), FUN = length)

# Ordenar los departamentos de mayor a menor según el número total de prestadores
departamentos_ordenados <- total_prestadores[order(-total_prestadores$x), ]

# Convertir los nombres de los departamentos a factor y ordenarlos según la nueva clasificación
Prestadores$depa_nombre <- factor(Prestadores$depa_nombre, levels = departamentos_ordenados$Group.1)

# Crear un gráfico de barras apiladas horizontal
ggplot(Prestadores, aes(x = depa_nombre, fill = clase_persona)) +
  geom_bar(position = "stack", width = 0.6) +
  labs(title = "Distribución de Prestadores por Departamento", y = "Número de Prestadores", x = "Departamento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(color = "red"))

#-------------------------------Comparación clpr y naju
ggplot(Prestadores) +
  geom_bar(aes(x = naju_nombre, fill = clpr_nombre), position = "dodge") +
  labs(title = "Comparación de clpr_nombre y naju_nombre",
       x = "naju_nombre",
       fill = "clpr_nombre") +
  theme_minimal()
# Modelamiento estadístico, u
#----------------------------Modelamiento municipios---------------------------#
names(Municipios)
# Instalar y cargar el paquete FactoMineR si aún no lo tienes instalado
# install.packages("FactoMineR")
library(FactoMineR)
# Realizar el análisis de componentes principales
pca_result <- PCA(Municipios[, c("Poblacion", "Superficie")], graph = FALSE)

# Ver los resultados del análisis
summary(pca_result)

# Graficar el biplot (si deseas visualizar los resultados)
plot(pca_result, choix = "var")
#-----------------------------------------regresion municipios-----------------#
library(lme4)

# Ajustar el modelo de efectos mixtos
modelo_mixto <- lmer(Poblacion ~ Superficie + (1 | Departamento), data = Municipios)

# Mostrar un resumen del modelo
summary(modelo_mixto)
#----------------------modelamiento Prestadores--------------------------------#
names(Prestadores)
Prestadores$fecha_radicacion <- as.Date(as.character(Prestadores$fecha_radicacion), format = "%Y%m%d")
Prestadores$fecha_vencimiento <- as.Date(as.character(Prestadores$fecha_vencimiento), format = "%Y%m%d")
# Convertir la variable 'clase_persona' a factor
Prestadores$Clase_Persona <- as.factor(Prestadores$clase_persona)
Prestadores$Clase_Persona <- as.numeric(Prestadores$clase_persona == "JURIDICO")

# Cargar la biblioteca necesaria
library(MASS)

# Convertir variables categóricas en factores
Prestadores$depa_nombre <- as.factor(Prestadores$depa_nombre)
Prestadores$muni_nombre <- as.factor(Prestadores$muni_nombre)
Prestadores$codigo_habilitacion <- as.factor(Prestadores$codigo_habilitacion)
# Continúa con el resto de las variables categóricas

# Ajustar el modelo de regresión logística
modelo_stepwise <- glm(clase_persona ~ depa_nombre + clpr_codigo + naju_codigo, data = Prestadores, family = binomial)


# Realizar selección de variables stepwise si es necesario
modelo_stepwise <- stepAIC(modelo_logistico, direction = "both")

# Resumen del modelo
summary(modelo_stepwise)
table(Prestadores$clase_persona)


levels(Prestadores$depa_nombre)
