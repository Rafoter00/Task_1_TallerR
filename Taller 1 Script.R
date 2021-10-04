#Task 1 Taller R
#Rafael Otero 201821640
#Maria Paula Alvarez 201820569
#Version de R: R version 4.1.0 (2021-05-18) ,Platform: x86_64-apple-darwin17.0 (64-bit), Running under: macOS Big Sur 11.5.2


#Taller 1
rm(list = ls())
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages(pacman)
require(pacman)
p_load(dplyr,data.table)
print

#Pregunta 1: Vectores
vector1_100=seq(1,100,1)#Vector de 1 a 100
is.numeric(vector1_100)

vector_impares= seq(1,99, 2)#Vector Impares
is.numeric(vector_impares)

vector_pares=vector1_100[-vector_impares] #Vector Pares

#Otra forma de hacerlo
vector=c(1:100)#Vector de 1 a 100

v_pares=vector[!vector %in% vector_impares]#Vector que contiene los datos "vector" que no están en el vector de numeros impares

#Pregunta 2:
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(rio,skimr,tidyverse,readxl,haven,WriteXLS) # Llamar y/o instalar las librerías de la clase
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

#Limpieza de datos
data_cultivos = import(file= "task_r_202102/task_1/data/input/cultivos.xlsx", col_names = TRUE, skip = 8) #Traer Base cultivos.xlsx

is.na(data_cultivos$MUNICIPIO) %>% table() #Revisar cuantas observaciones tienen NA
cultivos = data_cultivos %>% drop_na(MUNICIPIO) #Eliminar las observaciones que tengan NA en MUNICIPIO, estas corresponden a los totales por departamento
is.na(cultivos$MUNICIPIO) %>% table() #Verificar que no hayan quedado observaciones con NA en MUNICIPIO
cultivos = cultivos[-c(329),] #Eliminar la fila de referencias

cultivos = subset(cultivos, select = -c(CODDEPTO,CODMPIO))#Limpio codigos de departamento y municipio ( en mi opinion creo que los codigos son redundantes si ya tenemos nombres)


years = 1999:2019 %>% as.character() # Determino un vector de años
for(var in years){
  cultivos[,var] = as.numeric(cultivos[,var])
}# convierto las palabras a numericas

cultivos = cultivos %>% rename_with(tolower) #Pongo todos los nombres de columnas en minuscula
cultivos=cultivos %>% mutate(departamento = tolower(departamento))#Pongo en minusculas informacion de columna "departamento"
cultivos=cultivos %>% mutate(municipio = tolower(municipio))#Pongo en minuscula informacion de columna "municipio"

#Pivot a formato long

Pivotlong_cultivos=cultivos %>% pivot_longer(!departamento:municipio,names_to="año",values_to="numero de hectareas")#Pivoteo unicamente los años dejando estatico el departamento y el municipio

#Pregunta 3
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(rio,skimr,tidyverse,readxl,haven,WriteXLS) # Llamar y/o instalar las librerías de la clase
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

#Parte 3.1
cabecera_caracteristcas=import(file = "task_r_202102/task_1/data/input/2019/Cabecera - Caracteristicas generales (Personas).rds") #Importo caracteristicas generales
cabecera_desocupados=import(file = "task_r_202102/task_1/data/input/2019/Cabecera - Desocupados.rds") %>% mutate(desocupado=1)#Importo Desocupados
cabecera_ocupados=import(file = "task_r_202102/task_1/data/input/2019/Cabecera - Ocupados.rds")%>% mutate(ocupado=1)#Importo Ocupados 
cabecera_fuerzalaboral=import(file = "task_r_202102/task_1/data/input/2019/Cabecera - Fuerza de trabajo.rds")%>% mutate(fl=1) #Importo Fuerza laboral
cabecera_inactivos=import(file = "task_r_202102/task_1/data/input/2019/Cabecera - Inactivos.rds")%>% mutate(inactivo=1) #Importo inactivos laboral

geih = left_join(cabecera_caracteristcas,cabecera_desocupados,c("secuencia_p","orden","directorio"),suffix=c("","")) %>% left_join(.,cabecera_ocupados,c("secuencia_p","orden","directorio"),suffix=c("",""))  %>% left_join(.,cabecera_fuerzalaboral,c("secuencia_p","orden","directorio"),suffix=c("","")) %>% left_join(.,cabecera_inactivos,c("secuencia_p","orden","directorio"),suffix=c("","")) %>% select("secuencia_p","orden","directorio","P6020","P6040","P6920","INGLABO","P6050","dpto","fex_c_2011","ESC","mes","desocupado","fl","inactivo","ocupado")

#Con la funcion de arriba se pegaron las 5 bases de datos de cabecera mediante los identificadores "secuencia_p","orden","directorio". 
#Adicionalmente se dejaron las 8 variables de interes:"P6020","P6040","P6920","INGLABO","P6050","dpto","fex_c_2011","ESC","mes". 
#Por ultimo se dejaron tambien variables que permiten identificar: fl=1(nos dice que el individuo pertenece a la fuerza laboral),inactivo=1(nos dice que el individuo es inactivo),ocupado=1(Nos dice que el individuo es ocupado),descupado=1(nos dice que el individuo es desocupado).
#Podemos utilizar estas variables para el punto 3.2 si nos basamos en teoria economica. Por ejemplo: si el inidivudo pertenece a la fuerza laboral, y no es inactivo, debe ser ocupado o desocupado

#Parte 3.2
Total_ocupados_desocupado_edad= geih %>% group_by(P6040) %>% summarise(ocupado=table(ocupado),desocupado=table(desocupado))#Total de desocupados y ocupados por edad
Total_ocupados_desocupado_genero= geih %>% group_by(P6020) %>% summarise(ocupado=table(ocupado),desocupado=table(desocupado))#Total de desocupados y ocupados por genero
Total_ocupados_desocupado_ESC= geih %>% group_by(P6020,ESC) %>% summarise(ocupado=table(ocupado),desocupado=table(desocupado))#Total de desocupados y ocupados por genero y años de escolaridad

Ingreso_promedio_mediano_genero_edad= geih %>%  group_by(P6020,P6040)  %>% summarise(ingresos_promedio = mean(INGLABO, na.rm = TRUE),ingreso_mediano = median(INGLABO,na.rm= TRUE), varianza_ingreso=var(INGLABO,na.rm=TRUE)) %>% drop_na(ingresos_promedio) %>% subset(ingresos_promedio>0) #Ingreso Promedio y mediano por sexo y edad
Ingreso_promedio_mediano_genero_escolaridad= geih %>%  group_by(P6020,ESC)  %>% summarise(ingresos_promedio = mean(INGLABO, na.rm = TRUE),ingreso_mediano = median(INGLABO,na.rm= TRUE), varianza_ingreso=var(INGLABO,na.rm=TRUE)) %>% drop_na(ingresos_promedio) %>% subset(ingresos_promedio>0) #Ingreso Promedio y mediano por sexo y años de escolaridad


if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(tidyverse , rio , skimr , RColorBrewer , ggthemes , hrbrthemes , igraph) 


graph1 = ggplot(Total_ocupados_desocupado_edad) + geom_point(aes(x=P6040,y=ocupado,col="red")) #Grafica de dispersion edad vs ocupados
graph1
graph2 = ggplot(Total_ocupados_desocupado_edad) + geom_point(aes(x=P6040,y=desocupado)) #Grafica de dispersion edad vs desocupados
graph2
genero<-cut(Ingreso_promedio_mediano_genero_edad$P6020,breaks=c(0,1,2),labels=c("mujer","hombre"))
Ingreso_promedio_mediano_genero_edad$P6020=genero#Vuelvo variable P6020 categorica
Genero<-cut(Ingreso_promedio_mediano_genero_escolaridad$P6020,breaks=c(0,1,2),labels=c("mujer","hombre"))
Ingreso_promedio_mediano_genero_escolaridad$P6020=Genero #Vuelvo variable P6020 categorica
graph3 = ggplot(Ingreso_promedio_mediano_genero_edad)+geom_point(aes(x=P6040,y=ingresos_promedio,color=genero)) #Grafica de dispersion edad vs Ingreso promedio discriminado por genero
graph3
graph4 = ggplot(Ingreso_promedio_mediano_genero_escolaridad)+geom_point(aes(x=ESC,y=ingresos_promedio,color=Genero))+ theme_pander() + scale_colour_pander()#Grafica de dispersion años escolarid vs ingreso promedio discriminado por genero
graph4
graph5 = ggplot(data = Ingreso_promedio_mediano_genero_escolaridad , aes(x = ESC , y =ingreso_mediano , fill = Genero)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = ESC ),  
            position=position_dodge(width=0.9),  
            vjust=-0.25,
            size = 3.5) +
  scale_fill_brewer(palette = "Paired")+ 
  labs(title = "Ingreso Mediano divido por Escolaridad",
       caption = "Informacion obtenida de la GEIH",
       x = "Años de escolaridad", 
       y = "Ingreso Mediano", 
       fill = "Genero") + theme_bw()
#El ultimo comando es una graficos de barras que muestra el ingreso mediano segun los años de escolaridad discriminado por genero
graph5

#Guardar
ggsave(plot=graph1, file = "views/ejemplo1.jpeg")
ggsave(plot=graph2, file = "views/ejemplo2.jpeg")
ggsave(plot=graph3, file = "views/ejemplo3.jpeg")
ggsave(plot=graph4, file = "views/ejemplo4.jpeg")
ggsave(plot=graph5, file = "views/ejemplo5.jpeg")