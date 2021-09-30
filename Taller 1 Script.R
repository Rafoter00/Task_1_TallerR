#Task 1 Taller R
#Rafael Otero 201821640

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

#Pregunta 2:
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(rio,skimr,tidyverse,readxl,haven,WriteXLS) # Llamar y/o instalar las librerías de la clase
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

#Limpieza de datos
data_cultivos = import(file= "task_r_202102/task_1/data/input/cultivos.xlsx", col_names = TRUE, skip = 8) #Traer Base cultivos.xlsx
data_cultivos=data_cultivos[-c(352, 348, 341,333,302,291,272,236,221,216,212,207,200,195,188,160,151,122,105,99,86,69,63,9,353,354,355),]#Elimino Filas de Totales
data_cultivos = subset(data_cultivos, select = -c(CODDEPTO,CODMPIO))#Limpio codigos de departamento y municipio ( en mi opinion creo que los codigos son redundantes si ya tenemos nombres)
years = 1999:2019 %>% as.character() # Determino un vector de años
for(var in years){
data_cultivos[,var] = as.numeric(data_cultivos[,var])
 }# convierto las palabras a numericas
data_cultivos = data_cultivos %>% rename_with(tolower) #Pongo todos los nombres de columnas en minuscula
data_cultivos=data_cultivos %>% mutate(departamento = tolower(departamento))#Pongo en minusculas informacion de columna "departamento"
data_cultivos=data_cultivos %>% mutate(municipio = tolower(municipio))#Pongo en minuscula informacion de columna "municipio"

#Pivot a formato long
Pivotlong_datacultivos=data_cultivos %>% pivot_longer(!departamento:municipio,names_to="año",values_to="numero de hectareas")#Pivoteo unicamente los años dejando estatico el departamento y el municipio

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



