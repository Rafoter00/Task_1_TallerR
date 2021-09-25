#Task 1 Taller R

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


data_cultivos = import(file= "task_r_202102/task_1/data/input/cultivos.xlsx", col_names = TRUE, skip = 8) #Traer Base cultivos.xlsx
data_cultivos2<-data_cultivos[-c(352, 348, 341,333,302,291,272,236,221,216,212,207,200,195,188,160,151,122,105,99,86,69,63,9,353,354,355),]#Elimino Filas de Totales
data_cultivos2 %>%  select(starts_with(c("CODDEPTO","CODMPIO"))) %>% head()
data_cultivos_limpia = subset(data_cultivos2, select = -c(CODDEPTO,CODMPIO))#Limpio codigos de departamento y municipio
