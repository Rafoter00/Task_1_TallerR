#Task 1 Taller R

rm(list = ls())
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages(pacman)
require(pacman)
p_load(dplyr,data.table)
print

#Pregunta 1: Vectores
vector1_100=seq(1,100,1)
is.numeric(vector1_100)

vector_impares= seq(1,99, 2)
is.numeric(vector_impares)

vector_pares=vector1_100[-vector_impares]

#Pregunta 2:
