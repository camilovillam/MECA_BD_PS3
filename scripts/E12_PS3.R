#Big Data and Machine Learning for Applied Economics
#MEcA - Uniandes
#Problem Set 3
#
#Equipo 12

#Jorge E. García
#Ingrid Lorena Molano
#Camilo Villa Moreno

#Julio 26, 2022

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. PRELIMINARES: PREPARACIÓN ESPACIO DE TRABAJO Y LIBRERÍAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##Limpiar el entorno ----
rm(list=ls())


##Carga de librerías ----
install.packages("pacman")
library(pacman)

p_load(rio,
       sf,
       doParallel,
       gtsummary,
       GGally,
       stargazer,
       fabricatr,
       tableone,
       arsenal,
       janitor,
       tidyverse,
       gamlr,
       skimr, 
       caret,
       rvest,
       stargazer,
       smotefamily,
       MASS,
       ROCR,
       pROC,
       rpart,
       rpart.plot,
       glmnet,
       xgboost)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. CARGUE DE LAS BASES DE DATOS Y EXPLORACIÓN INICIAL----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


setwd("~/GitHub/MECA_BD_PS3")

train_prop <-readRDS("./stores/train.rds")
test_prop <-readRDS("./stores/test.rds")



##1.2. Exploración incial de los datos ----


view(train_prop)
view(test_prop)

table(train_prop$l3)
table(train_prop$property_type)
table(train_prop$operation_type)
table(train_prop$currency)
table(train_prop$ad_type)


table(test_prop$l3)
table(test_prop$property_type)
table(test_prop$operation_type)
table(test_prop$currency)
table(test_prop$ad_type)




#Exploración de las bases de datos:
skim(train_prop)
skim(test_prop)

#Comparación de variables (columnas) entre las dos bases de datos:
compare_df_cols(train_prop, test_prop)

#Resumen de diferencias
all_equal(train_prop, test_prop)


# Las variables son las mismas, excepto el Asking Price (Y) que no está
# en la de Test (esperable).
# 
# De acuerdo con Skim, las variables con mayor cantidad de NAs son:
#


# Var		        Complete Rate (Train)		    Complete rate (test)
# -------------------------------------------------------------------------
# rooms  		 	      0.502				            0.515			
# bathrooms 	  	  0.720				            0.617
# surface_total 	  0.258				            0.185      
# surface_covered   0.188				            0.103
# description       0.999	(70 NA)		      	0.999 	(16 NA)
# title             1.00 	(35 NA)        		1 	    (0 NA)


#surface_total, surface covered, rooms son prácticamente inutilizables.
#Hay que revisar si en la descripción, por análisis de texto, está la info.


#Se deben completar mínimo 2 variables geográficas:

#Ej. Distancia estación Metro / Transmilenio / Parada de bus(?).
#Ej. Distancia al centro de la ciudad.


#Se deben agregar mínimo dos variables por procesamiento de texto a partir de la descripción:
# Ej: Disponibilidad ascensor
# Ej: Parqueaderos, número de parqueaderos
# Ej: Área total, superficie (ver NAs, comentarios anteriores).
# Ej: Número de piso para el caso de apartamentos (prima de altura)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. ENSAYO: MAPA Y MEDICIÓN DE DISTANCIA ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Cargar las ciclovias (solo para ensayar)

ciclovias <- read_sf("./stores/Ciclovia/Ciclovia.shp")

ggplot()+
  geom_sf(data=ciclovias) +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))


upla<-read_sf("./stores/upla/UPla.shp")


sitios_ref <- data.frame(place="Uniandes",
               lat=4.601590,
               long=-74.066391,
               nudge_y=-0.001)

sitios_ref <- sitios_ref %>% mutate(latp=lat,longp=long)

sitios_ref <- st_as_sf(sitios_ref,coords=c('longp','latp'),crs=4326)


#Graficar Bogotá con las ciclovías en azul y Uniandes como punto de referencia:

ggplot()+
  geom_sf(data=upla
          %>% filter(grepl("RIO",UPlNombre)==FALSE),
          fill = NA) +
  geom_sf(data=sitios_ref, col="red") +
  geom_sf(data=ciclovias, col="blue") +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))


#Ejemplo de 6 sitios de la base de datos de Train (subset):

prop_subset <- head(train_prop)

prop_subset <- prop_subset %>% mutate(latp=lat,longp=lon)
prop_subset <- st_as_sf(prop_subset,coords=c('longp','latp'),crs=4326)


#Medición de distancias de las propiedades a Uniandes:

prop_subset$dist_a_Uniandes <-st_distance(prop_subset,sitios_ref)



#Se grafican las 6 propiedades en el mapa (junto con ciclovías y Uniandes, por ensayar)

ggplot()+
  geom_sf(data=upla
          %>% filter(grepl("RIO",UPlNombre)==FALSE),
          fill = NA) +
  geom_sf(data=sitios_ref, col="red") +
  geom_sf(data=ciclovias, col="blue") +
  geom_sf(data=prop_subset, col="orange") +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))
