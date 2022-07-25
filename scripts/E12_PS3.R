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
       sf, # Leer, escribir o manipular datos espaciales
       leaflet, #Visualizaciones dinámicas
       tmaptools, #geocode_OSM()
       osmdata,  #Get OSM´s data
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

train_prop <-readRDS("./stores/train.rds") #107.567 Obs
test_prop <-readRDS("./stores/test.rds") #11.150 Obs



##1.1. Exploración incial de los datos ----


view(train_prop)
view(test_prop)

table(train_prop$l3) #Bogotá 86.211 Medellín: 21.356
table(train_prop$property_type) #Apartamento 81.577 Casa:25.990
table(train_prop$operation_type) #venta: total obs 107.567
table(train_prop$currency) #COP: total obs 107.567
table(train_prop$ad_type) #Propiedad: total obs 107.567


table(test_prop$l3) #Bogotá 793 Medellín: 10.357
table(test_prop$property_type) #Apartamento 9.658 Casa:1.492
table(test_prop$operation_type) #venta: total obs 11.150
table(test_prop$currency) #COP: total obs 11.150
table(test_prop$ad_type) #Propiedad: total obs 11.150


#Las variables operation_type, currency y as_type no dan info relevante.

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

##1.2. Graficas NAs----
##NAs Base Train ---- 
#Para sacar la gráfica de NAs

cantidad_na <- sapply(train_prop, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(train_prop)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 11.11% de las entradas están vacías"

#Se visualiza el porcentaje de observaciones faltantes por variable

# se ordena de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))

# se convierte el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# # se quitan las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))
# 
#Las variables sin NAs son: property_id, ad_type, start_date, end_date, 
#created_on, lat, lon, l1, l2, l3, bedrooms, price, currency, property_type, 
#operation_type

porcentaje_na <- porcentaje_na[!filtro,]
# 
orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]

porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden)


# Se grafica el % de NA de las diferentes variables de interés
ggplot(porcentaje_na[1:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

##NAs Base Test ---- 
#Para sacar la gráfica de NAs

cantidad_na <- sapply(test_prop, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(test_prop)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 12.91% de las entradas están vacías"

#Se visualiza el porcentaje de observaciones faltantes por variable

# se ordena de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))

# se convierte el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# # se quitan las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))
# 
#Las variables sin NAs son: property_id, ad_type, start_date, end_date, 
#created_on, lat, lon, l1, l2, l3, bedrooms, currency, property_type, 
#operation_type

porcentaje_na <- porcentaje_na[!filtro,]# 

orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]

porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden)


# Se grafica el % de NA de las diferentes variables de interés
ggplot(porcentaje_na[1:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. SEPARAR LAS BASES/ BOGOTÁ D.C. Y MEDELLÍN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Base de Bogotá D.C.
train_bog <-subset(train_prop,train_prop$l3 =="Bogotá D.C")
test_bog <-subset(test_prop,test_prop$l3 =="Bogotá D.C")                   


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. VARIABLES DE TEXTO E IMPUTACIÓN DE DATOS ----

##3.1. Variables de texto----

##3.2. Imputación de datos----

##3.3. Definición bases datos definitivas ----

# 4. ENSAYO: MAPA Y MEDICIÓN DE DISTANCIA ----
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



#Cargar localidades:

loc_bog <- read_sf("./stores/localidades_Bog_shp/Loca.shp")


#Se transforma toda la base de datos de Train y test:

train_prop <- train_prop %>% mutate(latp=lat,longp=lon)
train_prop <- st_as_sf(train_prop ,coords=c('longp','latp'),crs=4326)

test_prop <- test_prop %>% mutate(latp=lat,longp=lon)
test_prop <- st_as_sf(test_prop ,coords=c('longp','latp'),crs=4326)


#Validar los sistemas de coordenadas de los objetos:

st_crs(ciclovias)
st_crs(prop_subset)
st_crs(sitios_ref)
st_crs(upla)
st_crs(loc_bog)
st_crs(train_prop)

ciclovias
prop_subset
sitios_ref
upla
loc_bog
train_prop

#Transformar todos los sistemas de coordenadas a 4326
ciclovias<-st_transform(ciclovias, 4326)
prop_subset<-st_transform(prop_subset, 4326)
sitios_ref<-st_transform(sitios_ref, 4326)
upla<-st_transform(upla, 4326)
loc_bog<-st_transform(loc_bog, 4326)



#Se grafican las propiedades en el mapa (junto con ciclovías y Uniandes, por ensayar)

ggplot()+
  geom_sf(data=upla
          %>% filter(grepl("RIO",UPlNombre)==FALSE),
          fill = NA) +
  geom_sf(data=loc_bog
          %>% filter(grepl("CHAPINERO",LocNombre)==TRUE),
          fill = "gray")+
  geom_sf(data=sitios_ref, col="red") +
  geom_sf(data=ciclovias, col="blue") +
  geom_sf(data=prop_subset, col="green") +
  geom_sf(data=test_prop
          %>% filter(grepl("Cundinamarca",l2)==TRUE),
          col="orange",
          size=1) +
    theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))


#Solo Chapinero:

ggplot()+
  geom_sf(data=loc_bog
          %>% filter(grepl("CHAPINERO",LocNombre)==TRUE),
          fill = NA)+
  geom_sf(data=test_prop
          %>% filter(grepl("Cundinamarca",l2)==TRUE),
          col="orange",
          size=1) +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))


#Ensayos con Leaflet:

leaflet() %>% addTiles() %>% addCircleMarkers(data=test_prop)
leaflet() %>% addTiles() %>% addCircles(data=test_prop)
leaflet() %>% addTiles() %>% addPolylines(data=ciclovias)
leaflet() %>% addTiles() %>% addPolygons(data=loc_bog)

#Solo Chapinero

leaflet() %>% addTiles() %>% addPolygons(data=loc_bog%>% filter(grepl("CHAPINERO",LocNombre)==TRUE))


#Propiedades de Test

leaflet() %>% addTiles() %>% 
  addPolygons(data=loc_bog %>% 
                filter(grepl("CHAPINERO",LocNombre)==TRUE)) %>% 
  addCircleMarkers(data=test_prop %>%
                     filter(grepl("Cundinamarca",l2)==TRUE),
                              col="red")


#Ensayos adicionales:

#Buscar sitio por nombre

uniandes <- geocode_OSM("Universidad de los Andes, Bogotá", as.sf=T)
uniandes

leaflet() %>% addTiles() %>% addCircles(data=uniandes)
uniandes <- st_transform(uniandes, 4326)


#Features:

available_features() %>% head(50)


#Estaciones de metro en Medellín:

## objeto osm
metromed  <-  opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="public_transport" , value="station") 

metromed_sf <- metromed %>% osmdata_sf()
metromed_sf
metromed_station  <-  metromed_sf$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=metromed_station)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. MODELO BOGOTÁ D.C. ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##5.0. subir base y prepararla ----

setwd("~/GitHub/MECA_BD_PS3")

train_prop <-readRDS("./stores/train.rds") #107.567 Obs
test_prop <-readRDS("./stores/test.rds") #11.150 Obs

#Se transforma toda la base de datos de Train y test:

train_prop <- train_prop %>% mutate(latp=lat,longp=lon)
train_prop <- st_as_sf(train_prop ,coords=c('longp','latp'),crs=4326)

test_prop <- test_prop %>% mutate(latp=lat,longp=lon)
test_prop <- st_as_sf(test_prop ,coords=c('longp','latp'),crs=4326)

#Base de Bogotá D.C.
train_bog <-subset(train_prop,train_prop$l3 =="Bogotá D.C")
test_bog <-subset(test_prop,test_prop$l3 =="Bogotá D.C") 

##5.1. Información Bogotá ----

#– At least 2 of these models should include predictors coming from external
#sources; both can be from open street maps.
#At least 2 predictors coming from the title or description of the properties.

###5.1.1 Información Datos Abiertos Bogotá y DANE ----

##=== Subir los shapes ===##

loc_bog <- read_sf("./stores/bogota/localidades_Bog_shp/Loca.shp") # Cargar localidades de Bogotá D.C.
upz_bog <-read_sf("./stores/Bogota/upla/UPla.shp") # Subir las UPZ de Bogotá D.C.
ciclo_bog <-read_sf("./stores/Bogota/Ciclovia/Ciclovia.shp") # Subir las ciclovias de Bogotá D.C.
monu_bog <-read_sf("./stores/Bogota/monumentos/Monumentos.shp") # Subir los monumentos de Bogotá D.C.
parques_bog <-read_sf("./stores/Bogota/Parques/parques.shp") # Subir los parques de Bogotá D.C.
sector_bog <-read_sf("./stores/Bogota/sector/SECTOR.shp") # Subir el sector catastral de Bogotá D.C.
leg_bog <-read_sf("./stores/Bogota/barriolegalizado/BarrioLegalizado.shp") # Subir los barrios legalizados de Bogotá D.C.
teatro_bog <-read_sf("./stores/Bogota/teatroauditorio/TeatroAuditorio.shp") # Subir los teatros/auditorios de Bogotá D.C.
seg_bog <-read_sf("./stores/Bogota/indiceseguridadnocturna/IndiceSeguridadUPZ.shp") # Subir los índices de Seguridad por UPZ de Bogotá D.C.
metro_bog <-read_sf("./stores/Bogota/estacionesMetro/ESTACIONES.shp") # Subir las estaciones de Metro de Bogotá D.C.
mz_bog <-read_sf("./stores/Bogota/manz/MANZ.shp") # Subir las manzanas de Bogotá D.C.
avaluo_bog <-read_sf("./stores/Bogota/avaluo_manzana/Avaluo_Manzana.shp") # Subir el avaluo de las manzanas de Bogotá D.C.
estrato_bog <-read_sf("./stores/Bogota/manzanaestratificacion/ManzanaEstratificacion.shp") # Subir el avaluo de las manzanas de Bogotá D.C.
Dane_mz_bog <-read_sf("./stores/Bogota/DANE/MGN_URB_MANZANA.shp")

#Transformar todos los sistemas de coordenadas a 4326
loc_bog    <-st_transform(loc_bog, 4326)
upz_bog    <-st_transform(upz_bog, 4326)
ciclo_bog  <-st_transform(ciclo_bog, 4326)
monu_bog   <-st_transform(monu_bog, 4326)
parques_bog<-st_transform(parques_bog, 4326)
sector_bog <-st_transform(sector_bog, 4326)
leg_bog    <-st_transform(leg_bog, 4326)
teatro_bog <-st_transform(teatro_bog, 4326)
seg_bog    <-st_transform(seg_bog, 4326)
metro_bog  <-st_transform(metro_bog, 4326)
mz_bog     <-st_transform(mz_bog, 4326)
avaluo_bog <-st_transform(avaluo_bog, 4326)
estrato_bog <-st_transform(estrato_bog, 4326)
Dane_mz_bog <-st_transform(Dane_mz_bog, 4326)

#Se corrigen geometrias

#para manzanas
mz_bog$geom_err <- st_is_valid(mz_bog, reason = T)
nrow(mz_bog)
table(mz_bog$geom_err)

mz_bog <- st_make_valid(mz_bog)

mz_bog$geom_err <- st_is_valid(mz_bog, reason = T)
nrow(mz_bog)
table(mz_bog$geom_err)

mz_bog<- filter(mz_bog,mz_bog$geom_err == "Valid Geometry")
nrow(mz_bog)


#Remover geometrías vacías:
table(st_is_empty(mz_bog))
mz_bog <- mz_bog %>% filter(!st_is_empty(.))

#para avaluo
avaluo_bog$geom_err <- st_is_valid(avaluo_bog, reason = T)
nrow(avaluo_bog)
table(avaluo_bog$geom_err)

avaluo_bog <- st_make_valid(avaluo_bog)

avaluo_bog$geom_err <- st_is_valid(avaluo_bog, reason = T)
nrow(avaluo_bog)
table(avaluo_bog$geom_err)

avaluo_bog <- filter(avaluo_bog,avaluo_bog$geom_err == "Valid Geometry")
nrow(avaluo_bog)

#Remover geometrías vacías:
table(st_is_empty(avaluo_bog))
avaluo_bog <- avaluo_bog %>% filter(!st_is_empty(.))


#para estrato
estrato_bog$geom_err <- st_is_valid(estrato_bog, reason = T)
nrow(estrato_bog)
table(estrato_bog$geom_err)

estrato_bog<- st_make_valid(estrato_bog)

estrato_bog$geom_err <- st_is_valid(estrato_bog, reason = T)
nrow(estrato_bog)
table(estrato_bog$geom_err)

estrato_bog<- filter(estrato_bog,estrato_bog$geom_err == "Valid Geometry")
nrow(estrato_bog)

#Remover geometrías vacías:
table(st_is_empty(estrato_bog))
estrato_bog <- estrato_bog %>% filter(!st_is_empty(.))


#para manzanas dane
Dane_mz_bog$geom_err <- st_is_valid(Dane_mz_bog, reason = T)
nrow(Dane_mz_bog)
table(Dane_mz_bog$geom_err)

Dane_mz_bog <- st_make_valid(Dane_mz_bog)

Dane_mz_bog$geom_err <- st_is_valid(Dane_mz_bog, reason = T)
nrow(Dane_mz_bog)
table(Dane_mz_bog$geom_err)

Dane_mz_bog <- filter(Dane_mz_bog,Dane_mz_bog$geom_err == "Valid Geometry")
nrow(Dane_mz_bog)

#Remover geometrías vacías:
table(st_is_empty(Dane_mz_bog))
Dane_mz_bog <- Dane_mz_bog %>% filter(!st_is_empty(.))


#Primera prueba join para train
sf_use_s2(TRUE)

train_bog <- st_join(train_bog,loc_bog[,c('LocCodigo','LocNombre')])
train_bog <- st_join(train_bog,upz_bog[,c('UPlCodigo','UPlNombre')])
train_bog <- st_join(train_bog,seg_bog[,c('t_puntos','p_upl')])
train_bog <- st_join(train_bog,mz_bog[,c('MANCODIGO','SECCODIGO')])
train_bog <- st_join(train_bog,avaluo_bog[,c('MANZANA_ID','GRUPOP_TER','AVALUO_COM','AVALUO_CAT')])
train_bog <- st_join(train_bog,estrato_bog[,c('ESTRATO')])
train_bog <- st_join(train_bog,Dane_mz_bog[,c('MANZ_CAG')])

#Primera prueba join para test
test_bog <- st_join(test_bog,loc_bog[,c('LocCodigo','LocNombre')])
test_bog <- st_join(test_bog,upz_bog[,c('UPlCodigo','UPlNombre')])
test_bog <- st_join(test_bog,seg_bog[,c('t_puntos','p_upl')])
test_bog <- st_join(test_bog,estrato_bog[,c('ESTRATO')])
test_bog <- st_join(test_bog,mz_bog[,c('MANCODIGO','SECCODIGO')])
test_bog <- st_join(test_bog,avaluo_bog[,c('MANZANA_ID','GRUPOP_TER','AVALUO_COM','AVALUO_CAT')])

skim(train_bog)
skim(test_bog)

##Prueba vecinos ajustes manzanas ----

#Para train:


#Dividir Train entre las que sí encontró manzana y los NA (al final se unen)
train_bog_mz_ok <- filter(train_bog,!(is.na(train_bog$MANCODIGO)))
train_bog_mz_na <- filter(train_bog,is.na(train_bog$MANCODIGO))

train_bog_mz_na$MANCODIGO <- NULL

train_bog_mz_na_muestra <- train_bog_mz_na[1:20, ]

install.packages("nngeo")
library(nngeo)
start_train = Sys.time()
train_bog_mz_na_muestra <- st_join(train_bog_mz_na_muestra,mz_bog[,c('MANCODIGO')],
                             join = st_nn, k = 1, maxdist = 50, progress=TRUE)
end_train = Sys.time()
end_train - start_train

colSums(is.na(train_bog_mz_na))

install.packages("sfheaders")
library(sfheaders)

train_bog_mz_na_df <- sf_to_df(train_bog_mz_na, fill = TRUE, unlist = NULL)
saveRDS(train_bog_mz_na_df,"./stores/Bogota/rds_calculados/MZ_train_NA.rds")

colSums(is.na(train_bog))
train_bog <- rbind(train_bog_mz_ok,train_bog_mz_na)
colSums(is.na(train_bog))

nrow(train_bog)

train_bog_mz_df <- sf_to_df(train_bog, fill = TRUE, unlist = NULL)
saveRDS(train_bog_mz_df,"./stores/Bogota/rds_calculados/mz_train.rds")

colSums(is.na(train_bog))

###5.1.2 Información de OpenSteetMap ----

#### 5.1.2.1 Estaciones transporte público Bogotá: ----

## objeto osm
tpublbog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="public_transport" , value="station") 

tpublbog_sf <- tpublbog %>% osmdata_sf()
tpublbog_sf
tpublbog_station  <-  tpublbog_sf$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=tpublbog_station)

## Distancia de las viviendas a las estaciones
dist_tpublb_test  <-  st_distance(x=test_bog, y=tpublbog_station)
dist_tpublb_train  <-  st_distance(x=train_bog, y=tpublbog_station)

## Distancia mínima
min_dist_tpublb_test  <-  apply(dist_tpublb_test,1,min)
min_dist_tpublb_train  <-  apply(dist_tpublb_train,1,min)

test_bog$dist_tpubl <- min_dist_tpublb_test
train_bog$dist_tpubl <- min_dist_tpublb_train


#### 5.1.2.2 Hospitales y clínicas en Bogotá: ----

## objeto osm
hospbog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="hospital")

clinbog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="clinic") 

hospbog_sf <- hospbog %>% osmdata_sf()
clinbog_sf <- clinbog %>% osmdata_sf()

hospbog_sf
clinbog_sf

hosp_bog  <-  hospbog_sf$osm_points
clin_bog  <-  clinbog_sf$osm_points

colnames(hosp_bog)
colnames(clin_bog)
compare_df_cols(hosp_bog, clin_bog)

nrow(hosp_bog)
nrow(clin_bog)
nrow(hosp_bog) + nrow(clin_bog)

#Se unen las filas por columnas comunes de clínicas y hospitales
hosp_bog <- rbind(hosp_bog[intersect(colnames(hosp_bog), colnames(clin_bog))],
                  clin_bog[intersect(colnames(hosp_bog), colnames(clin_bog))])

nrow(hosp_bog)
colnames(hosp_bog)

#leaflet() %>% addTiles() %>% 
#  addCircleMarkers(data=hosp_bog,color="blue")

## Distancia de las viviendas a clínicas u hospitales
dist_hospb_test  <-  st_distance(x=test_bog, y=hosp_bog)
dist_hospb_train  <-  st_distance(x=train_bog, y=hosp_bog)

## Distancia mínima
min_dist_hospb_test  <-  apply(dist_hospb_test,1,min)
min_dist_hospb_train  <-  apply(dist_hospb_train,1,min)

test_bog$dist_hosp <- min_dist_hospb_test
train_bog$dist_hosp <- min_dist_hospb_train

#### 5.1.2.3 Centros comerciales en Bogotá: ----

## objeto osm
ccombog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="shop" , value="mall") 

ccombog_sf <- ccombog %>% osmdata_sf()
ccomerc_bog  <-  ccombog_sf$osm_points

#leaflet() %>% addTiles() %>% addCircleMarkers(data=ccomerc_bog)

## Distancia de las viviendas a Centros comerciales
dist_ccomercb_test  <-  st_distance(x=test_bog, y=ccomerc_bog)
dist_ccomercb_train  <-  st_distance(x=train_bog, y=ccomerc_bog)

## Distancia mínima
min_dist_ccomercb_test  <-  apply(dist_ccomercb_test,1,min)
min_dist_ccomercb_train  <-  apply(dist_ccomercb_train,1,min)

test_bog$dist_ccomerc <- min_dist_ccomercb_test
train_bog$dist_ccomerc <- min_dist_ccomercb_train


#### 5.1.2.4. Parques en Bogotá: ----

## objeto osm
parkbog  <-  opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="leisure" , value="park")

parkbog_sf <- parkbog %>% osmdata_sf()
park_bog  <-  parkbog_sf$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=park_bog)

## Distancia de las viviendas a parques
dist_parkb_test  <-  st_distance(x=test_bog, y=park_bog)
dist_parkb_train  <-  st_distance(x=train_bog, y=park_bog)

## Distancia mínima
min_dist_parkb_test  <-  apply(dist_parkb_test,1,min)
min_dist_parkb_train  <-  apply(dist_parkb_train,1,min)

test_bog$dist_park <- min_dist_parkb_test
train_bog$dist_park <- min_dist_parkb_train


colSums(is.na(train_bog))
colSums(is.na(test_bog))

##5.2. Guardar la base de Bogotá con todos los datos de fuentes externas ----

saveRDS(train_bog, "stores/20220724_train_bog.rds")
saveRDS(test_bog, "stores/20220724_test_bog.rds")

##5.3. Gráficas Info Bogotá D.c. ----


#Primera prueba gráfica
ggplot()+
  geom_sf(data=upz_bog
          %>% filter(grepl("RIO",UPlNombre)==FALSE),
          fill = NA) +
  geom_sf(data=loc_bog
          %>% filter(grepl("CHAPINERO",LocNombre)==TRUE),
          fill = "gray")+
  geom_sf(data=parques_bog, col="orange") +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))



##5.4. Bases Train para pruebas de modelos ----

train_bog <-readRDS("./stores/20220724_train_bog_p.rds") 
test_bog <-readRDS("./stores/20220724_test_bog_p.rds")

train_bog_parques <-readRDS("./stores/Bogota/train_bog_parques.rds") 
test_bog_parques <-readRDS("./stores/Bogota/test_bog_parques.rds")


train_bog$dist_park <- train_bog_parques$dist_park
test_bog$dist_park <- test_bog_parques$dist_park

saveRDS(train_bog, "stores/20220724_train_bog_p.rds")
saveRDS(test_bog, "stores/20220724_test_bog_p.rds")


###5.4.1. Base de chapinero ----

train_cha <-subset(train_bog,train_bog$LocNombre =="CHAPINERO")


#prueba de remover NAs  
#train_cha2 <- train_cha[(!is.na(train_cha$MANZANA_ID)), ]
train_bog_modelos <- na.omit(train_bog)# Base de correr modelos

#skim(train_cha2)


###5.4.2 Partición de la base chapinero en tres----

#La base de datos Train se divide en tres particiones:
# Tr_train: Entrenar el modelo
# Tr_eval: Evaluar, ajustar y refinar el modelo
# Tr_test: Probar el modelo


# Revisar: Generamos las particiones
set.seed(100)
split1_bog <- createDataPartition(train_bog_modelos$price, p = .7)[[1]]
length(split1) 

other <- train_bog_modelos[-split1,]
Tr_train_bog <- train_bog_modelos[split1,]

split2 <- createDataPartition(other$price, p = 1/3)[[1]]

Tr_eval_bog <- other[ split2,]
Tr_test_bog <- other[-split2,]

##5.5. Formas funcionales propuestas ----

modelo1 <- as.formula (price ~ AVALUO_COM+rooms+dist_tpubl+dist_hosp+dist_ccomerc)

modelo2 <- as.formula (price ~ p_upl+rooms+bathrooms+dist_tpubl+dist_hosp+dist_ccomerc)

modelo3 <- as.formula (price ~ AVALUO_COM+rooms+bathrooms+surface_covered+dist_tpubl+dist_hosp)


# Prueba 0, con OLS

reg1_lm<-lm(modelo1,data=Tr_train_bog)
reg2_lm<-lm(modelo2,data=Tr_train_bog)
reg3_lm<-lm(modelo3,data=Tr_train_bog)

stargazer(reg1_lm,reg2_lm,reg3_lm,type="text")

#Pruebas con lagsarlm

install.packages("lagsarlmtree")
library(lagsarlmtree)
install.packages("spdep")
library(spdep)

Tr_train_sp <- as(Tr_train_bog, "Spatial")

Tr_train_neib <- dnearneigh(coordinates(Tr_train_sp), 0, 0.1, longlat = TRUE)

listw <- nb2listw(Tr_train_neib, style="W", zero.policy = TRUE)

# Prueba 1, saca error. Empty neighbour sets found.

reg1<-lagsarlm(modelo1,data=Tr_train_bog, listw=listw)
reg2<-lagsarlm(modelo2,data=Tr_train_bog, listw=listw)
reg3<-lagsarlm(modelo3,data=Tr_train_bog, listw=listw)

stargazer(reg1,reg2,reg3,type="text")

# Prueba 2, Usando eigen valores. Funciona. --- probar después y revisar con manzanas porqu eno funciona

ev <- eigenw(listw)
W <- as(listw, "CsparseMatrix")
trMatc <- trW(W, type="mult")


reg1<-lagsarlm(modelo1,data=Tr_train_bog, listw=listw,
               method="eigen", quiet=FALSE, control=list(pre_eig=ev, OrdVsign=1))

reg2<-lagsarlm(modelo2,data=Tr_train_bog, listw=listw,
               method="eigen", quiet=FALSE, control=list(pre_eig=ev, OrdVsign=1))

reg3<-lagsarlm(modelo3,data=Tr_train_bog, listw=listw,
               method="eigen", quiet=FALSE, control=list(pre_eig=ev, OrdVsign=1))


stargazer(reg1,reg2,reg3,type="text")

##5.6. Modelos de predicción ----

###5.6.1. Entrenamiento de modelos CV K-Fold ----

modelo_estimado1 <- train(modelo1,
                          data = Tr_train_bog,
                          trControl=trainControl(method="cv",number=10),
                          method="lm")

modelo_estimado2 <- train(modelo2,
                          data = Tr_train_bog,
                          trControl=trainControl(method="cv",number=10),
                          method="lm")

modelo_estimado3 <- train(modelo3,
                          data = Tr_train_bog,
                          trControl=trainControl(method="cv",number=10),
                          method="lm")

modelo_predicho1 <- predict(modelo_estimado1,newdata = Tr_test_bog )
modelo_predicho2 <- predict(modelo_estimado2,newdata = Tr_test_bog )
modelo_predicho3 <- predict(modelo_estimado3,newdata = Tr_test_bog )

#Cálculo del MSE:
MSE_modelo1 <- with (Tr_test_bog,mean((price - modelo_predicho1)^2))
MSE_modelo2 <- with (Tr_test_bog,mean((price - modelo_predicho2)^2))
MSE_modelo3 <- with (Tr_test_bog,mean((price - modelo_predicho3)^2))

MSE_modelo1
MSE_modelo2
MSE_modelo3

#Guardar los resultados en la base de Test
Tr_test_bog$y1 <- modelo_predicho1
Tr_test_bog$y2 <- modelo_predicho2
Tr_test_bog$y3 <- modelo_predicho3

#Determinar si es compra o no el inmueble

Tr_test_bog$compra_clas_p1 <- factor(if_else( Tr_test_bog$y1 > Tr_test_bog$price | Tr_test_bog$price-Tr_test_bog$y1<40000000, "Compra", "No_compra"))
Tr_test_bog$compra_clas_p2 <- factor(if_else( Tr_test_bog$y2 > Tr_test_bog$price | Tr_test_bog$price-Tr_test_bog$y1<40000000, "Compra", "No_compra"))
Tr_test_bog$compra_clas_p3 <- factor(if_else( Tr_test_bog$y3 > Tr_test_bog$price | Tr_test_bog$price-Tr_test_bog$y1<40000000, "Compra", "No_compra"))

summary(Tr_test_bog$compra_clas_p1)
summary(Tr_test_bog$compra_clas_p2)
summary(Tr_test_bog$compra_clas_p3)

#Determinar si el inmueble está subvalorado

Tr_test_bog$subvalorado_clas_p1 <- factor(if_else( Tr_test_bog$price-Tr_test_bog$y1>40000000 , "sub", "No_sub"))
Tr_test_bog$subvalorado_clas_p2 <- factor(if_else( Tr_test_bog$price-Tr_test_bog$y2>40000000 , "sub", "No_sub"))
Tr_test_bog$subvalorado_clas_p3 <- factor(if_else( Tr_test_bog$price-Tr_test_bog$y3>40000000 , "sub", "No_sub"))

summary(Tr_test_bog$subvalorado_clas_p1)
summary(Tr_test_bog$subvalorado_clas_p2)
summary(Tr_test_bog$subvalorado_clas_p3)

#Calcular el total de dinero gastado con el modelo versus el dinero gastado si las compras se efectuan con precio de mercado 

#para el modelo 1
compra_y1_summary <- Tr_test_bog %>% 
  group_by(compra_clas_p1) %>% 
  summarise(dinerocompra_clas_p1 = sum(y1))

si_compra_y1_df <- compra_y1_summary %>%
  filter(compra_clas_p1=="Compra")

si_compra_y1 <- si_compra_y1_df$dinerocompra_clas_p1[[1]]

compra_price_summary_m1 <- Tr_test_bog %>% 
  group_by(compra_clas_p1) %>% 
  summarise(dinerocompra_clas_p1_0 = sum(price))

si_compra_price_df_m1 <- compra_price_summary_m1 %>%
                      filter(compra_clas_p1=="Compra")

si_compra_price_m1 <- si_compra_price_df_m1$dinerocompra_clas_p1_0[[1]]

#para el modelo 2
compra_y2_summary <- Tr_test_bog %>% 
  group_by(compra_clas_p2) %>% 
  summarise(dinerocompra_clas_p2 = sum(y2))

si_compra_y2_df <- compra_y2_summary %>%
  filter(compra_clas_p2=="Compra")

si_compra_y2 <- si_compra_y2_df$dinerocompra_clas_p2[[1]]

compra_price_summary_m2 <- Tr_test_bog %>% 
  group_by(compra_clas_p2) %>% 
  summarise(dinerocompra_clas_p2_0 = sum(price))

si_compra_price_df_m2 <- compra_price_summary_m2 %>%
  filter(compra_clas_p2=="Compra")

si_compra_price_m2 <- si_compra_price_df_m2$dinerocompra_clas_p2_0[[1]]

#para el modelo 3
compra_y3_summary <- Tr_test_bog %>% 
  group_by(compra_clas_p3) %>% 
  summarise(dinerocompra_clas_p3 = sum(y2))

si_compra_y3_df <- compra_y3_summary %>%
  filter(compra_clas_p3=="Compra")

si_compra_y3 <- si_compra_y3_df$dinerocompra_clas_p3[[1]]

compra_price_summary_m3 <- Tr_test_bog %>% 
  group_by(compra_clas_p3) %>% 
  summarise(dinerocompra_clas_p3_0 = sum(price))

si_compra_price_df_m3 <- compra_price_summary_m3 %>%
  filter(compra_clas_p3=="Compra")

si_compra_price_m3 <- si_compra_price_df_m3$dinerocompra_clas_p3_0[[1]]

#Resumen

si_compra_y1
si_compra_price_m1

si_compra_y2
si_compra_price_m2

si_compra_y3
si_compra_price_m3

###5.6.2. Definición del control (a usarse en los demás modelos) ----

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

control <- trainControl(method = "cv", number = 5,
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

#Función para calcular la decisión de compra --- organizar donde subirlo
#Entrada: decis_compra(x=valores_predichos,y=error)

decision_compra <- function(x,y) case_when(y > 0 ~ x,
                                           abs(y) < 40000000 ~ x,
                                           abs(y) > 40000000 ~ 0)

### Matriz de desempeño de los modelos:----

resumen_modelos <- data.frame(matrix(rep(0,75),nrow=15,ncol=5))
colnames(resumen_modelos) <- c("Modelo","Dinero_gastado","Prop_compradas","Precio_prom_compr","MSE_test")
sapply(resumen_modelos, typeof)

###5.6.3. XGBoost 1 ----

xgb_bog1 <- as.formula (price ~ 
                          AVALUO_COM+
                          rooms+
                          bathrooms+
                          surface_covered+
                          dist_tpubl+
                          dist_hosp+
                          dist_ccomerc+
                          p_upl)


form_xgboost <- xgb_bog1

grid_default <- expand.grid(nrounds = c(250,500),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

start_xg <- Sys.time()

xgboost <- train(
  form_xgboost,
  data = Tr_train_bog, #base bogota
  method = "xgbTree",
  trControl = control,
  na.action  = na.pass,
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)
xgboost

#Cálculo del índice desempeño del modelo:
pred_xgb <- predict(xgboost,Tr_test_bog)
pred_xgb_df <- data.frame(pred_xgb)

#Identifico la variable que tenía NAs para poder luego filtrar observaciones:
nrow(Tr_test_bog) - nrow(pred_xgb_df) #Dif. entre la base y el num de predicciones
colSums(is.na(Tr_test_bog)) #Reviso cuál variable tenía la cantidad de NAs de la resta anterior.

#Le pego al DF con la predicción el precio real y la variable que tenía NAs para filtrarla:
pred_xgb_df <- cbind (Tr_test_bog[,c("property_id","price")], pred_xgb_df)
                            #!(is.na(Tr_test_med$COD_CAT_US))), #Filtra obs de la var con NAs
                    
pred_xgb_df$geometry <- NULL #Elimino geometría
#pred_xgb_df$COD_CAT_US <- NULL #Elimino la variable que tenía NAs, aquí no la necesito

pred_xgb_df$error_xgb1 <- pred_xgb_df$pred_xgb -pred_xgb_df$price
pred_xgb_df$compra_xgb1 <- decision_compra(pred_xgb_df$pred_xgb,pred_xgb_df$error_xgb1)

resumen_modelos[1,1] <- "XGBoost_Bog 1"
resumen_modelos[1,2] <- sum(predicciones$compra_xgb1)
resumen_modelos[1,3] <- sum(predicciones$compra_xgb1>0)
resumen_modelos[1,4] <- resumen_modelos[1,2] / resumen_modelos[1,3]
resumen_modelos[1,5] <- sum(predicciones$error_xgb1^2)


#Determinar si es compra o no el inmueble

pred_xgb_df$compra_xgb1 <- factor(if_else(pred_xgb_df$pred_xgb > pred_xgb_df$price | pred_xgb_df$price-pred_xgb_df$pred_xgb<40000000, "Compra", "No_compra"))

summary(pred_xgb_df$compra_xgb1)

#Determinar si el inmueble está subvalorado

pred_xgb_df$subvalorado_xgb1 <- factor(if_else( pred_xgb_df$price-pred_xgb_df$pred_xgb>40000000 , "sub", "No_sub"))

summary(pred_xgb_df$subvalorado_xgb1)

#Calcular el total de dinero gastado con el modelo versus el dinero gastado si las compras se efectuan con precio de mercado 

#para el modelo 1
compra_pred_xgb <- pred_xgb_df %>% 
  group_by(compra_xgb1) %>% 
  summarise(dinerocompra_xgb = sum(pred_xgb))

si_compra_pred_xgb_df <- compra_pred_xgb %>%
  filter(compra_xgb1=="Compra")

si_compra_pred_xgb <- si_compra_pred_xgb_df$dinerocompra_xgb[[1]]


compra_price_xgb <- pred_xgb_df %>% 
  group_by(compra_xgb1) %>% 
  summarise(dinerocompra_price_xgb_0 = sum(price))

si_compra_pred_xgb_df <- compra_price_xgb %>%
  filter(compra_xgb1=="Compra")

si_compra_price_xgb <- si_compra_pred_xgb_df$dinerocompra_price_xgb_0[[1]]



#Resumen

si_compra_pred_xgb
si_compra_price_xgb


end_xg <- Sys.time()
start_xg-end_xg

###5.6.4. XGBoost 2 ----

xgb_bog2 <- as.formula (price ~ 
                          AVALUO_COM+
                          rooms+
                          bathrooms+
                          surface_covered+
                          dist_tpubl+
                          dist_hosp+
                          dist_ccomerc+
                          p_upl+
                          dist_park)


form_xgboost2 <- xgb_bog2

grid_default <- expand.grid(nrounds = c(250,500),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

start_xg <- Sys.time()

xgboost2 <- train(
  form_xgboost2,
  data = Tr_train_bog, #base bogota
  method = "xgbTree",
  trControl = control,
  na.action  = na.pass,
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)
xgboost2

#Cálculo del índice desempeño del modelo:
pred_xgb2 <- predict(xgboost2,Tr_test_bog)
pred_xgb_df2 <- data.frame(pred_xgb2)

#Identifico la variable que tenía NAs para poder luego filtrar observaciones:
nrow(Tr_test_bog) - nrow(pred_xgb_df2) #Dif. entre la base y el num de predicciones
colSums(is.na(Tr_test_bog)) #Reviso cuál variable tenía la cantidad de NAs de la resta anterior.

#Le pego al DF con la predicción el precio real y la variable que tenía NAs para filtrarla:
pred_xgb_df2 <- cbind (Tr_test_bog[,c("property_id","price")], pred_xgb_df2)
#!(is.na(Tr_test_med$COD_CAT_US))), #Filtra obs de la var con NAs

pred_xgb_df2$geometry <- NULL #Elimino geometría
#pred_xgb_df$COD_CAT_US <- NULL #Elimino la variable que tenía NAs, aquí no la necesito

#pred_xgb_df$error_xgb2 <- pred_xgb_df2$pred_xgb -pred_xgb_df2$price
#pred_xgb_df$compra_xgb2 <- decision_compra(pred_xgb_df2$pred_xgb2,pred_xgb_df2$error_xgb2)

#resumen_modelos[1,1] <- "XGBoost_Bog 1"
#resumen_modelos[1,2] <- sum(predicciones$compra_xgb2)
#resumen_modelos[1,3] <- sum(predicciones$compra_xgb2>0)
#resumen_modelos[1,4] <- resumen_modelos[1,2] / resumen_modelos[1,3]
#resumen_modelos[1,5] <- sum(predicciones$error_xgb2^2)


#Determinar si es compra o no el inmueble

pred_xgb_df2$compra_xgb2 <- factor(if_else(pred_xgb_df2$pred_xgb2 > pred_xgb_df2$price | pred_xgb_df2$price-pred_xgb_df2$pred_xgb2<40000000, "Compra", "No_compra"))

summary(pred_xgb_df2$compra_xgb2)

#Determinar si el inmueble está subvalorado

pred_xgb_df2$subvalorado_xgb2 <- factor(if_else( pred_xgb_df2$price-pred_xgb_df2$pred_xgb2>40000000 , "sub", "No_sub"))

summary(pred_xgb_df2$subvalorado_xgb2)

#Calcular el total de dinero gastado con el modelo versus el dinero gastado si las compras se efectuan con precio de mercado 

#para el modelo 1
compra_pred_xgb2 <- pred_xgb_df2 %>% 
  group_by(compra_xgb2) %>% 
  summarise(dinerocompra_xgb2 = sum(pred_xgb2))

si_compra_pred_xgb_df2 <- compra_pred_xgb2 %>%
  filter(compra_xgb2=="Compra")

si_compra_pred_xgb2 <- si_compra_pred_xgb_df2$dinerocompra_xgb2[[1]]


compra_price_xgb2 <- pred_xgb_df2 %>% 
  group_by(compra_xgb2) %>% 
  summarise(dinerocompra_price_xgb_02 = sum(price))

si_compra_pred_xgb_df2 <- compra_price_xgb2 %>%
  filter(compra_xgb2=="Compra")

si_compra_price_xgb2 <- si_compra_pred_xgb_df$dinerocompra_price_xgb_02[[1]]



#Resumen

si_compra_pred_xgb
si_compra_price_xgb


end_xg <- Sys.time()
start_xg-end_xg



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. MODELO MEDELLÍN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++