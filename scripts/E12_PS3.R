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
       #gtsummary,
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
       xgboost,
       sfheaders,
       nngeo)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. CARGUE DE LAS BASES DE DATOS Y EXPLORACIÓN INICIAL----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


setwd("~/GitHub/MECA_BD_PS3")

train_prop <-readRDS("./stores/train.rds") #107.567 Obs
test_prop <-readRDS("./stores/test.rds") #11.150 Obs



##1.1. Exploración incial de los datos ----


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

##NAs Base Train 
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

##NAs Base Test
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



##1.3. Convertir las bases de datos en objetos geográficos ----

#Se transforma toda la base de datos de Train y test:

train_prop <- train_prop %>% mutate(latp=lat,longp=lon)
train_prop <- st_as_sf(train_prop ,coords=c('longp','latp'),crs=4326)

test_prop <- test_prop %>% mutate(latp=lat,longp=lon)
test_prop <- st_as_sf(test_prop ,coords=c('longp','latp'),crs=4326)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. SEPARAR LAS BASES/ BOGOTÁ D.C. Y MEDELLÍN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Base de Bogotá D.C.
train_bog <-subset(train_prop,train_prop$l3 =="Bogotá D.C")
test_bog <-subset(test_prop,test_prop$l3 =="Bogotá D.C")                   


#Base de Medellín
train_med <-subset(train_prop,train_prop$l3 =="Medellín")
test_med <-subset(test_prop,test_prop$l3 =="Medellín")   


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. VARIABLES DE TEXTO E IMPUTACIÓN DE DATOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##3.1. Variables de texto----

##3.2. Imputación de datos----

##3.3. Definición bases datos definitivas ----






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. MODELO MEDELLÍN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##6.1. PRELIMINARES Y GRÁFICOS ----

#Graficar las propiedades de Medellín:

#Todas juntas
leaflet() %>% addTiles() %>% 
  addCircleMarkers(data=train_med,color="green") %>% 
  addCircleMarkers(data=test_med,color="blue")

#Solo test
leaflet() %>% addTiles() %>% addCircleMarkers(data=test_med,color="blue")

#Solo train
leaflet() %>% addTiles() %>% addCircleMarkers(data=train_med,color="green")


#Comunas de Medellín:

El_Poblado <- getbb(place_name = "Comuna 14 - El Poblado Medellín", 
                   featuretype = "place:suburb", 
                   format_out = "sf_polygon")

class(El_Poblado)
st_crs(El_Poblado)

leaflet() %>% addTiles() %>% addPolygons(data=El_Poblado)



#Información estratos, lotes, predios Medellín

predios_med <- import("./stores/Medellín/informacion_predios.csv")
lotes_med <- import("./stores/Medellín/informacion_lotes.csv")
usos_med_geojson <- read_sf("./stores/Medellín/UsosGnalesSueloUrbano.geojson")





#Todos los barrios de Medellín
leaflet() %>% addTiles() %>% addPolygons(data=barrios_med,label=barrios_med$NOMBRE)

#Solo los barrios de El Poblado
leaflet() %>% addTiles() %>% 
  addPolygons(data=barrios_med %>% filter(grepl(14,LIMITECOMU)==TRUE),
              label=barrios_med$NOMBRE)


##6.2 INFORMACIÓN GEOGRÁFICA----

#A cada observación le quiero agregar:

#Comuna / Barrio / Estrato / Manzana / ...


#####Comuna: ----

catastro_med <- read_sf("./stores/Medellín/shp_SECTOR_CATASTRAL/SECTOR_CATASTRAL.shp")
st_crs(catastro_med)
catastro_med <- st_transform(catastro_med,4326)
colnames(catastro_med)

train_med <- st_join(train_med,catastro_med[,c('COMUNA','NOMBRE')])
test_med <- st_join(test_med,catastro_med[,c('COMUNA','NOMBRE')])


#####Barrio: ----

barrios_med <- read_sf("./stores/Medellín/shp_BarrioVereda/BarrioVereda_2014.shp")
st_crs(barrios_med)
barrios_med <- st_transform(barrios_med,4326)
colnames(barrios_med)

train_med <- st_join(train_med,barrios_med[,c('CODIGO','NOMBRE')])
test_med <- st_join(test_med,barrios_med[,c('CODIGO','NOMBRE')])


colSums(is.na(train_med))
colSums(is.na(test_med))



#####Estrato: ----

estratos_med <- read_sf("./stores/Medellín/shp_ESTRATIFICACION/ESTRATIFICACION.shp")
st_crs(estratos_med)
estratos_med <- st_transform(estratos_med,4326)
colnames(estratos_med)

estratos_med$geom_err <- st_is_valid(estratos_med, reason = T)
nrow(estratos_med)
table(estratos_med$geom_err)

31972-31671
#301 errores en las geometrías

estratos_med <- st_make_valid(estratos_med)

estratos_med$geom_err <- st_is_valid(estratos_med, reason = T)
nrow(estratos_med)
table(estratos_med$geom_err)
#Quedan solo 4 errores en las geometrías

estratos_med <- filter(estratos_med,estratos_med$geom_err == "Valid Geometry")
nrow(estratos_med)
#Se eliminaron las 4 geometrías malas


train_med <- st_join(train_med,estratos_med[,c('MANZANA','ESTRATO')])
test_med <- st_join(test_med,estratos_med[,c('MANZANA','ESTRATO')])

colSums(is.na(train_med))
colSums(is.na(test_med))

#Corrección por vecinos por los NA

#Crear la base de estratos para la Comuna de El Poblado:
estr_med_pobl <- filter(estratos_med,COMUNA==14)

#Dividir Train entre las que sí encontró estrato/manzana y los NA (al final se unen)
train_med_estr_ok <- filter(train_med,!(is.na(train_med$MANZANA)))
train_med_estr_na <- filter(train_med,is.na(train_med$MANZANA))

nrow(train_med_estr_ok)+nrow(train_med_estr_na)
nrow(train_med)

colnames(train_med_estr_na)
train_med_estr_na <- train_med_estr_na[,-(27:28)] #OJO, ELIMINAR COLUMNAS CORRECTAS



#Dividir Test entre las que sí encontró estrato/manzana y los NA (al final se unen)
test_med_estr_ok <- filter(test_med,!(is.na(test_med$MANZANA)))
test_med_estr_na <- filter(test_med,is.na(test_med$MANZANA))

nrow(test_med_estr_ok)+nrow(test_med_estr_na)
nrow(test_med)

colnames(test_med_estr_na)
test_med_estr_na <- test_med_estr_na[,-(26:27)] #OJO, ELIMINAR COLUMNAS CORRECTAS

#Ejecución del Join con Max Dist = 50m

#Para test:

#Remover geometrías vacías:
estr_med_pobl <- estr_med_pobl %>% filter(!st_is_empty(.))

start_test = Sys.time()
test_med_estr_na <- st_join(test_med_estr_na,estr_med_pobl[,c('MANZANA','ESTRATO')],
                           join = st_nn, k = 1, maxdist = 50, parallel=8)
end_test = Sys.time()
end_test - start_test

colSums(is.na(test_med_estr_na))

test_med_estr_na_df <- sf_to_df(test_med_estr_na, fill = TRUE, unlist = NULL)
saveRDS(test_med_estr_na_df,"./stores/Medellín/rds_calculados/ESTRATO_test_NA.rds")

colSums(is.na(test_med))
test_med <- rbind(test_med_estr_ok,test_med_estr_na)
colSums(is.na(test_med))


#Para train:

estratos_med <- estratos_med %>% filter(!st_is_empty(.))

start_train = Sys.time()
train_med_estr_na <- st_join(train_med_estr_na,estratos_med[,c('MANZANA','ESTRATO')],
                            join = st_nn, k = 1, maxdist = 50, parallel=8)
end_train = Sys.time()
end_train - start_train

colSums(is.na(train_med_estr_na))

train_med_estr_na_df <- sf_to_df(train_med_estr_na, fill = TRUE, unlist = NULL)
saveRDS(train_med_estr_na_df,"./stores/Medellín/rds_calculados/ESTRATO_train_NA.rds")

colSums(is.na(train_med))
train_med <- rbind(train_med_estr_ok,train_med_estr_na)
colSums(is.na(test_med))

nrow(test_med)

#Revisión luego de imputar el Uso:
colSums(is.na(train_med))
colSums(is.na(test_med))




#### Uso del suelo: ----

usos_med <- read_sf("./stores/Medellín/shp_UsosSueloUrbano/UsosGnalesSueloUrbano.shp")
st_crs(usos_med)
usos_med <- st_transform(usos_med,4326)

usos_med$geom_err <- st_is_valid(usos_med, reason = T)
nrow(usos_med)
table(usos_med$geom_err)

25448-25343
#105 errores en las geometrías

usos_med <- st_make_valid(usos_med)

usos_med$geom_err <- st_is_valid(usos_med, reason = T)
nrow(usos_med)
table(usos_med$geom_err)

usos_med <- filter(usos_med,usos_med$geom_err == "Valid Geometry")
nrow(usos_med)
#Se elimina la geometría mala

colnames(usos_med)
train_med <- st_join(train_med,usos_med[,c("COD_CAT_US","COD_SUBCAT","AREAGRALUS","SUBCATEGOR")])
test_med <- st_join(test_med,usos_med[,c("COD_CAT_US","COD_SUBCAT","AREAGRALUS","SUBCATEGOR")])

colSums(is.na(train_med))
colSums(is.na(test_med))


#Corrección por vecinos por los NA

#Crear la base de usos para la Comuna de El Poblado:
usos_med_pobl <- st_join(usos_med,catastro_med[,c('COMUNA','NOMBRE')])
usos_med_pobl <- filter(usos_med_pobl,usos_med_pobl$COMUNA==14)

#Dividir Train entre las que sí encontró Uso y los NA (al final se unen)
train_med_uso_ok <- filter(train_med,!(is.na(train_med$AREAGRALUS)))
train_med_uso_na <- filter(train_med,is.na(train_med$AREAGRALUS))

nrow(train_med_uso_ok)+nrow(train_med_uso_na)
nrow(train_med)

colnames(train_med_uso_na)
train_med_uso_na <- train_med_uso_na[,-(29:32)]  #OJO, ELIMINAR COLUMNAS CORRECTAS


#Dividir Test entre las que sí encontró Uso y los NA (al final se unen)
test_med_uso_ok <- filter(test_med,!(is.na(test_med$AREAGRALUS)))
test_med_uso_na <- filter(test_med,is.na(test_med$AREAGRALUS))

nrow(test_med_uso_ok)+nrow(test_med_uso_na)
nrow(test_med)

colnames(test_med_uso_na)
test_med_uso_na <- test_med_uso_na[,-(28:31)]  #OJO, ELIMINAR COLUMNAS CORRECTAS


#Ejecución del Join con Max Dist = 50m
#Para test:

start_test = Sys.time()
test_med_uso_na <- st_join(test_med_uso_na,usos_med_pobl[,c("COD_CAT_US","COD_SUBCAT","AREAGRALUS","SUBCATEGOR")],
                           join = st_nn, k = 1, maxdist = 50, parallel=3)
end_test = Sys.time()
end_test - start_test

#Reviso si quedaron NA
colSums(is.na(test_med_uso_na))

#Guardo en un DataFrame el resultado de la búsqueda de USOS
#(para evitar correr de nuevo el código, que se demora)

test_med_uso_na_df <- sf_to_df(test_med_uso_na, fill = TRUE, unlist = NULL)
saveRDS(test_med_uso_na_df,"./stores/Medellín/rds_calculados/USOS_NA_test.rds")

colSums(is.na(test_med))
test_med <- rbind(test_med_uso_ok,test_med_uso_na)
colSums(is.na(test_med))


#Ejecución del Join con Max Dist = 50m
#Para Train:

start_train = Sys.time()
train_med_uso_na <- st_join(train_med_uso_na,usos_med[,c("COD_CAT_US","COD_SUBCAT","AREAGRALUS","SUBCATEGOR")],
                           join = st_nn, k = 1, maxdist = 50, parallel=3)
end_train = Sys.time()
end_train - start_train

#Reviso si quedaron NA
colSums(is.na(train_med_uso_na))

#Guardo en un DataFrame el resultado de la búsqueda de USOS
#(para evitar correr de nuevo el código, que se demora)

train_med_uso_na_df <- sf_to_df(train_med_uso_na, fill = TRUE, unlist = NULL)
saveRDS(train_med_uso_na_df,"./stores/Medellín/rds_calculados/USOS_NA_train.rds")

colSums(is.na(train_med))
train_med <- rbind(train_med_uso_ok,train_med_uso_na)
colSums(is.na(train_med))


#Revisión luego de imputar el Uso:
colSums(is.na(train_med))
colSums(is.na(test_med))


ggplot()+
  geom_sf(data=manz_med_pobl,fill = NA) +
  theme_bw() +
  theme(axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))



#### Manzanas del DANE: ----

manz_med <- read_sf("./stores/Medellín/shp_MGNDANE/MGN_URB_MANZANA.shp")
st_crs(manz_med)
colnames(manz_med)
manz_med <- st_transform(manz_med,4326)

manz_med$geom_err <- st_is_valid(manz_med, reason = T)
nrow(manz_med)
table(manz_med$geom_err)

25448-25343
#1 error en la geometría

manz_med <- st_make_valid(manz_med)

manz_med$geom_err <- st_is_valid(manz_med, reason = T)
nrow(manz_med)
table(manz_med$geom_err)

manz_med <- filter(manz_med,manz_med$geom_err == "Valid Geometry")
nrow(manz_med)
#Se elimina la geometría mala

colnames(manz_med)
train_med <- st_join(train_med,manz_med[,c("CPOB_CCDGO","MANZ_CCDGO","MANZ_CCNCT","MANZ_CAG")])
test_med <- st_join(test_med,manz_med[,c("CPOB_CCDGO","MANZ_CCDGO","MANZ_CCNCT","MANZ_CAG")])

colSums(is.na(train_med)) #7440 en TRAIN sin MZN
colSums(is.na(test_med))  #3114 en TEST sin MZN


#Corrección por vecinos por los NA

#Crear la base de usos para la Comuna de El Poblado:
manz_med_pobl <- st_join(manz_med,catastro_med[,c('COMUNA','NOMBRE')])
manz_med_pobl <- filter(manz_med_pobl,manz_med_pobl$COMUNA==14)

leaflet() %>% addTiles() %>% addPolygons(data=manz_med_pobl)  

#Dividir Train entre las que sí encontró Uso y los NA (al final se unen)
train_med_mzn_ok <- filter(train_med,!(is.na(train_med$MANZ_CCDGO)))
train_med_mzn_na <- filter(train_med,is.na(train_med$MANZ_CCDGO))

nrow(train_med_mzn_ok)+nrow(train_med_mzn_na)
nrow(train_med)

colnames(train_med_mzn_na)
train_med_mzn_na <- train_med_mzn_na[,-(33:36)]  #OJO, ELIMINAR COLUMNAS CORRECTAS


#Dividir Test entre las que sí encontró Uso y los NA (al final se unen)
test_med_mzn_ok <- filter(test_med,!(is.na(test_med$MANZ_CCDGO)))
test_med_mzn_na <- filter(test_med,is.na(test_med$MANZ_CCDGO))

nrow(test_med_mzn_ok)+nrow(test_med_mzn_na)
nrow(test_med)

colnames(test_med_mzn_na)
test_med_mzn_na <- test_med_mzn_na[,-(32:35)]  #OJO, ELIMINAR COLUMNAS CORRECTAS


#Ejecución del Join con Max Dist = 50m

#Para test:

start_test = Sys.time()
test_med_mzn_na <- st_join(test_med_mzn_na,manz_med_pobl[,c("CPOB_CCDGO","MANZ_CCDGO","MANZ_CCNCT","MANZ_CAG")],
                           join = st_nn, k = 1, maxdist = 50, parallel=3)
end_test = Sys.time()
end_test - start_test

colSums(is.na(test_med_mzn_na))

test_med_mzn_na_df <- sf_to_df(test_med_mzn_na, fill = TRUE, unlist = NULL)
saveRDS(test_med_mzn_na_df,"./stores/Medellín/rds_calculados/MZN_test_NA.rds")

colSums(is.na(test_med))
test_med <- rbind(test_med_mzn_ok,test_med_mzn_na)
colSums(is.na(test_med))


#Para train:

start_train = Sys.time()
train_med_mzn_na <- st_join(train_med_mzn_na,manz_med[,c("CPOB_CCDGO","MANZ_CCDGO","MANZ_CCNCT","MANZ_CAG")],
                           join = st_nn, k = 1, maxdist = 50, parallel=3)
end_train = Sys.time()
end_train - start_train

colSums(is.na(train_med_mzn_na))

train_med_mzn_na_df <- sf_to_df(train_med_mzn_na, fill = TRUE, unlist = NULL)
saveRDS(train_med_mzn_na_df,"./stores/Medellín/rds_calculados/MZN_train_NA.rds")

colSums(is.na(train_med))
train_med <- rbind(train_med_mzn_ok,train_med_mzn_na)
colSums(is.na(test_med))

nrow(test_med)

#Revisión luego de imputar el Uso:
colSums(is.na(train_med))
colSums(is.na(test_med))






## 6.3. REGRESIONES ----


### Modelos básicos ----

reg1 <- lm(price ~ factor(ESTRATO),
           data=train_med)

reg2 <- lm(price ~ bedrooms + factor(ESTRATO), 
           data=train_med)

reg3 <- lm(price ~ bathrooms + bedrooms + factor(ESTRATO), 
           data=train_med)

reg4 <- lm(price ~ bathrooms + bedrooms + factor(ESTRATO) + surface_total, 
           data=train_med)

reg5 <- lm(price ~ bathrooms + bedrooms + factor(ESTRATO) + factor(NOMBRE.x) + surface_total, 
           data=train_med)


reg7 <- lm(price ~ bedrooms + ESTRATO + COD_CAT_US + COD_SUBCAT, 
           data=train_med)
 
reg7 <- lm(price ~ bathrooms + bedrooms + factor(ESTRATO) + factor(NOMBRE.x) + surface_total + factor(COD_SUBCAT), 
           data=train_med)

stargazer(reg6,type="text")

stargazer(reg1,reg2,reg3,type="text")
stargazer(reg4,reg5,type="text")
stargazer(reg4,reg6,type="text")
stargazer(reg6,reg7,type="text")



#Observatorio Inmobiliario de Medellín


#Explorar los archivos KML
st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2022.kml")
st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2021.kml")
st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2020.kml")
st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2019.kml")
st_layers("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2018.kml")


OIME_2022_apts <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2022.kml",layer="Apartamentos")
OIME_2021 <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2021.kml",layer="APARTAMENTOS USADOS.xlsx")
OIME_2020 <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2020.kml")
OIME_2019 <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2019.kml",layer="APARTAMENTOS.xlsx")
OIME_2018 <- read_sf("./stores/Medellín/OIME_ofertas/Ofertas de Ventas 2018.kml")


leaflet() %>% addTiles() %>% 
  addCircleMarkers(data=OIME_2022,popup = OIME_2022$Description, color ="green") %>% 
  addCircleMarkers(data=OIME_2021,popup = OIME_2021$Description, color ="red") %>% 
  addCircleMarkers(data=OIME_2020,popup = OIME_2020$Description, color ="blue") %>% 
  addCircleMarkers(data=OIME_2019,popup = OIME_2019$Description, color ="orange")%>%
  addCircleMarkers(data=OIME_2018,popup = OIME_2018$Description, color ="black")



#Estaciones de metro en Medellín:

## objeto osm
metromed  <-  opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="public_transport" , value="station") 

metromed_sf <- metromed %>% osmdata_sf()
metromed_sf
metromed_station  <-  metromed_sf$osm_points

leaflet() %>% addTiles() %>% addCircleMarkers(data=metromed_station)


#PARA QUÉ QUIEREN ENCONTRAR LAS MANZANAS?
#QUÉ INFO TIENEN MANZANAS DEL DANE?
#MED: USO DEL SUELO, ...
#MED: ÍNDICE DE INSEGURIDAD
#DISTANCIA A FEATURES: PARQUES, CENTRO COMERCIALES, HOSPITALES, ESTACIONES METRO




#Árbol MED:

###Modelo árbol básico (CART) ----

train_med_fact <- train_med 
train_med_fact$ESTRATO <- factor(train_med_fact$ESTRATO)
train_med_fact$COD_CAT_US <- factor(train_med_fact$COD_CAT_US)
train_med_fact$COD_SUBCAT <- factor(train_med_fact$COD_SUBCAT)


form_tree <- as.formula("price ~ bedrooms + ESTRATO + COD_CAT_US + COD_SUBCAT")


#cp_alpha<-seq(from = 0, to = 0.1, length = 10)

#Ensayo de tree con un control de internet:

cvCtrl <- trainControl(method="repeatedcv", repeats = 2, classProbs = TRUE)

tree <- train(
  form_tree,
  data = train_med_fact,
  method = "rpart",
  trControl = cvCtrl,
  na.action  = na.pass,
  parms=list(split='Gini'),
  controls=ctree_control(maxsurrogate=2),
  #tuneGrid = expand.grid(cp = cp alpha)#,
  tuneLength=200
  #preProcess = c("center", "scale")
)

tree
rpart.plot::prp(tree$finalModel)
pred_tree <- predict(tree,Tr_test)
c_matr_tree <- confusionMatrix(Tr_test$Pobre,pred_tree,positive="Pobre")
c_matr_tree









skim(test_med)

#Intersecciones, cruces, identificar manzana que se cruza con un punto



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


