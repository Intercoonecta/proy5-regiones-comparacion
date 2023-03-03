library(tidyverse) #Incluye a ggplot2
library(rerddap)
library(raster)
servidores <- servers()
View(servidores)
#Revisemos las primeras opciones disponibles
head(servidores)

servidores %>%
  filter(str_detect(str_to_lower(name), "noaa"))#busca en la columna de nombres lo que contenga la palabra coastwatch también aclarando que me tome independiente de si es minúscula o mayúscula (obligo a que lea en minúscula)
#dice si son datos públicos o no

noaa_url <- servidores %>% 
  filter(short_name == "UAF") %>%
  #Ahora que sabemos el nombre corto, podemos utilizar esto para filtrar los servidores
    pull(url)#extrae la información que está en una celda

#Veamos el resultado
noaa_url #ya tengo el servidor

?ed_search
datos=ed_datasets('grid')

#Podemos hacer una busqueda sencilla de datos de temperatura de la superficie del mar (SST por sus siglas en ingles) usando la funcion `ed_search`.

temp_sup_res_ <- ed_search(query = "sst", 
                          which = "griddap",
                          url = noaa_url)
head(temp_sup_res_$info)

temp_sup_res_2 <- ed_search_adv(query = "sst",
                                #Datos en grilla
                                protocol = "griddap",
                                #Servidor erddap
                                url = noaa_url,
                                #Datos mensuales
                                keywords = "monthly",
                                #Limites espaciales
                                #Limites temporales
                                minTime = "2004") #le tuve que sacar el max porque sino no funciona
View(temp_sup_res_2$info)

temp_sup_res_$info %>% filter(dataset_id == "erdMH1sstdmdayR20190SQ") #tengo que meterme en info para poder hacer el filter
temp_sup_res_2$info %>% filter(dataset_id == "erdMH1sstdmdayR20190SQ")
temp_sup_res_2$info$dataset_id== "erdMH1sstdmdayR20190SQ"#10

temp_sup_MODIS4KM <- griddap(temp_sup_res_2$info$dataset_id[10],
                        #Limites temporales
                        time = c("2004-01-01", "2008-12-31"),
                        #Limites espaciales
                        latitude = c(-55, -30),
                        longitude = c(-70, -50),
                        #Acceder a la informacion en formato netcdf
                        fmt = "nc",
                        #Guardar informacion en disco
                        store = disk(path = "Data/"))
getwd()

archivoSSTmodis <- list.files("Data/", pattern = "1643af705e79862ee0ebc0c5b2ba2fc4.nc", full.names = T)
archivoSSTmodis_r <- raster(archivoSSTmodis)
archivoSSTmodis_r

raster::plot(archivoSSTmodis_r[[1]])
raster::plot(archivoSSTmodis_r[[1]], col = RColorBrewer::brewer.pal(9, "YlOrRd"))

temp_prom_anual <- raster::mean(archivoSSTmodis_r, 12)

archivoSSTmodis_r@file@nbands


library(sf)
ecoreg=st_read("./Data/ecoregiones_jes.shp")
ecoreg$ECOREGION
ecoreg$ECOREGION[1]
ecoreg$geometry[1]##MALVINAS xmin: -64.1607 ymin: -55.79758 xmax: -54.4003 ymax: -47.80757
ecoreg$geometry[2]##North Patagonian Gulfs xmin: -71.07653 ymin: -47.55971 xmax: -57.29605 ymax: -37.45682
ecoreg$geometry[3]##Patagonian Shelf"xmin: -71.07653 ymin: -54.76612 xmax: -56.23102 ymax: -41.20498
ecoreg$geometry[4]##"Rio de la Plata" xmin:-61.89776 ymin: -36.55243 xmax: -54.93611 ymax: -29.15206
ecoreg$geometry[5]##"Rio Grande"  xmin: -55.39115 ymin: -35.64241 xmax: -45.83059 ymax: -26.84257
ecoreg$geometry[6]##Uruguay-Buenos Aires Shelf" xmin: -65.4206 ymin: -41.25759 xmax: -50.38224 ymax: -30.7737

?st_as_text


#aca para probar descargar los datos ORBIS

library(robis)
library(rgdal) # for `ogrInfo()` and `readOGR()`
library(tools) # for `file_path_sans_ext()`
library(dplyr) # for `inner_join()`, `filter()`, `summarise()`, and the pipe operator (%>%)
library(ggplot2) # for `fortify()` and for plotting
library(sp) # for `point.in.polygon()` and `spDists()`
library(tidyr) # for `gather()`
library(readr) # for `write_tsv()`
library(leaflet)
library(lubridate)
library(sf)
library(dplyr)

ACN_orbis=occurrence(scientificname = "Thalassarche melanophris", startdate = as.Date("2000-01-01"), enddate = as.Date("2022-12-31"))
str(ACN_orbis)
summary(ACN_orbis$decimalLongitude)
write.csv(ACN_orbis,"./Data/ACN_orbis.csv")
getwd()
ACN_orbis=read.csv("./proy5-regiones-comparacion/Parte_Jesi/Data/ACN_orbis.csv", header=T)
crs=CRS("+init=epsg:4326")
ACN_orbis_sp=SpatialPointsDataFrame(ACN_orbis[,c("decimalLongitude","decimalLatitude")],data=ACN_orbis, proj4string= crs)#identifica la espacialidad
plot(ACN_orbis_sp, axes=T)


ACN_orbi2 <- ACN_orbis %>% select(date_year,scientificName,decimalLatitude,decimalLongitude,month) %>% 
  mutate(YEAR=date_year,LAT=decimalLatitude,LON=decimalLongitude) %>% filter(LAT> -60 & LAT< -25 & LON> -70 & LON< -40)
#ACN_orbi3 <- ACN_orbi2%>% filter(LAT> -55) 
#ACN_orbi4 <- ACN_orbi3%>% filter(LAT< -30)
ACN_orbi2_sp=SpatialPointsDataFrame(ACN_orbi2[,c("LON","LAT")],data=ACN_orbi2, proj4string= crs)#identifica la espacialidad
plot(ACN_orbi2_sp, axes=T)
plot(st_geometry(st_read("./proy5-regiones-comparacion/Parte_Jesi/Data/America_del_sur.shp")), add=T) #abro los datos, los tengo guardatos en mi carpeta data
levels(as.factor(ACN_orbi2_sp$YEAR)) # "2000" "2001" "2002" "2006" "2007" "2008" "2019"



tierra <- rnaturalearth::ne_countries(returnclass = "sf")
ACN_orbi2_sp
ACN_orbi2_sf<-st_as_sf(ACN_orbi2_sp)
ecoreg=st_read("./proy5-regiones-comparacion/Parte_Jesi/Data/ecoregiones_jes.shp")
ecoreg$ECOREGION
ramp = rainbow(length(unique(ecoreg$ECOREGION)))
colors = ramp[as.factor(ecoreg$ECOREGION)]

plot(st_geometry(ecoreg), axis=T,col=colors, border="black") #xlim=c(-90,-75), ylim=c(-5, 2), col=colors, border="black")
plot(st_geometry(tierra), col="grey", add=T, border="black")
plot(st_geometry(ACN_orbi2_sf), add=T, pch=21, col="black", bg="330066", cex= 0.8)


levels(as.factor(ACN_orbi2_sf$YEAR))
  windows()
  
Mapagen2 <- ggplot()+
  geom_sf(data = ecoreg, aes(fill= ECOREGION),alpha = 0.3)+
  #geom_sf_text(data = ecoreg, aes(label= ECOREGION), colour = "black",size=2)+#https://yutani.rbind.io/post/geom-sf-text-and-geom-sf-label-are-coming/
  geom_sf(data = tierra,color = "black", fill="grey")+
  geom_sf_text(data = tierra, aes(label= name), colour = "black",size=3)+#https://yutani.rbind.io/post/geom-sf-text-and-geom-sf-label-are-coming/
  geom_sf(data= ACN_orbi2_sf, cex=1, mapping = aes(color=as.factor(YEAR)))+
  scale_colour_manual("AÑOS",values = c("red", "blue", "green","yellow","pink","33CCFF", "brown","3366FF"))+
 ggtitle('Albatros Ceja Negra (Thalassarche melanophris)') +
  scale_x_continuous(name ="LONGITUD")+
  scale_y_continuous(name ="LATITUD")+
   coord_sf(xlim = c(-70, -40), ylim = c(-60, -25), expand = FALSE)+
     theme(legend.position = "right")

#LAT> -60 & LAT< -25 & LON> -70 & LON< -40

#cc
