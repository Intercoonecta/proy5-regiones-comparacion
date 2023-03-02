#Step 1 cargar paquetes
install.packages(c("rgdal", "dplyr", "ggplot2", "sp", "tidyr", "readr", "leaflet", "lubridate"))
install.packages(classInt)
install.packages(robis)
devtools::install_github("iobis/robis")
devtools::install_github("classInt")
install.packages(sf)
install.packages(terra)
library(robis)
library(classInt)
library(rgdal) # for `ogrInfo()` and `readOGR()`
library(tools) # for `file_path_sans_ext()`
library(dplyr)
library(ggplot2) # for `fortify()` and for plotting
library(sp) # for `point.in.polygon()` and `spDists()`
library(tidyr) # for `gather()`
library(readr) # for `write_tsv()`
library(leaflet)
library(lubridate)
library(geos)
library(libgeos)

devtools::install_github("rsbivand/sp@evolution")

Sys.setenv("_SP_EVOLUTION_STATUS_"=2)
CRS("+proj=longlat")

##Paso 2 para manejar los datos de la geometría para graficas en un mapa

fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f <- fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

subset.shape <- function(x, domain){
  x.subset <- filter(x, long > domain[1] & 
                       long < domain[2] & 
                       lat > domain[3] & 
                       lat < domain[4])
  x.subset
}

#Paso 3 leer el archivo shapefile donde estan todos los poligonos, con datos globales. 
path.lme.coast <- ("~/Desktop/JUDITH/++ACADEMIC/Hackaton_AECID/Project_Hackaton/ohw-tutorials-OHW22/optional/espanol/lme-obis-extracciones/data")
fnam.lme.coast <- "LMEs66.shp"
dat.coast <- st_read(dsn = path.lme.coast, 
                     layer = file_path_sans_ext(fnam.lme.coast))
dat.coast <- fortify.shape(dat.coast) # a 410951x8 dataframe
dat.sel_5 <- subset(dat.coast, LME_NUMBER == 13) # Humboldt C.
dat.sel_4 <- subset(dat.coast, LME_NUMBER == 14) # Patagonia
dat.sel_9 <- subset(dat.coast, LME_NUMBER == 4) # Gulf of California
dat.sel_6 <- subset(dat.coast, LME_NUMBER == 12) # Caribbean
dat.sel_7 <- subset(dat.coast, LME_NUMBER == 11) # P. Ctral A.


#Paso 4 representando la costa y los límites de las áreas marinas seleccionadas
xlims <- c(-150, -25)
ylims <- c(-60, 60)
mapWorld <- borders(database = "world", colour="gray50", fill="gray50")


#Generamos un mapa con las línea de costa
p0 <- ggplot() + theme(text = element_text(size=18)) + 
  geom_path(data = dat.coast, aes(x = long, y = lat, group = group), 
            color = "black", linewidth = 0.25) + 
  coord_map(projection = "mercator") + 
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "", x = "Longitude", y = "Latitude"))
p0

p.sel <- p0 +
  geom_path(data = dat.sel_5,
            aes(x = long, y = lat, group = group),
            colour = "chartreuse4", linewidth = 1)
p.sel

#https://obis.org/maptool/# web para hacer poligonos WKT
#Intento 2 para hacer mapa
CentralPeru <-occurrence(geometry = "POLYGON((-84.28711 -3.42569, -80.77148 -3.33795, -78.04688 -10.91962, -81.38672 -10.91962, -85.16602 -6.05316, -84.72656 -3.77656, -84.28711 -3.42569))")

View(CentralPeru)
#Paso 5 extracción OBIS records de ocurrencia de diferentes grupos. El area se selecciona en obis-seleccionar AREAS LME. El numero que se encuentra en la url es el código que necesitamos.En mol code será el codigo del grupo concreto como Gorgoonidae. 

#Para humboldt
area=40013
depth=30
gorg_code=125275

#Para Gulf of California
area2=40004

#Para California current
area3=40003

#Para Pacific Central-American Coastal
area4=40011
#leer datos from OBIS

gorgoniidae.100_2=occurrence(taxonid = gorg_code, enddepth = depth, geometry = "POLYGON((-80.41992 -3.77656, -84.72656 -3.60114, -83.93555 1.84538, -79.54102 1.58183, -80.41992 -3.77656))")
gorgoniidae.100_3=occurrence(areaid = area2, taxonid = gorg_code, enddepth = depth)
gorgoniidae.100_4=occurrence(areaid = area3, taxonid = gorg_code, enddepth = depth)
gorgoniidae.100_5=occurrence(areaid = area4, taxonid = gorg_code, enddepth = depth)

View(gorgoniidae.100_2)
map_leaflet(gorgoniidae.100_2)
class(gorgoniidae.100_2)

library(rgdal)
crs=CRS("+init=epsg:4326")
gorgoniidae.100_2=SpatialPointsDataFrame(gorgoniidae.100_2[,c("decimalLongitude","decimalLatitude")],data=gorgoniidae.100_2, proj4string= crs)#identifica la espacialidad
Capa<-st_read("~/Desktop/JUDITH/+MASTER ECOLOGIA MARINA /+TESIS MASTER/ECOREGIONES/Marine Ecoregions of the World-2/data/commondata/data0/meow_ecos_expl_clipped_expl.shp")
plot(st_geometry(Capa), axis=T, xlim=c(-90,-75), ylim=c(-5, 2), col=colors, border="black")
plot(st_geometry(tierra), col="grey", add=T, border="black")
plot(gorgoniidae.100_2, add=T, pch=21, col="black", bg="red", cex= 1.5)

ramp = rainbow(length(unique(Capa$ECOREGION)))
colors = ramp[as.factor(Capa$ECOREGION)]


library(tidyverse) #Incluye a ggplot2
library(rerddap)
library(raster)
library(rnaturalearth)
tierra <- rnaturalearth::ne_countries(returnclass = "sf")



gorgoniidae.100_2




##mapview::

gorgoniidae.100_2=occurrence(areaid = area, taxonid =125275)
sub_gorgo1 <- data.frame(gorgoniidae.100_2$date_year, gorgoniidae.100_2$scientificName) 
sub_gorgo2 <- data.frame(gorgoniidae.100_3$date_year, gorgoniidae.100_3$scientificName) 
sub_gorgo3 <- data.frame(gorgoniidae.100_4$date_year, gorgoniidae.100_4$scientificName)
sub_gorgo4 <- data.frame(gorgoniidae.100_5$date_year, gorgoniidae.100_5$scientificName)

#cambiamos los headers de las columnas
names(sub_gorgo1)[names(sub_gorgo)=="gorgoniidae.100_2.date_year"]<-"year"
names(sub_gorgo1)[names(sub_gorgo)=="gorgoniidae.100_2.scientificName"]<-"species"

names(sub_gorgo2)[names(sub_gorgo2)=="gorgoniidae.100_3.date_year"]<-"year"
names(sub_gorgo2)[names(sub_gorgo2)=="gorgoniidae.100_3.scientificName"]<-"species"

names(sub_gorgo3)[names(sub_gorgo3)=="gorgoniidae.100_4.date_year"]<-"year"
names(sub_gorgo3)[names(sub_gorgo3)=="gorgoniidae.100_4.scientificName"]<-"species"

names(sub_gorgo4)[names(sub_gorgo4)=="gorgoniidae.100_5.date_year"]<-"year"
names(sub_gorgo4)[names(sub_gorgo4)=="gorgoniidae.100_5.scientificName"]<-"species"


total.100<-bind_rows(sub_gorgo1, sub_gorgo2, sub_gorgo3, sub_gorgo4)

gorgoniidae.100_2$scientificName
levels(as.factor(gorgoniidae.100_2$scientificName))

#Graficamos




grafico <-ggplot()+
  geom_histogram(data=total.100, aes(x=year, fill=species), binwidth = 2) +
  scale_fill_brewer(palette = "Spectral")+
  xlim(c(1941,2020))+
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(axis.text.x = element_text(size=14, angle=0), 
        axis.text.y = element_text(size=14, angle=0))
grafico

ggsave(grafico, filename = "test.png", device = "png", width = 20, height = 10,  dpi=300)


## Plot pie chart 
# Group plants in a single group
all_tbl <- table(total.100$species)
all_df <- as.data.frame(all_tbl)
p_list = c("Leptogorgia", "Eugorgia", "Psammogorgia")
rest_list = c("Gorgoniidae")
p_idx = match(p_list, rownames(all_tbl))
rest_idx = match(rest_list, rownames(all_tbl))
p_sum = sum(all_df$Freq[p_idx], na.rm = TRUE)
freq_val <- c(all_df$Freq[rest_idx], p_sum)
group_id <- c(c(rest_list), "Coral")
f_tbl <- data.frame(group = group_id, freq = freq_val)

cols <- rainbow(nrow(f_tbl))
groups_pie <- pie(f_tbl$freq, labels = f_tbl$group, col = cols)

#Extracción de SST and CHL data from selectd locations
library(readr)
library(rerddap)
library(lubridate)
library(dplyr)
library(flexdashboard)
library(reshape2)
library(leaflet)
library(ggplot2)
library(vegan)
library(xts)
library(dygraphs)
library(plotly)
library(mapdata)

library(RColorBrewer)
palette(brewer.pal(8, "Set2"))

## remove all spaces from string
NoSpaces = function(x){
  return(gsub(" ", "", x))
}

SSTSiteName = "Patagonia"   ## for the resulting file name
SSTcoords.lon = -63.
SSTcoords.lat = -42.5
SSTstartDate = "2002-06-01"

## set climatological date start-end
SSTclimStartDate = "2002-06-01"
SSTclimEndDate = "2012-12-31"
## set dataset source
SSTsource = info("jplMURSST41")
##
## Get sst 
SST <- griddap(SSTsource, 
               time=c(SSTstartDate, "last"),
               longitude = c(SSTcoords.lon,SSTcoords.lon),
               latitude = c(SSTcoords.lat,SSTcoords.lat),
               fields = "analysed_sst",
               fmt = "csv")

SST = SST[,c(1,4)]
names(SST) = c("time", "SST")

## convert time to a Data object
SST$time = as.Date(ymd_hms(SST$time))

#PASO 2 calculando SST climatologico
SST.clim = SST %>% filter(time>=ymd(SSTclimStartDate), time<=SSTclimEndDate) %>% 
  group_by(yDay = yday(time)) %>% 
  summarise(SST.mean = mean(SST),
            SST.median = median(SST),
            SST.sd = sd(SST),
            SST.q5 = quantile(SST, 0.05),
            SST.q10 = quantile(SST, 0.10),
            SST.q25 = quantile(SST, 0.25),
            SST.q75 = quantile(SST, 0.75),
            SST.q90 = quantile(SST, 0.90),
            SST.q95 = quantile(SST, 0.95),
            SST.min = min(SST),
            SST.max = max(SST))

#PASO 3 Plot SST times series

SST.xts = as.xts(SST$SST, SST$time)
dygraph(SST.xts, 
        ylab = "Sea Surface Temperature (Deg C)") %>% 
  dySeries("V1", label ="SST (Deg C)", color = "steelblue") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>% 
  dyOptions(fillGraph = FALSE, fillAlpha = 0.4) %>% 
  dyRangeSelector(dateWindow = c(max(SST$time) - years(5), max(SST$time)))

## subset SST for last year
SST.lastyear = SST %>% filter(year(time)==max(year(time)))

## make the plot
pp = ggplot(SST.clim, aes(yDay, SST.mean))
pp = pp + geom_line() + geom_smooth(span=0.25, se=FALSE, colour="steelblue") +  
  geom_ribbon(aes(ymin=SST.q25, ymax=SST.q75), fill="steelblue", alpha=0.5) +
  geom_line(data=SST.lastyear, aes(yday(time), SST), colour="red") + 
  ylab("Sea Surface Temperature (Deg C)") + xlab("Day of the Year") + 
  theme_bw(base_size = 9) 
ggplotly(pp) %>% plotly::config(displayModeBar = F) 

`geom_smooth()` using method = 'loess' and formula 'y ~ x'

#Paso4 Guardar los time series SST

write_csv(SST, path = paste0(NoSpaces(SSTSiteName), "_SST.csv"))
write_csv(SST.clim, path = paste0(NoSpaces(SSTSiteName), "_Climatology.csv"))

#Paso 5 crear mapa con los datos 

sstInfo <- info('jplMURSST41')
# get latest 3-day composite sst
GHRSST <- griddap(sstInfo, latitude = c(-60., -20.), longitude = c(-90., -47.), time = c('last','last'), fields = 'analysed_sst')

mycolor <- colors$temperature
w <- map_data("worldHires", ylim = c(-60., -20.), xlim = c(-90., -47.))
ggplot(data = GHRSST$data, aes(x = lon, y = lat, fill = analysed_sst)) + 
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(-90., -47.),  ylim = c(-60., -20.)) + ggtitle("Latest daily SST data")

#Paso 6 pedir CHL data de ERDDAP

## remove all spaces from string
NoSpaces = function(x){
  return(gsub(" ", "", x))
}

## set site coordinates and time for CHL extraction
CHLSiteName = "Patagonia"   ## for the resulting file name
CHLcoords.lon = -63
CHLcoords.lat = 42.5

CHLstartDate = "2012-01-01"

## set climatological date start-end
CHLclimStartDate = "2012-01-01"
CHLclimEndDate = "2016-12-31"

## set dataset source
CHLsource = info("erdMH1chla8day")

##
## Get CHL 
CHL <- griddap(CHLsource, 
               time=c(CHLstartDate, "last"),
               longitude = c(CHLcoords.lon,CHLcoords.lon),
               latitude = c(CHLcoords.lat,CHLcoords.lat),
               fields = "chlorophyll", fmt = "csv")

CHL = CHL[,c(1,4)]
names(CHL) = c("time", "CHL")
CHL = na.omit(CHL)

## convert time to a Data object
CHL$time = as.Date(ymd_hms(CHL$time))

#Paso 7 calcular la climatologia de CHL

CHL.clim = CHL %>% filter(time>=ymd(CHLclimStartDate), time<=CHLclimEndDate) %>% 
  group_by(yDay = yday(time)) %>% 
  summarise(CHL.mean = mean(CHL),
            CHL.median = median(CHL),
            CHL.sd = sd(CHL),
            CHL.q5 = quantile(CHL, 0.05),
            CHL.q10 = quantile(CHL, 0.10),
            CHL.q25 = quantile(CHL, 0.25),
            CHL.q75 = quantile(CHL, 0.75),
            CHL.q90 = quantile(CHL, 0.90),
            CHL.q95 = quantile(CHL, 0.95),
            CHL.min = min(CHL),
            CHL.max = max(CHL))

#Paso 8 graficar el plot de CHL time series




