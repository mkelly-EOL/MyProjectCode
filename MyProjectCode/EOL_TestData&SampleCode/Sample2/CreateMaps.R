
#Set working directory
# getwd()
# setwd( "D:\\HSR/GitHub\\EOL_SampleCode\\Sample2")
# getwd()



#install.packages("data.table")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("RColorBrewer"

library(data.table)
library(raster)
library(rgdal)
library(RColorBrewer)
library(sf)
library(tmap)
require(car)


IrelandGeo <- shapefile("D:\\HSR/GitHub\\EOL_SampleCode\\Sample2\\Census2011_Admin_Counties_generalised20m\\Census2011_Admin_Counties_generalised20m.shp")
#plot(IrelandGeo)
str(IrelandGeo, max.level = 2)
IrelandGeo@data

#Add colum for merge to HBCounties data from  COUNTYNAME field
IrelandGeo$fm_col <-  IrelandGeo$COUNTYNAME
IrelandGeo@data 

##Tidy up fm_col so merge will work correctly 
IrelandGeo$fm_col[IrelandGeo$fm_col== 'North Tipperary'] <- 'Tipperary North'
IrelandGeo$fm_col[IrelandGeo$fm_col == 'South Tipperary'] <- 'Tipperary South'
IrelandGeo$fm_col[IrelandGeo$fm_col == 'South Dublin'] <- 'Dublin'
IrelandGeo$fm_col[IrelandGeo$fm_col == 'Fingal'] <- 'Dublin'
IrelandGeo$fm_col[IrelandGeo$fm_col == 'Dún Laoghaire-Rathdown'] <- 'Dublin'

IrelandGeo$fm_col[IrelandGeo$fm_col == 'Dublin City'] <- 'Dublin'
IrelandGeo$fm_col[IrelandGeo$fm_col == 'Waterford City'] <- 'Waterford'
IrelandGeo$fm_col[IrelandGeo$fm_col == 'Cork City'] <- 'Cork'
IrelandGeo$fm_col[IrelandGeo$fm_col == 'Limerick City'] <- 'Limerick'
IrelandGeo$fm_col[IrelandGeo$fm_col == 'Galway City'] <- 'Galway'
#IrelandGeo@data

#IrelandGeo@data
#Make fm_col uppercase and remove word county
IrelandGeo$fm_col <- toupper(gsub('*.County','\\1', IrelandGeo$fm_col))
#IrelandGeo@data
#Get merge data with healthboard information
CountiesHB<- read.csv("D:\\HSR/GitHub\\EOL_SampleCode\\Sample2\\HBCounties.csv")
dtCountiesHB <- data.table(CountiesHB)
View(dtCountiesHB)
#Tidy data so map legends are neat
View(dtCountiesHB)
dtCountiesHB$HealthBoard <- recode(dtCountiesHB$HB, " 'NEHB' = 'North Eastern'; 'SEHB' = 'South Eastern';'WHB' = 'Western'; 'MHB' = 'Midlands';'EHB' = 'Eastern'; 'NWHB' = 'North Western';'SHB' = 'Southern';'MWHB' = 'Mid Western'")

#Reorder levels to match table and figures

dtCountiesHB$HealthBoard1 <- as.factor(dtCountiesHB$HealthBoard)
levels(dtCountiesHB$HealthBoard1)
dtCountiesHB$HealthBoard1 <- factor(dtCountiesHB$HealthBoard1 , levels = c('North Eastern','South Eastern', 'Western','Midlands','Eastern','North Western','Southern','Mid Western'))
levels(dtCountiesHB$HealthBoard1)
setcolorder(CountiesHB,c(2,4,1,3, 5))
View(dtCountiesHB)


#Get hospice coordinates
Hospicelonlat <- read.csv("D:\\HSR/GitHub\\EOL_SampleCode\\Sample2\\Hospicelonlat.csv")
dtHospicelonlat <- data.table(Hospicelonlat)
View(dtHospicelonlat)
#Convert to a shape file
lonlat <- st_as_sf(dtHospicelonlat, coords = c("lon", "lat"), crs = 4326, agr = "constant")

#Prepare base  map file
IrelandGeo1 <-  merge(IrelandGeo,dtCountiesHB,  by.x ="fm_col", by.y ="COUNTY", all.x = TRUE)
IrelandGeo1@data

#Remove unwanted labels
IrelandGeo1$fm_col[IrelandGeo1$COUNTYNAME == 'Dublin City'] <- ''
IrelandGeo1$fm_col[IrelandGeo1$COUNTYNAME == 'Waterford City'] <- ''
IrelandGeo1$fm_col[IrelandGeo1$COUNTYNAME == 'Cork City'] <- ''
IrelandGeo1$fm_col[IrelandGeo1$COUNTYNAME == 'Limerick City'] <- ''
IrelandGeo1$fm_col[IrelandGeo1$COUNTYNAME == 'Galway City'] <- ''
IrelandGeo1$fm_col[IrelandGeo1$COUNTYNAME == 'Dún Laoghaire-Rathdown'] <- ''
#IrelandGeo1$fm_col[IrelandGeo1$COUNTYNAME == 'Fingal'] <- ''
#IrelandGeo1$fm_col[IrelandGeo1$COUNTYNAME == 'South Dublin'] <- ''


#tmaps
M1 <- tm_shape(IrelandGeo1) +
  tm_fill("HealthBoard1", title = "Health Boards", palette = 'Spectral' ) +
  tm_text("fm_col", size=0.5 ) +
  tm_layout( legend.outside = TRUE,legend.outside.size = 0.4, frame = FALSE) +
  tm_borders(alpha=0.9) + 
  tm_shape(lonlat) +
  tm_dots(NA, size = 0.3)

M1
