## applied-spatial-analysis.R
##

ASDAR_BOOK <- "http://www.asdar-book.org/book2ed"
chapters <- c("hello", "cm", "vis", "die", "cm2",
              "std", "sppa", "geos", "lat", "dismap")
# setwd() # move to download folder
for (i in chapters) {
  fn <- paste(i, "mod.R", sep="_")
  download.file(paste(ASDAR_BOOK, fn, sep = "/"), fn)
}
list.files()



## 2.3 Spatial Objects
library(sp)
## eastings before northings
getClass("Spatial") ## 2 slots: bbox and proj4string
getClass("CRS") ## Coordinate Refence System

(m <- matrix(c(0,0,1,1), ncol = 2, dimnames=list(NULL, c("min", "max"))))
(crs <- CRS(projargs = as.character(NA)))
(S <- Spatial(bbox = m, proj4string = crs))

bb <- matrix(c(350,85,370,95), ncol = 2, dimnames=list(NULL, c("min","max")))
Spatial(bbox = bb, proj4string = CRS("+proj=longlat"))

## 2.4 SpatialPoints

CRAN_df <- read.table("ch2/CRAN051001a.txt", header = TRUE)
CRAN_mat <- cbind(CRAN_df$long, CRAN_df$lat)
row.names(CRAN_mat) <- 1:nrow(CRAN_mat)
(CRAN_mat)

getClass("SpatialPoints")
llCRS <- CRS("+proj=longlat +ellps=WGS84")
CRAN_sp <- SpatialPoints(CRAN_mat, proj4string = llCRS)
summary(CRAN_sp)

## 2.4.1 Methods
bbox(CRAN_sp)
proj4string(CRAN_sp)
proj4string(CRAN_sp) <- CRS(as.character(NA))
proj4string(CRAN_sp) <- llCRS


(brazil <- which(CRAN_df$loc=="Brazil"))
coordinates(CRAN_sp)[brazil,]

## 2.4.2 Data Frames for Spatial Point Data
str(row.names(CRAN_df))

CRAN_spdf1 <- SpatialPointsDataFrame(coords = CRAN_mat, 
                                     data = CRAN_df, 
                                     proj4string = llCRS, 
                                     match.ID = TRUE)
CRAN_spdf1[4,]
str(CRAN_spdf1$loc)
str(CRAN_spdf1[["loc"]])

s <- sample(nrow(CRAN_df))
CRAN_spdf2 <- SpatialPointsDataFrame(CRAN_mat, 
                                     CRAN_df[s,], 
                                     proj4string = llCRS, 
                                     match.ID = TRUE)
CRAN_spdf1[4,]
CRAN_spdf2[4,]

getClass("SpatialPointsDataFrame")
## generic methods like names, str work as expected for data.frame objects
names(CRAN_spdf1)
names(CRAN_spdf1@data)

## alt approaches to construction SpatialPointsDataFrames

CRAN_spdf4 <- SpatialPointsDataFrame(CRAN_sp, CRAN_df)
all.equal(CRAN_spdf4, CRAN_spdf2)

CRAN_df0 <- CRAN_df
coordinates(CRAN_df0) <- CRAN_mat
proj4string(CRAN_df0) <- llCRS
all.equal(CRAN_df0, CRAN_spdf2)

turtle_df <- read.csv("ch2/seamap105_mod.csv")
summary(turtle_df)
head(turtle_df)
timestamp <- as.POSIXlt(strptime(as.character(turtle_df$obs_date), "%m/%d/%Y %H:%M:%S"),"GMT")
turtle_df1 <- data.frame(turtle_df, timestamp = timestamp)
head(turtle_df1)
turtle_df1$lon <- ifelse(turtle_df1$lon < 0, turtle_df1$lon + 360, turtle_df1$lon)
turtle_sp <- turtle_df1[order(turtle_df1$timestamp),]
coordinates(turtle_sp) <- c("lon", "lat")
proj4string(turtle_sp) <- CRS("+proj=longlat +ellps=WGS84")

## 2.5 SpatialLines
getClass("Line")
getClass("Lines")
getClass("SpatialLines")

library("maps")
japan <- map("world", "japan", plot = FALSE)
p4s <- CRS("+proj=longlat +ellps=WGS84")  
library(maptools)
SLjapan <- map2SpatialLines(japan, proj4string = p4s)
str(SLjapan, max.level = 2)  

Lines_len <- sapply(slot(SLjapan, "lines"), function(x) length(slot(x,"Lines")))
table(Lines_len)  
  
volcano_s1 <- ContourLines2SLDF(contourLines(volcano))
t(slot(volcano_s1, "data"))

llCRS <- CRS("+proj=longlat +ellps=WGS84")
auck_shore <- MapGen2SL("ch2/auckland_mapgen.dat", llCRS)
summary(auck_shore)

## 2.6 SpatialPolygons
lns <- slot(auck_shore, "lines")
table(sapply(lns, function(x) length(slot(x,"Lines"))))
islands_auck <- sapply(lns, function(x){
  crds <- slot(slot(x,"Lines")[[1]], "coords")
  identical(crds[1,],crds[nrow(crds),])
})
table(islands_auck)

getClass("Polygon")
getClass("Polygons")
getClass("SpatialPolygons")

## 2.6.1 SpatialPolygonsDataFrame

