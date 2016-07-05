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
library("maptools")
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
library(maps)
library(maptools)
state.map <- map("state", plot = FALSE, fill = TRUE)
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])

state.sp <- map2SpatialPolygons(state.map, IDs = IDs, proj4string = CRS("+proj=longlat +ellps=WGS84"))

sat <- read.table("ch2/state.sat.data_mod.txt", row.names = 5, header = TRUE)
str(sat)
id <- match(row.names(sat), row.names(state.sp))
row.names(sat)[is.na(id)]
sat1 <- sat[!is.na(id),]
state.spdf <- SpatialPolygonsDataFrame(state.sp, sat1)
str(slot(state.spdf, "data"))
str(state.spdf, max.level=2)

## error signaled if row names don't match
rownames(sat1)[2] <- "foobar"
SpatialPolygonsDataFrame(state.sp, sat1)
rownames(sat1)[2] <- "arizona"
SpatialPolygonsDataFrame(state.sp, sat1)

DC <- "district of columbia"
not_dc <- !(row.names(state.spdf)==DC)
state.spdf1 <- state.spdf[not_dc,]
dim(state.spdf1)
summary(state.spdf1)

## 2.6.2 Holes and Ring Direction
## clockwise (ring is not hole); counter clockwise (ring is hole)
library(rgeos)
library(rgdal)
## 2.8 Raster Objects and the raster Package
library(raster)
r <- raster("ch2/70042108.tif")
class(r)
inMemory(r)
object.size(r)
cellStats(r, max)
cellStats(r, min)
inMemory(r)

## remove vals <= 0, since only care abt land elevation
out <- raster(r)
bs <- blockSize(out)
out <- writeStart(out, filename = tempfile(), overwrite = TRUE)
for (i in 1:bs$n) {
  v <- getValues(r, row = bs$row[i], nrows = bs$nrows[i])
  v[v <= 0] <- NA
  writeValues(out, v, bs$row[i])
}
out <- writeStop(out)
cellStats(out, min)
cellStats(out,max)
inMemory(out)
plot(out, col = terrain.colors(100))

r1 <- as(out, "SpatialGridDataFrame")
summary(r1)

## Chaper 3 -- Visualizing Spatial Data
## traditional - allows incremental addition to plots
## grid - does not allow incremental addition

## 3.1 The Traditional Plotting System
## 3.1.1 Plotting Points, Lines, Polygons, and Grids

library(sp)
data(meuse)
coordinates(meuse) <- c("x", "y") ## transforms df to SpatialPointsDataFrame
head(meuse)
plot(meuse)
title("points")

cc <- coordinates(meuse)
cc
m.s1 <- SpatialLines(list(Lines(list(Line(cc)),"line1")))
plot(m.s1)
title("lines")

data(meuse.riv)
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)),"meuse.riv"))
meuse.pol <- SpatialPolygons(meuse.lst)
plot(meuse.pol, col="grey")
title("polygons")

data(meuse.grid)
class(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixels")
image(meuse.grid, col="grey")
title("grid")

image(meuse.grid, col = "lightgrey")
plot(meuse.pol, col="grey", add = TRUE)
# lines(meuse.pol, col="red") ## alt to above, adds automatically
plot(meuse, add = TRUE)

## 3.1.2 Axes & Layout Elements

layout(matrix(c(1,2),1,2))
plot(meuse.pol, axes = TRUE)
plot(meuse.pol, axes = FALSE)
axis(1, at = c(178000 + 0:2 * 2000), cex.axis=0.7)
axis(2, at = c(326000 + 0:3 * 4000), cex.axis=0.7)
box()

oldpar <- par(no.readonly = TRUE)
layout(matrix(c(1,2),1,2))
plot(meuse, axes = TRUE, cex = 0.6)
plot(meuse.pol, add = T)
title("Sample locations")
par(mar = c(0,0,0,0) + 0.1)
plot(meuse, axes = FALSE, cex = 0.6)
plot(meuse.pol, add = T)
box()
par(oldpar)

# par(mfrow=c(1,1))
# plot(meuse)
# plot(meuse.pol, add = T)
# plot(meuse)
# SpatialPolygonsRescale(layout.scale.bar(), 
#                        offset = locator(1), 
#                        scale = 1000, 
#                        fill = c("transparent", "black"), 
#                        plot.grid = F)
# text(locator(1),"0") ## need to click on plot
# text(locator(1),"1 km")
# SpatialPolygonsRescale(layout.north.arrow(), 
#                        offset=locator(1), 
#                        scale=400, 
#                        plot.grid=FALSE)

## 3.1.3 Degree in Axes Labels & Reference Grid
library(maptools)
library(maps)
wrld <- map("world", 
            interior = FALSE, 
            xlim = c(-179, 179), 
            ylim=c(-89, 89), 
            plot = FALSE)
wrld_p <- pruneMap(wrld, xlim = c(-179, 179))
llCRS <- CRS("+proj=longlat + ellpsWGS84")
wrld_sp <- map2SpatialLines(wrld_p, proj4string=llCRS)
prj_new <- CRS("+proj=moll")
library(rgdal)
wrld_proj <- spTransform(wrld_sp, prj_new)
wrld_grid <- gridlines(wrld_sp, easts = c(-179, seq(-150, 150, 50), 179.5), norths = seq(-75, 75, 15), ndiscr = 100)
wrld_grid_proj <- spTransform(wrld_grid, prj_new)
at_sp <- gridat(wrld_sp, easts = 0, norths = seq(-75, 75, 15), offset = 0.3)
at_proj <- spTransform(at_sp, prj_new)


par(mfrow=c(1,1))
plot(wrld_proj, col="grey60")
plot(wrld_grid_proj, add = T, lty = 3, col="grey70")
text(coordinates(at_proj), pos = at_proj$pos, offset = at_proj$offset, labels = parse(text=as.character(at_proj$labels)), cex=0.6)
title("Da World")

## 3.1.4 Plot Size, Plotting Area, Map Scale, & Multiple Plots
par("pin") ## total size of figure
par(pin = c(4,4))
##dev.off() ## turn off plotting
##windows(width=10, height = 10) ## X11() <- unix; quartz() <- mac
##pdf("file.pdf", width = 5, height = 7)

pin <- par("pin")
dxy <- apply(bbox(meuse), 1, diff)
ratio <- dxy[1]/dxy[2]
par(pin=c(ratio*pin[2],pin[2]), xaxs = "i", yaxs = "i")
plot(meuse, pch = 3)
box()

## 3.1.5 Plotting Attributes and Map Legends
grays <- gray.colors(4, 0.55, 0.95)
image(zn.idw, col = grays, breaks = log(c(100,200,400,800,1800)))

## 3.2.1 Straight Trellis Example
head(meuse)

library(lattice)
data(meuse)
levelplot(z ~ x + y / name, spmap.to.lev(meuse$zn[c("direct","log")]),asp="iso")

library(maptools)
data(meuse.grid)
coordinates(meuse.grid) <- c("x","y")
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")
im <- as.image.SpatialGridDataFrame(meuse.grid["dist"])
c1 <- ContourLines2SLDF(contourLines(im))
spplot(c1)

## 3.2.3 Adding Reference and Layout Elements to Plots
## example of build an sp.layout object

river <- list("sp.polygons", meuse.pol)
north <- list("SpatialPolygonsRescale", 
              layout.north.arrow(),
              offset = c(178750, 332500))
scale <- list("SpatialPolygonsRescale", 
              layout.scale.bar(), 
              offset = c(180200, 329800),
              scale = 1000,
              fill = c("transparent", "black"))
txt1 <- list("sp.txt", c(180200, 329950),"0")
txt2 <- list("sp.txt", c(181200, 329950),"1")
pts <- list("sp.points", meuse, pch=3, col = "black")
meuse.layout <- list(river, north, scale, txt1, txt2, pts)
spplot(zn["log"], sp.layout = meuse.layout)

## 3.2.4 Arranging Panel Layout
## 3.3 Alternatives Routes: ggplot, latticeExtra

library("ggplot2")
methods(fortify)

m <- as(meuse, "data.frame")
class(m)
ggplot(m) + aes(x,y) + geom_point() + coord_equal()



library("latticeExtra")
p <- spplot(meuse["zinc"])
head(meuse)
meuse["zinc"]


## 3.4 Interactive Plots
## 3.4.1 Interacting with Base Graphics

plot(meuse$lead)
meuse.id <- identify(coordinates(meuse))
class(meuse)

plot(meuse)
region <- locator(type="o")
n <- length(region$x)
p <- Polygon(cbind(region$x, region$y)[c(1:n, 1),], hole = FALSE)
ps <- Polygons(list(p), ID="region")
sps <- SpatialPolygons(list(ps))
plot(meuse[sps,], pch = 16, cex = 0.5, add = T)
names(meuse)

## identify county/polygon in which is clicked
library(maptools)
prj <- CRS("+proj=longlat +datum=NAD27")
nc_shp <- system.file("shapes/sids.shp", package = "maptools")[1]
nc <- readShapePoly(nc_shp, proj4string = prj)
plot(nc)
pt <- locator(type="p")


## 3.4.2 Interacting with spplot and Lattice Plots

## ERRORS

## 3.5 Colour Palettes and Class Intervals
## 3.5.1 Colour Palettes

rw.colors <- colorRampPalette(c("red", "white"))
image(meuse.grid["dist"], col = rw.colors(10))

library(RColorBrewer)
example(brewer.pal)
library("classInt")
pal <- brewer.pal(5, "Reds")
q5 <- classIntervals(meuse$zinc, n = 5, style="quantile")
q5
diff(q5$brks)
plot(q5, pal = pal)

fj5 <- classIntervals(meuse$zinc, n = 5, style="fisher")
fj5
diff(fj5$brks)
plot(fj5, pal = pal)

q5Colours <- findColours(q5, pal)
plot(meuse, col = q5Colours, pch = 19)

## Chapter 4 - Spatial Data Import and Export
library(rgdal)

NEWS <- "http://svn.osgeo.org/metacrs/proj/trunk/proj/NEWS"
PROJ4_NEWS <- readLines(NEWS)
lns <- grep("Release Notes|ESPG", PROJ4_NEWS)
head(PROJ4_NEWS[lns])

EPSG <- make_EPSG()
EPSG[grep("^# ED50$", EPSG$note),]

CRS("+init=epsg:4230")

ED50 <- CRS("+init=epsg:4230 +towgs84=-87,-96,-120,0,0,0,0")
ED50

## 4.1.3 Projection and Transformation
IJ.east <- as(char2dms("4d31'00\"E"), "numeric")
IJ.north <- as(char2dms("52d28'00\"N"), "numeric")
IJ.ED50 <- SpatialPoints(cbind(x=IJ.east, y = IJ.north), proj4string=ED50)
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
x <- as(dd2dms(coordinates(res)[1]), "character")
y <- as(dd2dms(coordinates(res)[2],TRUE),"character")
cat(x, y, "\n")
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat = TRUE) * 1000



library(maptools)
gzAzimuth(coordinates(IJ.ED50), coordinates(res))

proj4string(IJ.ED50) <- CRS("+init=epsg:4230")
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat = TRUE) * 1000
gzAzimuth(coordinates(IJ.ED50), coordinates(res))

EPSG[grep("Atlas", EPSG$note),1:2]

CRS("+init=epsg:2163")
proj <- projInfo("proj")
proj[proj$name == "laea",]
ellps <- projInfo("ellps")
ellps[grep("a=6370997", ellps$major),]

## geographical coordinates should be converted to decimal form

(IJ.dms.E <- "4d31'00\"E")
(IJ.dms.N <- "52d28'00\"N")

## convert char strings to class DMS objs
(IJ_east <- char2dms(IJ.dms.E))
(IJ_north <- char2dms(IJ.dms.N))
getSlots("DMS")
c(as(IJ_east, "numeric"), as(IJ_north, "numeric"))

## 4.2 Vector File Formats









