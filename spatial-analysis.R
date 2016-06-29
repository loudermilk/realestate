## R for Spatial Analysis and Mapping
## Exercises from book

library("GISTools")
library("RColorBrewer")
library("OpenStreetMap")
library("RgoogleMaps")
library("PBSmapping")
library("raster")


##
## Ch. 2
##


data(georgia)
appling <- georgia.polys[[1]]
plot(appling, asp = 1, type = 'n', xlab = 'Easting', ylab = 'Northing')
polygon(appling, density = 14, angle = 135)
polygon(appling, col=rgb(0,.5,.7))
polygon(appling, col=rgb(0,.5,.7, .4))
polygon(appling, col="#B3B333")
# locator() - find spots on plot
text(1287000, 1052627, "Appling County", cex=1.5)
text(1285756, 1046931, "GEORGIA", col='darkred')

data(meuse.grid)
mat <- SpatialPixelsDataFrame(points = meuse.grid[c("x","y")],data=meuse.grid)
class(mat)
dim(mat)
head(mat)
#set plot parameters
par(mfrow=c(1,2))
par(mar=c(0,0,0,0))
image(mat, "dist")
# examine color palatte with 7 classes
greenpal <- brewer.pal(7,'Greens')
image(mat, "dist", col=greenpal)
#reset par
par(mfrow=c(1,1))

##
## Ch.3
##

data(newhaven)
ls()
plot(roads)
plot(places)
plot(tracts)
plot(blocks)
plot(breach)

par(mar=c(0,0,0,0))
plot(blocks)
plot(roads, add = TRUE, col = "red")

plot(blocks)
plot(breach, col='red', add = TRUE)

# displlay all colors
colors()

## Creating a map - end to end
## save as pdf
pdf(file = "CT-map")

plot(blocks, lwd = 0.5, border = "grey50")
plot(breach, col = "red", pch =1, add = TRUE)

## add a scale
map.scale(534750, 152000, miles2ft(2), "Miles", 4, 0.5)

## add compass point
north.arrow(534750, 154000, miles2ft(.25), col="lightblue")
title("New Haven, CT.")
## write file
dev.off()
##

## 3.3 MAPPING SPATIAL OBJECTS
rm(list=ls())

data(georgia)
pdf(file = "GA-map")

#plot(georgia, col = "red", bg = "wheat")

georgia.outline <- gUnaryUnion(georgia, id = NULL)
plot(georgia, col = "red", bg = "wheat", lty = 2, border = "blue")
plot(georgia.outline, lwd = 3, add = TRUE)
title(main = "The State of Georgia", 
      font.main = 2, 
      cex.main = 1.5, 
      sub = "and its counties", 
      font.sub = 3, 
      col.sub = "blue")
dev.off()

## Set the number of maps per plat
par(mfrow=c(1,2))
par(mar=c(2,0,3,0))
plot(georgia, col = "red", bg = "wheat")
title("georgia")
plot(georgia2, col = "orange", bg = "lightyellow")
title("georgia2")
## reset mfrow
par(mfrow = c(1,1))

## Do something with the names
names(data.frame(georgia))
(Names <- data.frame(georgia)[,13])

Lat <- data.frame(georgia)[,1]
Lon <- data.frame(georgia)[,2]
par(mar = c(0,0,0,0))
plot(georgia, col = NA)
pl <- pointLabel(Lon, Lat, Names, offset = 0, cex = 0.5)

## plot subregions
county.tmp <- c(81,82,83,150,62,53,21,16,124,121,17)
georgia.sub <- georgia[county.tmp,]
par(mar = c(0,0,3,0))
plot(georgia.sub, col = "gold1", border = "grey")
plot(georgia.outline, add = TRUE, lwd = 2)
title("A subset of Georgia", cex.main = 2, font.main = 1)
pl <- pointLabel(Lon[county.tmp], Lat[county.tmp], Names[county.tmp], offset = 3, cex = 1.5)

plot(georgia, border = "grey", lwd = 0.5)
plot(georgia.sub, add = TRUE, col = "lightblue")
plot(georgia.outline, lwd = 2, add = TRUE)
title("Georgia with a subset of counties")


## Using OpenStreetMap
## define the upper-left and lower-roght conrners
ul <- as.vector(cbind(bbox(georgia.sub)[2,2],
                      bbox(georgia.sub)[1,1]))
lr <- as.vector(cbind(bbox(georgia.sub)[2,1],
                      bbox(georgia.sub)[1,2]))
MyMap <- openmap(ul, lr, 9, 'mapquest')
par(mar=c(0,0,0,0))
plot(MyMap, removeMargin = FALSE)
plot(spTransform(georgia.sub, osm()), add = TRUE, lwd = 2)

## using GoogleMaps
shp <- SpatialPolygons2PolySet(georgia.sub)
bb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"])
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.jpg")
par(mar = c(0,0,0,0))
PlotPolysOnStaticMap(MyMap, shp, lwd = 2, 
                     col = rgb(.25,.25,.25,.25), add = FALSE)
par(mar=c(5,4,4,2))


## 3.4.2 Attributes and Data Frames
rm(list=ls())
data(newhaven)
ls()
summary(blocks)
summary(breach)
summary(tracts)

blocks_df <- data.frame(blocks)
blocks_df$P_VACANT
hist(blocks_df$P_VACANT)

breach.dens <- kde.points(breach, lims = tracts)
summary(breach.dens)
head(data.frame(breach.dens))

breach.dens.grid <- as(breach.dens, "SpatialGridDataFrame")
summary(breach.dens.grid)

## 3.4.3 Mapping Polygons and Attributes
choropleth(blocks, blocks$P_VACANT)
vacant.shades <- auto.shading(blocks$P_VACANT)
vacant.shades
choro.legend(533000,161000, vacant.shades)

vacant.shades <- auto.shading(blocks$P_VACANT, n = 7)
choropleth(blocks, blocks$P_VACANT, shading = vacant.shades)
choro.legend(531679,166071.3, vacant.shades)

## Changing colors

display.brewer.all()
brewer.pal(5,'Blues')

vacant.shades <- auto.shading(blocks$P_VACANT, 
                              cols = brewer.pal(5,"Greens"))
choropleth(blocks, blocks$P_VACANT, shading = vacant.shades)
choro.legend(533000,161000,vacant.shades)

## 3.4.4 Mapping Points and Attributes

plot(blocks)
plot(breach, add = TRUE, col = add.alpha(brewer.pal(5,"Reds"), 0.5), pch=1)


brewer.pal(5,"Reds")
add.alpha(brewer.pal(5,"Reds"), 0.5)

## Transforming lat/lon to spatial points
data(quakes)
head(quakes)

coords.tmp <- cbind(quakes$long, quakes$lat)
quakes.spdf <- SpatialPointsDataFrame(coords.tmp, data = data.frame(quakes))
par(mar = c(0,0,0,0))
par(mfrow = c(1,2))
plot(quakes.spdf)
plot(quakes.spdf, pch = 1, col = 'orange')
par(mfrow = c(1,1))

data(georgia)
county_index <- c(1,3,151,113)
tmp <- georgia.polys[county_index]
t1 <- Polygon(tmp[1]); t1 <- Polygons(list(t1), "1")
t2 <- Polygon(tmp[2]); t2 <- Polygons(list(t2), "2")
t3 <- Polygon(tmp[3]); t3 <- Polygons(list(t3), "3")
t4 <- Polygon(tmp[4]); t4 <- Polygons(list(t4), "4")
tmp.Sp <- SpatialPolygons(list(t1,t2,t3,t4), 1:4)
plot(tmp.Sp, col=2:5)
names <- georgia[,13]
tmp.names <- names[[1]][county_index]
tmp.spdf <- SpatialPolygonsDataFrame(tmp.Sp, data = data.frame(tmp.names))
plot(tmp.spdf, col = 2:5)


par(mfrow = c(2,2))
par(mar = c(0,0,0,0))
choropleth(quakes.spdf, quakes$mag)

shades <- auto.shading(quakes$mag, n = 6, cols = brewer.pal(6,'Greens'))
choropleth(quakes.spdf, quakes$mag, shades, pch = 1)
shades$cols <- add.alpha(shades$cols, 0.5)
choropleth(quakes.spdf, quakes$mag, shading = shades, pch = 20)



tmp <- quakes$mag
tmp <- tmp - min(tmp)
tmp <- tmp / max(tmp) 
plot(quakes.spdf, cex = tmp*3, pch = 1, col = "red")


par(mfrow=c(1,2))
par(mar = c(0,0,0,0))
tmp2 <- cut(quakes$mag, fivenum(quakes$mag), include.lowest = TRUE)
class <- match(tmp2, levels(tmp2))
pch.var <- c(0,1,2,5)
plot(quakes.spdf, pch = pch.var[class], cex = 0.7)

library(RgoogleMaps)
par(mfrow=c(1,1))
Lat <- as.vector(quakes$lat)
Long <- as.vector(quakes$long)
MyMap <- MapBackground(lat = Lat, lon=Long, zoom = 10)
PlotOnStaticMap(MyMap, Lat, Long, cex = tmp + 0.3, pch = 1, col = "red")

MyMap <- MapBackground(lat = Lat, lon = Long, zoom = 10, maptype = "satellite")
PlotOnStaticMap(MyMap, Lat, Long, cex = tmp + 0.3, pch = 1, col = "red")


## 3.4.5 Mapping Lines and Attributes

data(newhaven)
xmin <- bbox(roads)[1,1]
ymin <- bbox(roads)[2,1]
xmax <- xmin + diff(bbox(roads)[1,]) /2
ymax <- ymin + diff(bbox(roads)[2,]) /2
xx <- as.vector(c(xmin, xmin, xmax, xmax, xmin))
yy <- as.vector(c(ymin, ymax, ymax, ymin, ymin))
crds <- cbind(xx,yy)
Pl <- Polygon(crds)
ID <- "clip"
Pls <- Polygons(list(Pl), ID = ID)
SPls <- SpatialPolygons(list(Pls))
df <- data.frame(value = 1, row.names = ID)
clip.bb <- SpatialPolygonsDataFrame(SPls, df)
roads.tmp <- gIntersection(clip.bb, roads, byid = T)
tmp <- as.numeric(gsub("clip", "", names(roads.tmp)))
tmp <- data.frame(roads)[tmp,]
roads.tmp <- SpatialLinesDataFrame(roads.tmp, data = tmp, match.ID = F)

par(mfrow=c(1,3))
par(mar = c(0,0,0,0))
plot(roads.tmp)
road.class <- unique(roads.tmp$AV_LEGEND)
shades <- rev(brewer.pal(length(road.class), "Spectral"))
tmp <- roads.tmp$AV_LEGEND
index <- match(tmp, as.vector(road.class))
plot(roads.tmp, col = shades[index], lwd = 3)
plot(roads.tmp, lwd = roads.tmp$LENGTH_MI * 10)

## 3.4.6 Mapping Raster Attributes
data(meuse.grid)
class(meuse.grid)
summary(meuse.grid)
plot(meuse.grid$x, meuse.grid$y, asp = 1)
meuse.grid <- SpatialPixelsDataFrame(points = meuse.grid[c("x","y")], data = meuse.grid)

par(mfrow=c(1,2))
par(mar=c(.25, .25, .25, .25))
image(meuse.grid, "dist", col = rainbow(7))
image(meuse.grid, "dist", col = heat.colors(7))

par(mar = c(.25,.25,.25,.25))
p1 <- spplot(meuse.grid,"dist", col.regions = terrain.colors(20))
print(p1, position = c(0,0,.5,.5), more = TRUE)
p2 <- spplot(meuse.grid, c("part.a", "part.b", "soil", "ffreq"), col.regions = topo.colors(20))
print(p2, position = c(.5,0,1,.5), more = TRUE)


## 3.5.1 Histograms and Boxplots
data(newhaven)

summary(blocks$P_VACANT)
hist(blocks$P_VACANT)
fivenum(blocks$P_VACANT)

hist(blocks$P_VACANT, breaks = 20, col = "cyan", border = "salmon",
     main = "Distribution of Vacant Property Percentages", xlab = "precent vacant",
     xlim=c(0,40))

index <- blocks$P_VACANT > 10
high.vac <- blocks[index,]
low.vac <- blocks[!index,]

cols <- rev(brewer.pal(3, "Blues"))
par(mfrow = c(1,2))
par(mar = c(2.5, 2, 3, 1))
attach(data.frame(high.vac))

boxplot(P_OWNEROCC, P_WHITE, P_BLACK, names = c("OwnerOCC", "White", "Black"),
        col = cols, cex.axis = 0.7, main = "High Vacancy")

resetPlotParams <- function() {
  par(mfrow = c(1,1))
  par(mar = c(5,4,4,2))
}

resetPlotParams()

## 3.5.2 Scatter Plots and Regressions
par(mfrow = c(1,2))
plot(blocks$P_VACANT/100, blocks$P_WHITE/100)
plot(blocks$P_VACANT/100, blocks$P_BLACK/100)

p.vac <- blocks$P_VACANT/100
p.w <- blocks$P_WHITE/100
p.b <- blocks$P_BLACK/100
mod.1 <- lm(p.vac ~ p.w)
mod.2 <- lm(p.vac ~ p.b)

fac <- 0.05
cols <- brewer.pal(6, "Spectral")
plot(jitter(p.vac, fac), jitter(p.w, fac), xlab = "Proportion Vacant", 
     ylab = "Proportion White / Proportion Black", col = cols[1], xlim = c(0,0.8))
points(jitter(p.vac, fac), jitter(p.b, fac), col = cols[6])
abline(a = coef(mod.1)[1], b = coef(mod.1)[2], lty = 1, col = cols[1])
abline(a = coef(mod.2)[1], b = coef(mod.2)[2], lty = 1, col = cols[6])
legend(.71, .19, legend = "Black", bty = "n", cex = .8)
legend(.71, .095, legend = "White", bty = "n", cex = .8)

## 3.5.3 Mosaic Plots
## for data where there is some kind or true/false statement, mosaic plots can be used
## to generate a powerful visualization of the statistical properties and relationships
## between variables
par(mfrow = c(1,1))
pops <- data.frame(blocks[,14:18]) * data.frame(blocks)[,11]
pops <- as.matrix(pops/100)
colnames(pops) <- c("White", "Black", "Ameri", "Asian", "Other")
vac.10 <- (blocks$P_VACANT > 10) + 0
mat.tab <- xtabs(pops ~vac.10)
mat.tab
ttext <- sprintf("Mosiac plot of vacant properties w ethnicity")
mosaicplot(t(mat.tab), xlab='', ylab='Vacant properties > 10 percent', main=ttext, shade=TRUE, las=3, cex=0.8)


## Chapter 5 - Using R as a GIS
library(GISTools)
data(tornados)


par(mar=c(0,0,0,0))
plot(us_states)
plot(torn, add = T, pch=1, col="orange", cex=0.4, alpha = 0.1)
plot(us_states, add = T)
summary(torn)

# do analysis on subset
index <- us_states$STATE_NAME == "Texas" |
  us_states$STATE_NAME == "New Mexico" |
  us_states$STATE_NAME == "Oklahoma" |
  us_states$STATE_NAME == "Arkansas"
AoI <- us_states[index,]
plot(AoI)
plot(torn, add = T, pch = 1, col="orange")

AoI.torn <- gIntersection(AoI, torn) #loses attributes
par(mar=c(0,0,0,0))
plot(AoI)
plot(AoI.torn, add = T, pch = 1, col="orange")
plot(AoI, add =T)


AoI.torn <- gIntersection(AoI, torn, byid = TRUE) #doesn't lose attributes

head(data.frame(AoI.torn))
row.names(data.frame(us_states[index,]))
us_states$STATE_NAME[index]

tmp <- rownames(data.frame(AoI.torn))
tmp <- strsplit(tmp, " ")
torn.id <- (sapply(tmp, "[[", 2))
state.id <- (sapply(tmp, "[[", 1))
torn.id <- as.numeric(torn.id)
df1 <- data.frame(torn[torn.id,])
head(df1)

df2 <- us_states$STATE_NAME[as.numeric(state.id)]
df <- cbind(df1, df2)
head(df)
names(df)[1] <- "State"
AoI.torn <- SpatialPointsDataFrame(AoI.torn, data = df)


index2 <- match(df2, us_states$STATE_NAME)
df3 <- data.frame(us_states)[index2,]  
df3 <- cbind(df2, df1, df3)
names(df3)[1] <- "State"
AoI.torn <- SpatialPointsDataFrame(AoI.torn, data = df3)

## 5.3 Buffers - display events *near* areas of interest
## tornados that occur in Texas and within 25 km of border

AoI <- us_states2[us_states2$STATE_NAME == "Texas",]
AoI.buf <- gBuffer(AoI, width = 25000)
par(mar=c(0,0,0,0))
plot(AoI.buf)
plot(AoI, add = T, border = "blue")

data("georgia")
buf.t <- gBuffer(georgia2, width = 5000, byid = T, id = georgia2$Name)
plot(buf.t)
plot(georgia2, add = T, border = "blue")

plot(buf.t[1,])
plot(georgia2[1,], add = T, border = "blue")

## 5.4 Merging Spatial Features

AoI.merge <- gUnaryUnion(us_states)
par(mar=c(0,0,0,0))
plot(us_states, border = "darkgreen", lty = 3)
plot(AoI.merge, add = T, lwd = 0.5)

## 5.5.1 Point-in-Polygon
torn.count <- poly.counts(torn, us_states)
head(torn.count)
names(torn.count)

## 5.5.2 Area Calculations

proj4string(us_states2)
poly.areas(us_states2) /(100*100) #hectacres
poly.areas(us_states2) /(1000*1000) #square kilometers

## 5.5.3 Point and Areas Analysis Exercise
data(newhaven)
densities <- poly.counts(breach, blocks) /
  ft2miles(ft2miles(poly.areas(blocks)))
cor(blocks$P_OWNEROCC, densities)
plot(blocks$P_OWNEROCC, densities)

breaches ~ Poisson(AREA * exp(a + b * blocks$P_OWNEROCC))

data(newhaven)
attach(data.frame(blocks))
n.breaches <- poly.counts(breach, blocks)
area <- ft2miles(ft2miles(poly.areas(blocks)))
model1 <- glm(n.breaches ~ P_OWNEROCC, offset = log(area), family = poisson)
detach(data.frame(blocks))

summary(model1)
s.resids <- rstandard(model1)
resid.shades <- shading(c(-2,2),c("red", "grey", "blue"))
par(mar=c(0,0,0,0))
choropleth(blocks, s.resids, resid.shades)
par(mar=c(5,4,4,2))

# 

attach(data.frame(blocks))
n.breaches <- poly.counts(breach, blocks)
area <- ft2miles(ft2miles(poly.areas(blocks)))
model2 <- glm(n.breaches ~ P_OWNEROCC+P_VACANT, offset = log(area), family = poisson)
s.resids.2 <- rstandard(model2)
detach(data.frame(blocks))

s.resids.2 <- rstandard(model2)
par(mar=c(0,0,0,0))
choropleth(blocks, s.resids.2, resid.shades)
par(mar=c(5,4,4,2))

## 5.6 Creating Distance Attributes

data(newhaven)
proj4string(places) <- CRS(proj4string(blocks))
centroids. <- gCentroid(blocks, byid = T, id = rownames(blocks))
distances <- ft2miles(gDistance(places, centroids., byid=T))
head(distances)
distances <- gWithinDistance(places, blocks, byid=T, dist=miles2ft(1.2))


## 5.6.1 Distance Analysis/Accessibility Exercise

distances <- ft2miles(gDistance(places, centroids., byid=T))
min.dist <- as.vector(apply(distances, 1, min))
access <- min.dist < 1
ethnicity <- as.matrix(data.frame(blocks[,14:18])/100)
ethnicity <- apply(ethnicity, 2, function(x)(x*blocks$POP1990))
colnames(ethnicity) <- c("White", "Black", "Native American", "Asian", "Other")

mat.access.tab <- xtabs(ethnicity~access)
data.set <- as.data.frame(mat.access.tab)
colnames(data.set) <- c("Access", "Ethnicity", "Freq")

modelethnic <- glm(Freq~Access*Ethnicity, data = data.set, family = poisson)
summary(modelethnic)$coef
mod.coefs <- summary(modelethnic)$coef
tab <- 100*(exp(mod.coefs[,1])-1)
tab <- tab[7:10]
names(tab) <- colnames(ethnicity)[2:5]
tab

mosaicplot(t(mat.access.tab),xlab='',ylab='Access to Supply', main="Mosaic plot of access", shade = TRUE, las =3, cex = 0.8)


## 5.7 Combining spatial datasets and their attributes
## dealing with different geounits

data(newhaven)
bb <- bbox(tracts)
grd <- GridTopology(cellcentre.offset = c(bb[1,1]-200, bb[2,1]-200),
                    cellsize = c(10000,10000), cells.dim = c(5,5))
int.layer <- SpatialPolygonsDataFrame(as.SpatialPolygons.GridTopology(grd), 
                                      data = data.frame(c(1:25)), match.ID = FALSE)
names(int.layer) <- "ID"
int.res <- gIntersection(int.layer, tracts, byid = TRUE)


par(mfrow = c(1,2))
par(mar=c(0,0,0,0))
plot(int.layer, lty=2)
Lat <- as.vector(coordinates(int.layer)[,2])
Lon <- as.vector(coordinates(int.layer)[,1])
Names <- as.character(data.frame(int.layer)[,1])
plot(tracts, add = T, border = "red", lwd = 2)
p1 <- pointLabel(Lon, Lat, Names, offset = 0, cex = 0.7)
plot(int.layer, border = "white")
plot(int.res, col=blues9, add = T)

names(int.res)


tmp <- strsplit(names(int.res), " ")
tracts.id <- (sapply(tmp, "[[", 2))
intlayer.id <- (sapply(tmp, "[[", 1))

## generate area and proportions
int.areas <- gArea(int.res, byid = TRUE)
tract.areas <- gArea(tracts, byid = TRUE)
## match this to the new layer
index <- match(tracts.id, row.names(tracts))
tract.areas <- tract.areas[index]
tract.prop <- zapsmall(int.areas/tract.areas, 3)
## create df for new layer
df <- data.frame(intlayer.id, tract.prop)
houses <- zapsmall(tracts$HSE_UNITS[index] * tract.prop, 1)
df <- data.frame(df, houses, int.areas)

int.layer.houses <- xtabs(df$houses~df$intlayer.id)
index <- as.numeric(gsub("g", "", names(int.layer.houses)))
## create temporary variable
tmp <- vector("numeric", length = dim(data.frame(int.layer))[1])
tmp[index] <- int.layer.houses
i.houses <- tmp
## attach outputs to original zone dataset
int.layer <- SpatialPolygonsDataFrame(int.layer, 
                                      data = data.frame(data.frame(int.layer),
                                                        i.houses),
                                      match.ID = FALSE)
## set plot parameters & shading variable
par(mar=c(0,0,0,0))
par(mfrow=c(1,1))
shades <- auto.shading(int.layer$i.houses,
                       n = 6,
                       cols = brewer.pal(6, "Greens"))
## map the data
choropleth(int.layer, int.layer$i.houses, shades)
plot(tracts, add = T)
choro.legend(530000, 159115, bg = "white", shades, title = "No. of houses", under = "")
## reset plot marins
par(mar=c(5,4,4,2))


## Converting between raster and vector
## 5.8.1 Raster to Vector
library(GISTools)
library(raster)
data(tornados)

## converting points
r <- raster(nrow = 180, ncols = 360, ext = extent(us_states2))
t2 <- as(torn2, "SpatialPoints")
r <- rasterize(t2, r, fun=sum) ## has cells describing tornado densities 
## set the plot extent by specifying color "white"
plot(r, col = "white")
plot(us_states2, add = T, border = "grey")
plot(r, add = T)

## converting lines
us_outline <- as(us_states2, "SpatialLinesDataFrame")
r <- raster(nrow = 180, ncols = 360, ext = extent(us_states2))
r <- rasterize(us_outline, r, "STATE_FIPS")
plot(r)

## converting polygons/areas
r <- raster(nrow = 180, ncols = 360, ext = extent(us_states2))
r <- rasterize(us_states2, r, "POP1997")
plot(r)

## 5.8.2 Coverting to sp Classes
r <- raster(nrow = 60, ncols = 120, ext = extent(us_states2))
r <- rasterize(us_states2, r, "STATE_FIPS")
## use function `as` to coerce to SpatialPixelsDataFrame & SpatialGridDataFrame
g <- as(r, 'SpatialGridDataFrame')
p <- as(r, 'SpatialPixelsDataFrame')
par(mar=c(0,0,0,0))
image(g, col = topo.colors(51))
points(p, cex = 0.5)

plot(p, cex = 0.5, pch = 1, col = p$layer)

head(data.frame(g))
head(data.frame(p))

r <- raster(nrow=60, ncols=120, ext = extent(us_states2))
r <- rasterize(us_states2, r, "POP1997")
r2 <- r
## subset the data
r2[r < 10000000] <- NA
g <- as(r2, "SpatialGridDataFrame")
p <- as(r2, "SpatialPixelsDataFrame")
par(mar=c(0,0,0,0))
plot(p, cex = 0.5, pch = 1)

## 5.8.3 Vector to Raster

## load data and convert to raster
data(newhaven)
r <- raster(nrow = 60, ncols = 60, ext = extent(tracts))
## convert polys to raster
r <- rasterize(tracts, r, "VACANT")
poly1 <- rasterToPolygons(r, dissolve = T)
## convert to points
points1 <- rasterToPoints(r)
## plot points, rasterized polys, & original polys
par(mar=c(0,0,0,0))
plot(points1, col = "grey", axes = F, xaxt='n', ann = F)
plot(poly1, lwd = 1.5, add = T)
plot(tracts, border = "red", add = T)

## 5.9 Introoduction to Raster Analysis
## 5.9.1 Raster Data Preparation

library(GISTools)
library(raster)
library(sp)
## load data
data(meuse.grid)
## create SpatialPixelsDataFrame
coordinates(meuse.grid) <- ~x+y
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")
## create threee raster layers
r1 <- raster(meuse.grid, layer = 3) ##dist
r2 <- raster(meuse.grid, layer = 4) ##soil
r3 <- raster(meuse.grid, layer = 5) ##ffreq
## visually inspect attributes
image(r1, asp=1)
image(r2, asp=1)
image(r3, asp=1)

## 5.9.2 Raster Reclassification

Raster_Result <- r2 + (r3 *10)
table(Raster_Result$values)
spplot(Raster_Result, col.regions=brewer.pal(9,"Spectral"), cuts=8)
# image(Raster_Result, asp = 1)

## Identify locations in meuse data that:
## (a) greater than half the re-scaled distance to Meuse River
## (b) have soil class of 1
## (c) have flood frequency class of 3

r1a <- r1 > 0.5
r2a <- r2 >= 2
r3a <- r3 < 3

## for all conditions true and crisp boolean boundary (intersection)
Raster_Result <- r1a * r2a * r3a
table(as.vector(Raster_Result@data@values))
plot(Raster_Result, legend = F, asp = 1)
legend(x='bottomright', legend=c("Suitable","Crappy"), fill=(terrain.colors(n=2)),bty="n")


## for one condition true (union)
Raster_Result <- r1a + r2a + r3a
table(as.vector(Raster_Result@data@values))
image(Raster_Result, col = heat.colors(3), asp = 1)
legend(x='bottomright', 
       legend = c("Cond-1", "Cond-2", "Cond-3"),
       fill = (heat.colors(n=3)), bty="n")

## 5.9.3 Other Raster Calculations

## you can apply any mathematical function to rasters
Raster_Result <- sin(r3) + sqrt(r1)
image(Raster_Result)

data(meuse)
coordinates(meuse) <- ~x+y
## select a point layer
soil.1 <- meuse[meuse$soil == 1,]
## create empty raster layer
r <- raster(meuse.grid)
dist <- distanceFromPoints(r, soil.1)
plot(dist)
plot(soil.1, add = T)

## Chapter 6 - POINT PATTERN ANAYSIS USING R

## Point patterns are collections on geo points assumed to have been generated 
## by a random process. KDEs (Kernel Density Estimates) operate by averaging
## a series of small 'bumps' (probability distributions in two dimensions)
## centered on each observed point. Steps to produce a KDE map: (1) compute
## the KDE, (2) plot the KDE, (3) clip the plot to the study area.

require(GISTools)
data(newhaven)
breach.dens <- kde.points(breach, lims = tracts) ## compute density
level.plot(breach.dens) ## create level plot
masker <- poly.outer(breach.dens, tracts, extend = 100) ## use masking to clip
add.masking(masker)
plot(tracts, add = T)


## R Kernel Density Comparison
require(GISTools)
data(newhaven)
par(mfrow=c(1,2), mar=c(0,0,2,0))
brf.dens <- kde.points(burgres.f, lims = tracts)
level.plot(brf.dens)
masker <- poly.outer(brf.dens, tracts, extend=100)
add.masking(masker)
plot(tracts, add = T)

brn.dens <- kde.points(burgres.n, lims = tracts)
level.plot(brn.dens)
masker <- poly.outer(brn.dens, tracts, extend=100)
add.masking(masker)
plot(tracts, add = T)


## 6.4.1 Hexagonal Binning Using R
library("fMultivar")
hbins <- hexBinning(coordinates(breach))
head(hbins$x)
head(hbins$y)
head(hbins$z)
u <- c(1,0,-1,-1,0,1)
u <- u * min(diff(unique(sort(hbins$x))))
v <- c(1,2,1,-1,-2,-1)
v <- v * min(diff(unique(sort(hbins$y))))/3
par(mfrow=c(1,1))
plot(blocks)
shades <- brewer.pal(9, "Greens")
for (i in 1:length(hbins$x)) {
  polygon(u + hbins$x[i], v + hbins$y[i], col = shades[hbins$z[i]], border = NA)
}
plot(blocks, add = T)

## 6.5 SECOND-ORDER ANALYSIS OF POINT PATTERNS
## 6.5.1 Using the K-function in R
library("spatstat")
data(bramblecanes)
plot(bramblecanes)

kf <- Kest(bramblecanes, correction='border')
plot(kf)

## simulation or envelope analysis
kf.env <- envelope(bramblecanes, Kest, correction='border')
plot(kf.env)

## significance testing
mad.test(bramblecanes, Kest)
dclf.test(bramblecanes, Kest)


## 6.5.2 The L-function
## identify clustering in spatial processes
lf.env <- envelope(bramblecanes, Lest, correction = 'border')
plot(lf.env)
mad.test(bramblecanes, Lest)


## 6.5.3 The G-function
gf.env <- envelope(bramblecanes, Gest, correction='border')
plot(gf.env)

## 6.6 LOOKING AT MARKED POINT PATTERNS
## 6.6.1 Cross-L-function Analysis in R

class(marks(bramblecanes))
ck.bramble <- Lcross(bramblecanes, i = 0, j = 1, correction = 'border')
plot(ck.bramble)
ckenv.bramble <- envelope(bramblecanes, Lcross, i = 0, j = 1, correction = 'border')
plot(ckenv.bramble)
dclf.test(bramblecanes, Lcross, i = 0, j = 1, correction = 'border')

## 6.7 INTERPOLATION OF POINT PATTERNS WITH CONTINUOUS ATTRIBUTES
