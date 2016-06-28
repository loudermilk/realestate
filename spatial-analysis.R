## R for Spatial Analysis and Mapping
## Exercises from book

##
## Ch. 2
##
library("GISTools")
library("RColorBrewer")
library("OpenStreetMap")
library("RgoogleMaps")
library("PBSmapping")


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
