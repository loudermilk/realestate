## applied-spatial-analysis-part2.R

library("spatstat")
data("japanesepines")
summary(japanesepines)
library(maptools)
spjpines <- as(japanesepines, "SpatialPoints")
summary(spjpines)
spjpines1 <- elide(spjpines, scale = TRUE, unitsq = TRUE)
summary(spjpines1)

pppjap <- as(spjpines, "ppp")
summary(pppjap)

library(rgdal)
spasthma <- readOGR("ch7/.","spasthma")
class(spasthma)
spbdry <- readOGR("ch7/.","spbdry")
spsrc <- readOGR("ch7/.","spsrc")
sproads <- readOGR("ch7/.","sproads")

## when studying a point process, the most basic test that cen be performed
## is that of Complete Spatial Randomness (CSR) - events are distributed
## independently at random and uniformly over the study area. This implies
## that there are no regions where the events are more/less likely to occur
## and that the presence of a given event does not modify the probability of
## an event happening elsewhere.







