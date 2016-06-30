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

