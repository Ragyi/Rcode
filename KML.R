#install.packages("sp")
library(sp)
install.packages("plotKML", dependencies = TRUE)
library(plotKML)
install.packages("rgdal")
library(rgdal)
paths()

get("ref_CRS", envir = plotKML.opts)

data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
eberg <- eberg[runif(nrow(eberg))<.2,]
bubble(eberg["CLYMHT_A"])
plotKML(eberg["CLYMHT_A"])

plotKML(eberg["CLYMHT_A"], colour_scale=rep("#FFFF00", 2), points_names="")

