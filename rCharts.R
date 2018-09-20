#### Installing Dependencies
#installed.packages("devtools")
require(devtools)
#install.packages("downloader")
library(downloader)
require(devtools)
#install_github('rCharts', 'ramnathv')
library(rCharts)
#install_github(c("slidify", "slidifyLibraries"), "ramnathv", ref = "dev")
library(slidify)
library(slidifyLibraries)
#installed.packages("rJava")
library(rJava)
#devtools::install_github("rstudio/leaflet")
#install.packages("OpenStreetMaps", dependencies = TRUE)
library(OpenStreetMap)
library(ggplot2)
#install_github("rstudio/leaflet")
library(leaflet)
#install.packages("viridis")
library(viridis)
#devtools::install_github("ropensci/plotly")
#library(plotly)
#set_credentials_file("ragyi", " nnpd9wn99v")
#install_github('ramnathv/rMaps')
library(rMaps)
#install.packages("reshape2")
library(reshape2)
#install.packages("rjson")
library(rjson)

### Still require NVD3 and Crosslet packages to complete code

names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

####Polychart

r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = 'point', color = 'gear')
r1

####Time Series

library(ggplot2)
data(economics, package = 'ggplot2')
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = 'date', y = c('psavert', 'uempmed'), type = 'Line',
            data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1

####NVD3

hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = 'multiBarChart')
n1

#### xCharts

uspexp <- melt(USPersonalExpenditure)
names(uspexp)[1:2] = c('category', 'year')
x1 <- xPlot(value ~ year, group = 'category', data = uspexp,type = 'line-dotted')
x1

#### Highcharts

h1 <- Highcharts$new()
h1$chart(type = "spline")
h1$series(data = c(1, 3, 2, 4, 5, 4, 6, 2, 3, 5, NA), dashStyle = "longdash")
h1$series(data = c(NA, 4, 1, 3, 4, 2, 9, 1, 2, 3, 4), dashStyle = "shortdot")
h1$legend(symbolWidth = 80)
h1

#### Leaflet

map3 <- Leaflet$new()
map3$setView(c(51.505, -0.09), zoom = 13)
map3$marker(c(51.5, -0.09), bindPopup = "<p> Store 1 </p>")
map3$marker(c(51.495, -0.083), bindPopup = "<p> Store 2 </p>")
map3

#### Rickshaw

usp = reshape2::melt(USPersonalExpenditure)
p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = 'Var1', data = usp, type = 'area')
p4


#### rMaps

crosslet(
  x = "Australia",
  y = c("web_index", "universal_access", "impact_empowerment", "freedom_openness"),
  data = web_index)
