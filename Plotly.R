## Need a plotly login (Experied)

library(plotly)
library(ggplot2)
py <- plotly()

trace0 <- list(
  x = c(1, 2, 3, 4),
  y = c(10, 15, 13, 17)
)
trace1 <- list(
  x = c(1, 2, 3, 4),
  y = c(16, 5, 11, 9)
)
response <- py$plotly(trace0, trace1, kwargs=list(filename="basic-line", fileopt="overwrite"))

response$url

#### ggplot Synax
## plotly login experied
ggiris <- qplot(PetalWidth, SepalLength, data = iris, color = Species)
response <- py$ggplotly(ggiris)
