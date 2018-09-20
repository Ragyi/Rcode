nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")

View(nba)

nba <- nba[order(nba$PTS),]

row.names(nba) <- nba$Name

nba <- nba[,2:20]

nba_matrix <- data.matrix(nba)

d3heatmap(nba_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
d3heatmap(nba_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))

