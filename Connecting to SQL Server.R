#Connecting to SQL Server

install.packages("RODBC")
library(devtools)
library(downloader)
#Install it rhoguh Homebrew (terminal)
library(RODBC)
channel <- odbcConnect("54.252.211.210", uid="winston.juay", pwd="dog stopped relationship distant");

p <- sqlQuery(channel, "
SELECT * FROM myTable
");
close(channel);