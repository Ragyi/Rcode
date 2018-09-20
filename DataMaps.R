library(devtools)
install.packages("datamap")
library(datamap)
install.packages("Quandl")
library(Quandl)
install_github('markmarkoh/datamaps')

vcData = Quandl("FBI_UCR/USCRIME_TYPE_VIOLENTCRIMERATE")
kable(head(vcData[,1:9]), format = 'html', table.attr = "class=nofluid")


