# For listing available datasets:
library(help= "datasets")

con <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
n <- c("hour", "temperature", "wind", "humidity", "sensation")
cepagri <- read.table(con, header = FALSE, fill = TRUE, sep = ";", col.names = n)
sapply(cepagri, class)
cepagri$hour <- as.POSIXlt(cepagri$hour, format="%d/%m/%Y-%H:%M")
