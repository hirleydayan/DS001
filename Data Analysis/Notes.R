# For listing available datasets:
library(help= "datasets")

con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
n <- c("hour", "temperature", "wind", "humidity", "sensation")
cepagri <- read.table(con, header = FALSE, fill = TRUE, sep = ";", col.names = n)
head(cepagri)
sapply(cepagri, class)
cepagri$hour <- as.POSIXct(cepagri$hour, format="%d/%m/%Y-%H:%M")
