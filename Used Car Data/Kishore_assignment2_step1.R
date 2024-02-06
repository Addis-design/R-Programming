usedcars<-read.csv('Desktop/Projects/R/Car Data/DealerShipUsedCars.csv', stringsAsFactors = FALSE)
write.csv(usedcars, file = "usedcars.csv", row.names = FALSE)
