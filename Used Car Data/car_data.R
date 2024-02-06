cr_data<-read.csv('Desktop/Projects/R/Car Data/DealerShipUsedCars.csv', stringsAsFactors = FALSE)
write.csv(cr_data, file = "cr_data.csv", row.names = FALSE)