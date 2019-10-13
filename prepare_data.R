library(dplyr)

crime <- read.csv("D:\\python\\crime\\crimes-in-boston\\crime.csv")
grouped_data <- aggregate(list(COUNT=crime$INCIDENT_NUMBER), by=list(DISTRICT=crime$DISTRICT, YEAR=crime$YEAR), FUN=length);
library(tidyr)
final <- spread(grouped_data, DISTRICT, COUNT)

finaldf <- final[-1]
row.names(finaldf) <- final$YEAR
View(finaldf)

