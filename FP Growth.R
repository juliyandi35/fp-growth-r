if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:sentiment, unload=TRUE)
  detach(package:tm, unload=TRUE)
}

library(plyr)
library(arules)
library(arulesViz)
library(Matrix)
library(base)

travel <- readxl:: read_excel('Data Kunjungan.xlsx')
class(travel)

str(travel)
travel$Date <- as.Date(travel$Date)
head(travel)

sum(is.na(travel))

sorted <- travel[order(travel$ID),]

sorted$ID <- as.numeric(sorted$ID)
str(sorted)

DestinationList <- ddply(sorted, c("ID","Date"), function(df1)paste(df1$Destination,collapse = ","))

head(DestinationList,6)

DestinationList$ID <- NULL
DestinationList$Date <- NULL
colnames(DestinationList) <- c("DestinationList")

write.csv(DestinationList,"DestinationList.csv", quote = FALSE, row.names = TRUE)
head(DestinationList)

visit = read.transactions(file="DestinationList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
print(visit)

visit@itemInfo$labels <- gsub("\"","",visit@itemInfo$labels)

basket_rules <- apriori(visit, parameter = list(minlen=2, sup = 0.001, conf = 0.05, target="rules"))

print(length(basket_rules))

summary(basket_rules)
inspect(basket_rules[1:20])
inspect(basket_rules[1:5])

plot(sort(basket_rules,by="lift"),method="graph",control=list(type="items"))

plot(basket_rules, jitter = 0)

plot(basket_rules, method = "grouped", control = list(k = 5))

plot(basket_rules, method = "grouped", control = list(k = 10))

plot(basket_rules[1:10], method="graph")

plot(basket_rules[1:20], method="graph")

plot(basket_rules[1:50], method="graph")

plot(basket_rules[1:10], method="paracoord")

plot(basket_rules[2500:2510], method="paracoord")

itemFrequencyPlot(visit, topN = 19)

basket_rules2 <- apriori(visit, parameter = list(minlen=3, sup = 0.001, conf = 0.1, target="rules"))
print(length(basket_rules2))
summary(basket_rules2)

plot(basket_rules2, method="graph")

plot(basket_rules2[1:10], method="paracoord")
