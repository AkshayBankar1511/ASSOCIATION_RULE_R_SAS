#Reading the data.
data <- read.csv("Cosmetics.csv", header = T, colClasses = "factor")
str(data)

#Inspecting the data
names(data)
head(data)
tail(data)
summary(data)
str(data)

dim(data)

#Ploting with barplot
yes <- colSums(data=="Yes")
yes
no <- colSums(data=="No")
no
purchased <- rbind(yes,no)
purchased

barplot(purchased,legend=rownames(purchased)) #plot1
barplot(purchased, beside = T, legend=rownames(purchased)) #plot2

#Arules package
library(arules)
rules <- apriori(data)
summary(rules)


#Specifying the parameter
rules <- apriori(data,parameter = list(minlen=2, maxlen=3,supp=.7)) 
inspect(rules)

#Finding interesting rules-1
rules <- apriori(data,parameter = list(minlen=2, maxlen=3, conf=0.7),
                 appearance = list(rhs=c("Foundation=Yes"), default="lhs"))
inspect(rules)
summary(rules)

#Visualizing rules
library(arulesViz)
plot(rules)
plot(rules, method="grouped")
plot(rules, method = "graph", control = list(type="items"))
plot(rules@quality)

#Finding interesting rules-2
rules <- apriori(data,parameter = list(minlen=2, maxlen=5,supp=0.1,conf=0.5),
                 appearance=list(rhs=c("Foundation=Yes"),lhs=c("Bag=Yes", "Blush=Yes",
                 "Nail.Polish=Yes", "Brushes=Yes", "Concealer=Yes","Eyebrow.Pencils=Yes",
                 "Bronzer=Yes", "Lip.liner=Yes", "Mascara=Yes", "Eye.shadow=Yes","Lip.Gloss=Yes",
                 "Lipstick=Yes","Eyeliner=Yes"), default="none"))
quality(rules)<-round(quality(rules),digits = 3)
inspect(rules)

#Redundant rules
subset.matrix<-is.subset(rules,rules)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1
which(redundant)

#Removing the rules
rules2<-rules[!redundant]
rules2<-sort(rules2, by="lift")
inspect(rules2)


#Exploring it with Ruleexpo function
rules_ex <- apriori(data, parameter = list(minlen=2,maxlen=4,conf=0.75))
ruleExplorer(rules_ex)

