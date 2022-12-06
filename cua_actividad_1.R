rm(list=ls())
##################Tema 1#############################################

requiredPackages <- c("arsenal", "car", "corrplot", "fdth","gapminder","dplyr","DescTools", "foreign", "e1071", "expss", "GGally", "ggplot2", "haven", 
                      "knitr","plotly", "remotes", "summarytools","ggridges","table1", "tableone", "tidyverse", "SmartEDA")

sesion1 <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

sesion1(requiredPackages)
######################################################
##LOAD DATA
dataNum <- read.csv('./data/HackerRank-Developer-Survey-2018-Numeric.csv', header = T, stringsAsFactors = F)
dataValues <- read.csv('./data/HackerRank-Developer-Survey-2018-Values.csv', header = T, stringsAsFactors = F)

View(dataValues)
View(dataNum)

head(dataValues)
glimpse(dataValues)
names(dataValues)
str(dataValues)
dim(dataValues)

sapply(dataValues, function(x) sum(is.na(x)))

###### Estudio de variable q3Gender categorica ######################
# Tabla de frecuencias variable categórica q3Gender
table(dataValues$q3Gender)
# Remove nulls from q3Gender
dataNotNullGender <- dataValues[!(is.na(dataValues$q3Gender) | dataValues$q3Gender=="Non-Binary" | dataValues$q3Gender=="#NULL!"), ]
# Tabla de frecuencias variable q3Gender not Non-Binary and not null values
table(dataNotNullGender$q3Gender)

# bar graph q3Gender comparison
ggplot(data=dataNotNullGender, aes(q3Gender, fill=q3Gender)) + 
  geom_bar(aes(y = (..count..))) +
  ggtitle("Women vs men in tech") +
  xlab("Gender") +
  ylab("Total count in 2018") + 
  geom_text(aes(label = (..count..),
               y= (..count..),accuracy = 0.01), stat= "count", vjust = -.5)

###### Estudio de variable q1AgeBeginCoding categorica ######################
# Tabla de frecuencias variable categórica q1AgeBeginCoding
table(dataValues$q1AgeBeginCoding)
# Remove nulls and group by gender from dataNum dataset to order the graph later
dataByageCodingAndGender <- dataValues %>% filter(q1AgeBeginCoding != "#NULL!") %>% filter(q3Gender != "#NULL!" & q3Gender != "Non-Binary") %>% group_by(q1AgeBeginCoding, q3Gender) %>% count()
View(dataByageCodingAndGender)
# accumulative bar chart 
ggplot(dataByageCodingAndGender, aes(x = q1AgeBeginCoding, y = n, fill = q3Gender)) + 
  geom_bar(stat="identity") + 
ylab("Total count in 2018") +
  scale_x_discrete(labels=c("5 to 10", "11 to 15", "16 to 20", "21 to 25", "26 to 30", "31 to 35", "36 to 40", "41 to 50", "> 50")) +
  scale_fill_brewer(palette="Set3", labels = c("Male", "Female"))


 