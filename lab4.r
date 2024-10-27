library(readr)
library(class)
library(gmodels)
library(dplyr)
bcd <- read.csv('C:/Users/oleti/Downloads/WDBC-data.csv')
str(bcd)
bcd <- select(bcd,-id,-X)
table(bcd$diagnosis)
round(prop.table(table(bcd$diagnosis)) * 100, digits = 1)
## Missing values
sum(is.na(bcd))
head(bcd)
cor(select(bcd,-diagnosis))
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
new_bcd <- as.data.frame(lapply(select(bcd,-diagnosis), normalize))
summary(select(new_bcd,radius_mean,smoothness_mean))
bcd_train <- new_bcd[1:429,]
bcd_test <- new_bcd[430:569,]
bcd_train_labels <- bcd[1:429, 1]
bcd_test_labels <- bcd[430:569, 1]
#Train Model
bcd_test_pred <- knn(train = bcd_train, test = bcd_test, cl = bcd_train_labels,k)
                     #Model Performance
                     cm = CrossTable(x = bcd_test_labels, y = bcd_test_pred, prop.chisq = FALSE)
                     cm
                     
                     