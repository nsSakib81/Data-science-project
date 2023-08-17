dataset<-read.csv("D:/bmi.csv",header= TRUE,sep=",")
dataset


str(dataset)
summary(dataset)

sum(is.na(dataset))

dataset$Gender<-factor(dataset$Gender,levels = c ("Male","Female"),labels = c(1,2))
dataset



numerical_bmi_cols <- c("Height", "Weight", "Index")
bmidata <- dataset[numerical_bmi_cols]


normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


bmidata_n <- as.data.frame(lapply(bmidata, normalize))
colnames(bmidata_n) <- paste(colnames(bmidata_n), "_normalized", sep = "")


head(bmidata_n)



set.seed(100)

bmidata.d <- sample(1:nrow(bmidata_n), size = nrow(bmidata_n) * 0.8, replace = FALSE)
head(bmidata.d)


train.risk<-dataset[bmidata.d,]
head(train.risk)
test.risk<- dataset[-bmidata.d,]
head(test.risk)


train.bmiStage<-dataset[bmidata.d,4]
test.bmiStage<-dataset[-bmidata.d,4]

NROW(train.bmiStage)
NROW(test.bmiStage)


install.packages("class")
library(class)



knn.50<- knn(train=train.risk, test=test.risk, cl=train.bmiStage, k=50)

ACC.50 <- 100 * sum(test.bmiStage == knn.50)/NROW(test.bmiStage)
ACC.50



table(knn.50,test.bmiStage)
knn.50

install.packages("lattice")
library(lattice)
install.packages("caret")
library(caret)
install.packages("stringi")
library(stringi)
install.packages("ggplot2")
library(ggplot2)


finalResult<- confusionMatrix(table(knn.50,test.bmiStage))
print(finalResult)

























