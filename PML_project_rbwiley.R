## Machine Learning Project code
#coder\analyst: RB Wiley

#load necessary packages
library("caret", lib.loc="~/R/win-library/3.2")
library("e1071", lib.loc="~/R/win-library/3.2")
library("randomForest", lib.loc="~/R/win-library/3.2")
library("plyr", lib.loc="~/R/win-library/3.2")
library("ipred", lib.loc="~/R/win-library/3.2")

#load data
training <- read.csv( "./data/pml-training.csv")
testing <- read.csv("./data/pml-testing.csv")


##EDA
trainingsum = summary(training)
trainingsum
str(training, list.len = 160)

# drop NAs from Training set
TrainingNoNA <- training
TrainingNoNA[ training == '' | training == 'NA'] <- NA
NoNAList <- which(colSums(is.na(TrainingNoNA))!=0)
TrainingNoBlank <- TrainingNoNA[, -NoNAList]
TrainingCln <- TrainingNoBlank[,-(1:7)]      #drop 1st 7 attribute\timestamp columns

# drop NAs from Testinging set
TestingNoNA <- testing
TestingNoNA[ testing == '' | testing == 'NA'] <- NA
TestNoNAList <- which(colSums(is.na(TestingNoNA))!=0)
TestingNoBlank <- TestingNoNA[, -TestNoNAList]
TestingCln <- TestingNoBlank[,-(1:7)]      #drop 1st 7 attribute\timestamp columns


#partition training into training and validation sets
#   kfold or random: random
inTrain <- createDataPartition(y=TrainingCln$classe, p=0.75, list=FALSE)
validation1 <- TrainingCln[-inTrain,]
training1 <- TrainingCln[inTrain,]
dim(validation1) ; dim(training1)

##training1 is the Training Set ;      validation1 is the Validation Set
##
#view sample of x,y plots

pairs(training1[,1:10])
#plot(training1[,1:10])
pairs(training1[,21:30])
pairs(training1[,41:50])

# set overall seed
set.seed(10001)

#preProcess the data
preObj <- preProcess(training1[,-53], method=c("center", "scale"))



#feature determination & cross validation


#apply training controls
TrainControl <- trainControl(method = "cv", number = 5, returnResamp = "all")


#Sample error plots




#train() - method = "glm", "rf", "treebag", 

##norun GLM
#ModelfitGLM <- train(classe ~ ., data=training1,  method="glm")
#ModelfitGLM$finalModel
#ResultGLM <- predict(ModelfitGLM, newdata = validation1)
#confusionMatrix(ResultGLM, validation1$classe)

##Good result w/ RF: use this model
ModelfitRF <- train(classe ~ ., data=training1,  method="rf", tuneLength = 1, ntree = 25)
ModelfitRF$finalModel
ResultRF <- predict(ModelfitRF, newdata = validation1)
confusionMatrix(ResultRF, validation1$classe)

###Test RF model
Resulttest <- predict(ModelfitRF, newdata = TestingCln)
Resulttest

#don't bother
ModelTB <- train(classe ~ ., method = "treebag", data = training1, trControl = TrainControl)
ModelfitTB$finalModel
ResultTB <- predict(ModelfitTB, newdata = validation1)
confusionMatrix(ResultTB, validation1$classe)

#prediction & cross validation


#
