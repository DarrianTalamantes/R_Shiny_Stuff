####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Importing libraries
library(RCurl) # for downloading the iris CSV file
library(randomForest)
library(caret)

# Importing data
flowerData <- iris

# Performs stratified random split of the data set using the caret package
TrainingIndex <- createDataPartition(flowerData$Species, p=0.8, list = FALSE)
TrainingSet <- flowerData[TrainingIndex,] # Training Set, 80% of origional set
TestingSet <- flowerData[-TrainingIndex,] # Test Set, 20% of origional set

# writing data into csv files to stop possible shuffling of data and aid reproducibility 
write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

# Building Random forest model
model <- randomForest(as.factor(Species) ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)

# Save model to RDS file, allows us to call the model very quickly and not have to rebuild it everytime
saveRDS(model, "model.rds")
