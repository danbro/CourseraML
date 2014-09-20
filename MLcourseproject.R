# Course Project in Practical Machine Learning
# by Daniel A. Brodén, 2014
# Coursera Data Analysis Specialization track

# Reading test and training sets
testing <- read.csv("pml-testing.csv")
training <- read.csv("pml-training.csv")

# Keeping data of interest
training <- training[,c(-1,-3,-4,-5,-6,-7)]
threshold <- 0.5*dim(training)[1]
training <- training[, colSums(is.na(training)) < threshold]
training <- training[, colSums(training == "") < threshold]

# Prediction Model
library(tree); library(randomForest)
set.seed(1)
modFit1 <- tree(as.factor(classe)~., data=training)
modFit2 <- randomForest(as.factor(classe)~., data=training, ntree=50)

# Evaluating prediction model on test set
answers <- vector(mode="character",length=dim(testing)[1])
for(i in 1:dim(testing)[1]) {
        answers[i] <- as.character(predict(modFit2, testing[i,]))
}

# Printing output for submission
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(answers)


