
setwd("C:/Users/Zouzi Qi/Desktop/Capstone Team 1")

# Version 2 is for the different fill data method used the clean dataset. 
rm(list=ls())
# Read in all dataset
library(tidyverse)
library(randomForest)
library(tidyr)
library(ModelMetrics)
library(caret)
rm(list=ls())
library('readxl')
df <- read_excel('All_Data.xlsx')
cols <- colnames(df)

# split variables into different categories
split.vars <- function(cols){
  deis <- c()
  Gs <- c()
  backgrounds <- c()
  for (col in cols){
    if (startsWith(col, 'dei_')){
      deis <- c(deis, col)
    }
    else if (startsWith(col, 'G_')){
      Gs <- c(Gs, col)
    }
    else if (col != 'gpa')
    {
      backgrounds <- c(backgrounds, col)
    }
  }
  return (list(
    deis, Gs, backgrounds
  ))
}


# Dealing with NA


for (col in cols){
  print(paste(col, sum(is.na(df[, col])) / dim(df)[1]))
}

# pick variables whose proportion of NAs is not too high, here set the threshold to be 0.2
data <- df[!is.na(df[, 'gpa']), ]
data <- data[data$gpa != 'I donâ\u0080\u0099t know', ]
thr <- .2
cols.NA <- c()
for (col in cols){
  if (sum(is.na(data[, col])) / dim(data)[1] <= thr){
    cols.NA <- c(cols.NA, col)
  }
}

cols.cate <- split.vars(cols.NA)
deis <- setdiff(
  cols.cate[[1]],
  c(
    'dei_76',
    'dei_80',
    'dei_81',
    'dei_82',
    'dei_83',
    'dei_84'
  )
)
Gs <- setdiff(
  cols.cate[[2]], 
  c("G_School Climate Overall" , "G_Classroom Teaching Supporting Diversity, Equity, and Inclusion", "G_Outcomes")
)
backgrounds <- setdiff(
  cols.cate[[3]],
  "...80"
)




# For the NA in the remaining non-numeric variables, denote it by a new category.
# For the NA in the remaining numeric variables, fill them with the mean value of that variable
for (col in setdiff(
  cols.NA,
  c("G_School Climate Overall" , "G_Classroom Teaching Supporting Diversity, Equity, and Inclusion", "G_Outcomes", 'dei_76', 'dei_80','dei_81', 'dei_82', 'dei_83', 'dei_84')
)){
  if (class(unlist(data[col])) == 'numeric'){
    data[is.na(data[, col]), col] <- mean(
      unlist(data[!is.na(data[, col]), col]), na.rm = TRUE
    )
  }
  else {
    data[is.na(data[, col]), col] <- 'Not answered'
  }
}

# check again
for (col in cols.NA){
  print(paste(col, sum(is.na(data[, col])) / dim(df)[1]))
}


# split dataset


deis.df <- data[c(deis, 'gpa')]
Gs.df <- data[c(Gs, backgrounds)]
backgrounds.df <- data[c(backgrounds)]

# define a function to handle blank
handle.blank <- function(char){
  return (paste(strsplit(char, split=' ')[[1]], collapse=''))
}

# one hot encoding
library('nnet')
for (col in colnames(Gs.df)){
  if (class(unlist(Gs.df[col])) != 'numeric'){
    x <- class.ind(unlist(Gs.df[col]))
    x <- x[, -1]
    names <- c()
    temp.names <- colnames(x)
    for (name in temp.names){
      names <- c(names, handle.blank(paste(col, name, sep='')))
    }
    colnames(x) <- names
    Gs.df <- cbind(
      Gs.df[, -which(colnames(Gs.df) == col)], x
    )
  }
}

# handle blank in the columns name
names <- c()
temp.names <- colnames(Gs.df)
for (name in temp.names){
  names <- c(names, 
             gsub(
               '-', '',
               gsub('\'', '', gsub(',', '', handle.blank(name)))
             )
  )
}
colnames(Gs.df) <- names
Gs.df <- cbind(Gs.df, data['gpa'])

# Background df
for (col in colnames(backgrounds.df)){
  if ((col != 'gpa') & (class(unlist(backgrounds.df[col])) != 'numeric')){
    x <- class.ind(unlist(backgrounds.df[col]))
    x <- x[, -1]
    names <- c()
    temp.names <- colnames(x)
    for (name in temp.names){
      names <- c(names, handle.blank(paste(col, name, sep='')))
    }
    colnames(x) <- names
    backgrounds.df <- cbind(
      backgrounds.df[, -which(colnames(backgrounds.df) == col)], x
    )
  }
}
backgrounds.df <- cbind(backgrounds.df, data['gpa'])

# handle blank in the columns name
names <- c()
temp.names <- colnames(backgrounds.df)
for (name in temp.names){
  names <- c(names, 
             gsub(
               '-', '',
               gsub('\'', '', gsub(',', '', handle.blank(name)))
             )
  )
}
colnames(backgrounds.df) <- names

# background part:
#split training and testing

trainIndex <- createDataPartition(backgrounds.df$gpa,  
                                  p = .8,   
                                  list = FALSE,
                                  times = 1)
back_cateTraining <- backgrounds.df[trainIndex, ]  #training 
back_cateTesting <- backgrounds.df[-trainIndex, ]  #testing
back_cateTraining$gpa <- as.factor(back_cateTraining$gpa)
rf.back <- randomForest(
  gpa~.,
  data=back_cateTraining,
  ntree =500,
  mtry=3,
  importance=TRUE,
  proximity=TRUE
)
pred.back <- predict(
  rf.back,
  back_cateTesting[, 1:51]
)
acc.back <- mean(
  pred.back == back_cateTesting$gpa
)
print(acc.back)
#  Accuracy: 0.3759791



# dei part:
#split training and testing

trainIndex <- createDataPartition(deis.df$gpa,  
                                  p = .8,   
                                  list = FALSE,
                                  times = 1)
dei_cateTraining <- deis.df[trainIndex, ]  #training 
dei_cateTesting <- deis.df[-trainIndex, ]  #testing
dei_cateTraining$gpa <- as.factor(dei_cateTraining$gpa)
rf.dei <- randomForest(
  gpa~.,
  data=dei_cateTraining,
  ntree =500,
  mtry=3,
  importance=TRUE,
  proximity=TRUE
)
pred.dei <- predict(
  rf.dei,
  dei_cateTesting[, 1:29]
)
acc.dei <- mean(
  pred.dei == dei_cateTesting$gpa
)
print(acc.dei)
#Accuracy: 0.310705


# Gs part:
#split training and testing

trainIndex <- createDataPartition(Gs.df$gpa,  
                                  p = .8,   
                                  list = FALSE,
                                  times = 1)
gs_cateTraining <- Gs.df[trainIndex, ]  #training 
gs_cateTesting <- Gs.df[-trainIndex, ]  #testing
gs_cateTraining$gpa <- as.factor(dei_cateTraining$gpa)
rf.gs <- randomForest(
  gpa~.,
  data=gs_cateTraining,
  ntree =500,
  mtry=3,
  importance=TRUE,
  proximity=TRUE
)
pred.gs <- predict(
  rf.gs,
  gs_cateTesting[, 1:56]
)
acc.gs <- mean(
  pred.gs == gs_cateTesting$gpa
)
print(acc.gs)
#Accuracy: 0.3524804


# Model 1
varImpPlot(rf.back, main = "All_variable importance")

# Model 2
varImpPlot(rf.dei, main = "Dei_variable importance")

# Model 3
varImpPlot(rf.gs, main = "G_variable importance")

# This are not good for the Model Version 1 Result










#### Fulling Train without testing set:
#####


# Model fit

## Model 1: Random Forest


mse <- function(pred, y){
  return (mean((pred - y) ^ 2))
}

library('randomForest')
# Model 1: background
backgrounds.df$gpa <- as.factor(backgrounds.df$gpa)
rf.back <- randomForest(
  gpa~.,
  data=backgrounds.df,
  ntree =500,
  mtry=3,
  importance=TRUE,
  proximity=TRUE
)
pred.back <- predict(
  rf.back,
  backgrounds.df[, 1:51]
)
acc.back <- mean(
  pred.back == backgrounds.df$gpa
)
print(acc.back)
mse.back <- mse(
  as.numeric(pred.back),
  as.numeric(backgrounds.df$gpa)
)
print(mse.back)

# Model 2: deis
deis.df$gpa <- as.factor(deis.df$gpa)
rf.dei <- randomForest(
  gpa~.,
  data=deis.df,
  ntree =500,
  mtry=3,
  importance=TRUE,
  proximity=TRUE
)
pred.dei <- predict(
  rf.dei,
  deis.df[, 1:29]
)
acc.dei <- mean(
  pred.dei == deis.df$gpa
)
print(acc.dei)
mse.dei <- mse(
  as.numeric(pred.dei),
  as.numeric(deis.df$gpa)
)
print(mse.dei)

# Model 3: Gs
Gs.df$gpa <- as.factor(Gs.df$gpa)
rf.gs <- randomForest(
  gpa~.,
  data=Gs.df,
  ntree =500,
  mtry=3,
  importance=TRUE,
  proximity=TRUE
)
pred.gs <- predict(
  rf.gs,
  Gs.df[, 1:56]
)
acc.gs <- mean(
  pred.gs == Gs.df$gpa
)
print(acc.gs)
mse.gs <- mse(
  as.numeric(pred.gs),
  as.numeric(Gs.df$gpa)
)
print(mse.gs)



## Importance plot 


# Model 1
varImpPlot(rf.back, main = "All_variable importance")

# Model 2
varImpPlot(rf.dei, main = "Dei_variable importance")

# Model 3
varImpPlot(rf.gs, main = "G_variable importance")


# What are some current blindspots school and district admin should be focusing on to remedy current challenges?
  
# According to the plot of variable importance of the accpetable model, variables have the highest importance in determing gpa include dei_32, dei_73, dei_03, dei_21, dei_12, dei_13, dei_17. Those are the current blindspots school and district admin should be focusing on to remedy current challenges.



## Model 2: KNN


library(caret)
# Model 1
knn.background <- train(gpa~.,
                        data=backgrounds.df,
                        method = 'knn',
                        preProcess = c('center','scale'),
                        tuneLength = 5)
pred.back <- predict(
  knn.background,
  backgrounds.df[, 1:51]
)
acc.back <- mean(
  pred.back == backgrounds.df$gpa
)
print(acc.back)
mse.back <- mse(
  as.numeric(pred.back),
  as.numeric(backgrounds.df$gpa)
)
print(mse.back)

# Model 2
knn.deis <- train(gpa~.,
                  data=deis.df,
                  method = 'knn',
                  preProcess = c('center','scale'),
                  tuneLength = 5)
pred.deis <- predict(
  knn.deis,
  deis.df[, 1:29]
)
acc.deis <- mean(
  pred.deis == deis.df$gpa
)
print(acc.deis)
mse.deis <- mse(
  as.numeric(pred.deis),
  as.numeric(deis.df$gpa)
)
print(mse.deis)

# Model 3
knn.gs <- train(gpa~.,
                data=Gs.df,
                method = 'knn',
                preProcess = c('center','scale'),
                tuneLength = 5)
pred.gs <- predict(
  knn.gs,
  Gs.df[, 1:56]
)
acc.gs <- mean(
  pred.gs == Gs.df$gpa
)
print(acc.gs)
mse.gs <- mse(
  as.numeric(pred.gs),
  as.numeric(Gs.df$gpa)
)
print(mse.gs)


# The performance of KNn is not as desirable as random forest.