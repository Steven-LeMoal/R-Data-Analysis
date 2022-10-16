#Data Preprocessing in R

#1
install.packages("caTools")

#2
dataset = read.csv("/Users/steven/Documents/ESILV A4/Machine Learning/TD1/dataset.csv")

#3
View(dataset)

#4.1
dataset$Age[is.na(dataset$Age)] <- mean(dataset$Age, na.rm = TRUE)
#dataset$Age=ifelse(is.na(dataset$Age),ave(dataset$Age, FUN= function(x)mean(x, na.rm=TRUE)),dataset$Age)

#4.2
dataset$Salary[is.na(dataset$Salary)] <- mean(dataset$Salary, na.rm = TRUE)
#dataset$Salary=ifelse(is.na(dataset$Salary),ave(dataset$Salary, FUN= function(x)mean(x, na.rm=TRUE)),dataset$Salary)

#5
dataset$Country = factor(dataset$Country, levels = c('France','Spain','Germany'), labels = c(1, 2 , 3))
#dataset$Country <- unclass(dataset$Country)
#dataset[sapply(dataset, is.factor)] <- data.matrix(dataset[sapply(dataset, is.factor)])

#6.1
dataset$Purchased = factor(dataset$Purchased,levels = c('No', 'Yes'),labels = c(0, 1))
dataset$Purchased[is.na(dataset$Purchased)] <- 0
as.factor(dataset$Purchased)
#dataset$Purchased <- unclass(dataset$Purchased)

#6.2
#set.seed permet de retrouver les valeurs sur une autre machine
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training_data = subset(dataset, split == TRUE)
test_data = subset(dataset, split == FALSE)

View(training_data)
View(test_data)

#7
#On selectionne les colonnes 2 et 3
training_data[, 2:3] = scale(training_data[, 2:3])
test_data[, 2:3] = scale(test_data[, 2:3])

View(training_data)
View(test_data)

#First Machine Learning Project in R Step-By-Step

#1
dataset2 = read.csv("/Users/steven/Documents/ESILV A4/Machine Learning/TD1/iris.data", stringsAsFactors=T)
View(dataset2)

#2
#install.packages("caret")
library("caret")
set.seed(150)
trainIndex <- createDataPartition(dataset2$class, p = .8, list = FALSE, times = 1)
head(trainIndex)

test_set <- dataset2[-trainIndex,]
dataset2 <- dataset2[trainIndex,]

View(dataset2)
View(test_set)

#3
dim(dataset2)
sapply(dataset2, class)
head(dataset2)
levels(dataset2$class)

#4
summary(dataset2)
prop.table(table(dataset2$class))

#5
x <- dataset2[,1:4]
y <- dataset2[,5]
#5.1
par(mfrow=c(1,4))
for(i in 1:4) {boxplot(x[,i], main=names(iris)[i])}
#5.2
plot(y)

#6
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#7.1
featurePlot(x=x, y=y, plot="ellipse")
#7.2
featurePlot(x=x, y=y, plot="box")

#8
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#9
#KNN
set.seed(7)
fit.knn <- train(class~., data=dataset2, method="knn", metric=metric,trControl=control)
#SVM
set.seed(7)
fit.svm <- train(class~., data=dataset2, method="svmRadial",metric=metric, trControl=control)
#Random Forest
set.seed(7)
fit.rf <- train(class~., data=dataset2, method="rf", metric=metric,trControl=control)

#10
results <- resamples(list(knn=fit.knn,svm=fit.svm, rf=fit.rf))
summary(results)

#11
dotplot(results)
#on peut voir que le meilleur modèle est knn
print(fit.knn)

#12
predictions <- predict(fit.knn, test_set)
confusionMatrix(predictions, test_set$class)




