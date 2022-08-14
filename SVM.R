install.packages('caTools')
install.packages('caTools')
install.packages("caret")
install.packages("kernlab")
install.packages("ROCR")
library(e1071)
library(MASS)
library(caTools)
library(caret)
library(kernlab)
library(ROCR)
library(e1071)
library(caTools)
library(kernlab)

#Importing data
df = read.csv(file.choose(), header = T)
head(df)
View(df)
summary(df)

#Checking NA's
any(is.na(df))
colSums(is.na(df)) 


#Imputing mode of each of the variable in place of NA values
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}                                                     #mode function
m1 = getmode(df$healthgroup)
df$healthgroup[is.na(df$healthgroup)] = m1      #healthgroup mode imputation

m2 = getmode(df$agegrp)
df$agegrp[is.na(df$agegrp)] = m2                #Agegroup mode imputation

m3 = getmode(df$race)
df$race[is.na(df$race)] = m3                    #race mode imputation

m4 = getmode(df$employ.ins)
df$employ.ins[is.na(df$employ.ins)] = m4        #employ.ins mode imputation

m5 = getmode(df$insured )
df$insured [is.na(df$insured )] = m5            #insured  mode imputation

m6 = getmode(df$employ)
df$employ [is.na(df$employ )] = m6              #employ  mode imputation

m7 = getmode(df$marital.stat)
df$marital.stat[is.na(df$marital.stat)] = m7    #marital.stat  mode imputation

m8 = getmode(df$postponed.care)
df$postponed.care[is.na(df$postponed.care)] = m8    #postponed.care  mode imputation

m9 = getmode(df$emergency )
df$emergency [is.na(df$emergency )] = m9    #emergency   mode imputation

m10 = getmode(df$specialist)
df$specialist[is.na(df$specialist)] = m10     #specialist    mode imputation

m11 = getmode(df$meds)
df$meds[is.na(df$meds)] = m11            #meds mode imputation

m12 = getmode(df$health)
df$health[is.na(df$health)] = m12            #health mode imputation

m13 = getmode(df$educ)
df$educ[is.na(df$educ)] = m13            #educ mode imputation

#Encoding the target variable using factor()
df$dental.visit = factor(df$dental.visit, levels = c(0, 1))
#############################################################

#data slicing test and train
set.seed(2)
intrain <- createDataPartition(y=df$dental.visit, p=0.7, list=F)

training <- df[intrain,]
testing <- df[-intrain,]

dim(training)
dim(testing)


#Our target variable should be categorical, so factorize them
training["dental.visit"] = factor(training[["dental.visit"]])


#Train our model
#before train your model implement  trainControl method
trctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3) #number is number of iteration, repeat the cross validation

#train model
svm_linear <- train(dental.visit~. , data=training, method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),  
                    tuneLength=10)
#preprocessing parameter help in scaling and centering data
svm_linear

#Predict classes on test set
test_pred <- predict(svm_linear, newdata= testing)   # first paramter is our train model
test_pred 

summary(test_pred)

#Accuracy 
confusionMatrix(test_pred, testing$dental.visit) 

#Building & tuning of an SVM classifier with different values of C
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svmgrid <- train(dental.visit ~., data = training, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = grid,
                 tuneLength = 10)
svmgrid

plot(svmgrid) 


#make predictions using this model for our test set
test_pred <- predict(svmgrid, newdata= testing)
test_pred  

summary(test_pred)  

#Confusion matrix
confusionMatrix(test_pred, testing$dental.visit) 
