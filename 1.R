#' ---
#' title: "Heart Disease Classifier"
#' author: "Ayush Arora, Muriel Banze, Shweta Wahane"
#' date: "10/02/2020"
#' ---

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
packages <- c("synthpop", "tidyverse", "dplyr", "ggplot2", "ggthemes", "naniar", "ggpubr", "scorecard", "class", "caTools", "e1071")
ipak(packages)

library('dplyr');
library('tidyverse');
library("synthpop");
library("naniar")
library("ggplot2");
library("ggthemes");
library("ggpubr");
library("ggsci");
library("scorecard");
library("class");
library("caTools");
library("e1071");
library("rpart")
library("rpart.plot")
library("tree")
library("MASS")

clevelandHeart <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)

## Modifying Column Names so that they are easier to read
names(clevelandHeart)[1]  <- "Age"
names(clevelandHeart)[2]  <- "Sex" ## 1 <- Male , 0 <- Female
names(clevelandHeart)[3]  <- "Chest_Pain" ## 1 <- typical angina , 2 <- atypical angina, 
## 3: <- non-anginal pain, 4 <- asymptomatic
names(clevelandHeart)[4]  <- "Resting_Blood_Pressure" ##in mmHg
names(clevelandHeart)[5]  <- "Cholestrol_Level" ## in mgDl
names(clevelandHeart)[6]  <- "Fasting_Sugar" ##fbs > 120 mg/dl 1 <- true, 0 <- false 
names(clevelandHeart)[7]  <- "Resting_ECG" ## 0 <- normal , 1 <- wave abnormality , 
## 2 <- left ventricular hypertrophy
names(clevelandHeart)[8]  <- "Max_Heart_Rate" 
names(clevelandHeart)[9]  <- "Exercise_Induced_Anigna" ## 1 <- yes , 0 <- no
names(clevelandHeart)[10] <- "ST_Depressions" 
names(clevelandHeart)[11] <- "ST_Depressions_Slope" ## 1 <- upsloping, 2 <- flat , 3 <- downsloping
names(clevelandHeart)[12] <- "Major_Vessels_Colored" ## 0-3 <- Number of vessels
names(clevelandHeart)[13] <- "Thallium_Test" ## 3 <- normal , 6 <- fixed , 7 <- reversible 
names(clevelandHeart)[14] <- "Target" ## 0 <- no heart disease , 1 <- heart disease 

##clevelandHeart$Sex <- as.logical(clevelandHeart$Sex)
##clevelandHeart$Exercise_Induced_Anigna <- as.logical(clevelandHeart$Exercise_Induced_Anigna)
clevelandHeart$Major_Vessels_Colored <- as.integer(clevelandHeart$Major_Vessels_Colored)
## Column 13 and 14 need preprocessing
## 13
print(typeof(clevelandHeart$Thallium_Test))
clevelandHeart$Thallium_Test <- as.integer(clevelandHeart$Thallium_Test)
index <- clevelandHeart$Thallium_Test == 3
clevelandHeart$Thallium_Test[index] <- 0 
index <- clevelandHeart$Thallium_Test == 6
clevelandHeart$Thallium_Test[index] <- 1 
index <- clevelandHeart$Thallium_Test == 7
clevelandHeart$Thallium_Test[index] <- 2 

## 14
clevelandHeart$Target <- as.logical(clevelandHeart$Target)
## Printing the Summary of the data set
print(summary(clevelandHeart))
##na_if(clevelandHeart, '?')
print(sum(is.na(clevelandHeart)))
## Removing the NA Values
clevelandHeart <- clevelandHeart %>%
  mutate(Target = ifelse(clevelandHeart$Target == "TRUE", 1, 0))

clevelandHeart <- na.omit(clevelandHeart)
data.seed = 1785012
syndata <- syn(data = clevelandHeart, seed = data.seed, )


syndata_df <- syndata$syn
syndata_df <- syndata_df
sum(is.na(syndata_df))
syndata_df <- na.omit(syndata_df)
##syndata_df %>% replace_with_na(replace = list(x = '?'))
##syndata_df[syndata_df == '?' ] <- 'na'
##syndata_df <- na.omit(syndata_df)
## Check for replicated unique units 
common <- intersect(clevelandHeart$col, syndata_df$col)  
clevelandHeart[common,] # give you common rows in data frame 1  
syndata_df[common,] # give you common rows in data frame 2
mergedData <- bind_rows(clevelandHeart, syndata_df)

## PLOTS FOR AGE VS TARGET
agePlot1 <- ggplot(clevelandHeart,aes(as.factor(Target),Age,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
 scale_fill_tron() +  
  ggtitle( label="Age vs Target", subtitle = "Original Data") +
  labs(y="Age",x="Target",fill="Target") 

agePlot2 <- ggplot(syndata_df,aes(as.factor(Target),Age,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
scale_fill_tron() +  
  ggtitle( label="Age vs Target", subtitle = "Synthetic Data") +
  labs(y="Age",x="Target",fill="Target")

agePlot3 <- ggplot(mergedData,aes(as.factor(Target),Age,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
 scale_fill_tron() +  
  ggtitle( label="Age vs Target", subtitle = "Mixed Data") +
  labs(y="Age",x="Target",fill="Target")

agePlot <- ggarrange(agePlot1, agePlot2, agePlot3, ncol = 2, nrow = 2)
agePlot

## PLOTS FOR SEX VS TARGET
sexPlot <- clevelandHeart %>% group_by(Sex, Target) %>% summarise(Count = n()) %>%
  ungroup()

sexPlot1 <- ggplot(sexPlot,aes(Count,as.factor(Sex),fill=as.factor(Target)))+
  geom_bar(stat= "Identity",alpha=0.5)+
  scale_fill_tron() +  
  ggtitle( label="Sex vs Target", subtitle = "Original Data") +
  labs(y="Sex",x="Count",fill="Target") 

sexPlot <- syndata_df %>% group_by(Sex, Target) %>% summarise(Count = n()) %>%
  ungroup()

sexPlot2 <- ggplot(sexPlot,aes(Count,as.factor(Sex),fill=as.factor(Target)))+
  geom_bar(stat= "Identity",alpha=0.5)+
  scale_fill_tron() +  
  ggtitle( label="Sex vs Target", subtitle = "Synthetic Data") +
  labs(y="Sex",x="Count",fill="Target") 

sexPlot <- mergedData %>% group_by(Sex, Target) %>% summarise(Count = n()) %>%
  ungroup()

sexPlot3 <- ggplot(sexPlot,aes(Count,as.factor(Sex),fill=as.factor(Target)))+
  geom_bar(stat= "Identity",alpha=0.5)+
  scale_fill_tron() +  
  ggtitle( label="Sex vs Target", subtitle = "Merged Data") +
  labs(y="Sex",x="Count",fill="Target") 

sexPlot <- ggarrange(sexPlot1, sexPlot2, sexPlot3, ncol = 2, nrow = 2)
sexPlot

## PLOTS FOR CHESTPAIN VS TARGET
cpPlot <- clevelandHeart %>% group_by(Chest_Pain, Target) %>% summarise(Count = n()) %>%
  ungroup()

cpPlot1 <- ggplot(cpPlot,aes(Count,as.factor(Chest_Pain),fill=as.factor(Target)))+
  geom_bar(stat= "Identity",alpha=0.5)+
  scale_fill_tron() +  
  ggtitle( label="Chest Pain vs Target", subtitle = "Original Data") +
  labs(x="Count",y="Chest Pain",fill="Target") 

cpPlot <- syndata_df %>% group_by(Chest_Pain, Target) %>% summarise(Count = n()) %>%
  ungroup()

cpPlot2 <- ggplot(cpPlot,aes(Count,as.factor(Chest_Pain),fill=as.factor(Target)))+
  geom_bar(stat= "Identity",alpha=0.5)+
  scale_fill_tron() +  
  ggtitle( label="Chest Pain vs Target", subtitle = "Synthetic Data") +
  labs(x="Count",y="Chest Pain",fill="Target") 

cpPlot <- mergedData %>% group_by(Chest_Pain, Target) %>% summarise(Count = n()) %>%
  ungroup()

cpPlot3 <- ggplot(cpPlot,aes(Count,as.factor(Chest_Pain),fill=as.factor(Target)))+
  geom_bar(stat= "Identity",alpha=0.5)+
  scale_fill_tron() +  
  ggtitle( label="Chest Pain vs Target", subtitle = "Mixed Data") +
  labs(x="Count",y="Chest Pain",fill="Target") 

cpPlot <- ggarrange(cpPlot1, cpPlot2, cpPlot3, ncol = 2, nrow = 2)
cpPlot

## PLOTS FOR BP VS TARGET
bpPlot1 <- ggplot(clevelandHeart,aes(Resting_Blood_Pressure, col =as.factor(Target),fill=as.factor(Target)))+
           geom_freqpoly(alpha=0.8, stat="bin")+
           guides(col=F)+
           scale_fill_tron() +  
           ggtitle( label="Blood Pressure vs Target", subtitle = "Original Data") +
           labs(color="Target",x="Resting Blood Pressure")

bpPlot2 <- ggplot(syndata_df,aes(Resting_Blood_Pressure, col =as.factor(Target),fill=as.factor(Target)))+
           geom_freqpoly(alpha=0.8, stat="bin")+
           guides(col=F) +
           scale_fill_tron() +  
           ggtitle( label="Blood Pressure vs Target", subtitle = "Synthetic Data") +
           labs(color="Target",x="Resting Blood Pressure")

bpPlot3 <- ggplot(mergedData,aes(Resting_Blood_Pressure, col =as.factor(Target),fill=as.factor(Target)))+
           geom_freqpoly(alpha=0.8, stat="bin")+
           guides(col=F)+
           scale_fill_tron() +  
           ggtitle( label="Blood Pressure vs Target", subtitle = "Mixed Data") +
           labs(color="Target",x="Resting Blood Pressure")

bpPlot <- ggarrange(bpPlot1, bpPlot2, bpPlot3, ncol = 2, nrow = 2)
bpPlot

## PLOTS FOR CHOLESTROL VS TARGET
cholPlot1 <- ggplot(clevelandHeart,aes(as.factor(Target),Cholestrol_Level,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="Cholestrol vs Target", subtitle = "Original Data") +
  labs(y="Cholestoral",x="Target",fill="Target")


cholPlot2 <- ggplot(syndata_df,aes(as.factor(Target),Cholestrol_Level,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="Cholestrol vs Target", subtitle = "Synthetic Data") +
  labs(y="Cholestoral",x="Target",fill="Target")


cholPlot3 <- ggplot(mergedData,aes(as.factor(Target),Cholestrol_Level,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="Cholestrol vs Target", subtitle = "Merged Data") +
  labs(y="Cholestoral",x="Target",fill="Target")

cholPlot <- ggarrange(cholPlot1, cholPlot2, cholPlot3, ncol = 2, nrow = 2)
cholPlot

## PLOTS FOR SUGAR VS TARGET
sugPlot1 <- ggplot(clevelandHeart,aes(as.factor(Fasting_Sugar),fill=as.factor(Target)))+
  geom_bar(alpha=0.2, stat="count")+
  scale_fill_tron()+
  ggtitle( label="Sugar vs Target", subtitle = "Original Data") +
  labs(y="Count",x="Sugar",fill="Target")

sugPlot2 <- ggplot(syndata_df,aes(as.factor(Fasting_Sugar),fill=as.factor(Target)))+
  geom_bar(alpha=0.2, stat="count")+
  scale_fill_tron()+
  ggtitle( label="Sugar vs Target", subtitle = "Synthetic Data") +
  labs(y="Count",x="Sugar",fill="Target")

sugPlot3 <- ggplot(mergedData,aes(as.factor(Fasting_Sugar),fill=as.factor(Target)))+
  geom_bar(alpha=0.2, stat="count")+
  scale_fill_tron()+
  ggtitle( label="Sugar vs Target", subtitle = "Merged Data") +
  labs(y="Count",x="Sugar",fill="Target")

sugPlot <- ggarrange(sugPlot1, sugPlot2, sugPlot3, ncol = 2, nrow = 2)
sugPlot

## PLOTS FOR ECG VS TARGET
ecgPlot1 <- ggplot(clevelandHeart,aes(y = as.factor(Resting_ECG),fill=as.factor(Target)))+
  geom_bar(alpha=0.2, stat="count")+
  scale_fill_tron()+
  coord_flip() +
  ggtitle( label="ECG vs Target", subtitle = "Original Data") +
  labs(y="Count",x="ECG",fill="Target")

ecgPlot2 <- ggplot(syndata_df,aes(y = as.factor(Resting_ECG),fill=as.factor(Target)))+
  geom_bar(alpha=0.2, stat="count")+
  scale_fill_tron()+
  coord_flip() +
  ggtitle( label="ECG vs Target", subtitle = "Synthetic Data") +
  labs(y="Count",x="ECG",fill="Target")

ecgPlot3 <- ggplot(mergedData,aes(y = as.factor(Resting_ECG),fill=as.factor(Target)))+
  geom_bar(alpha=0.2, stat="count")+
  scale_fill_tron()+
  coord_flip() +
  ggtitle( label="ECG vs Target", subtitle = "Merged Data") +
  labs(y="Count",x="ECG",fill="Target")

ecgPlot <- ggarrange(ecgPlot1, ecgPlot2, ecgPlot3, ncol = 2, nrow = 2)
ecgPlot

## PLOTS FOR MAX HEARTRATE VS TARGET
hrPlot1 <- ggplot(clevelandHeart,aes(Max_Heart_Rate,col=as.factor(Target),fill=as.factor(Target)))+
  geom_histogram(bins= 20,alpha=0.2)+
  guides(col=F)+
  scale_fill_tron()+
  ggtitle( label="Heart Rate vs Target", subtitle = "Original Data") +
  labs(y="Count",x="Heart Rate",fill="Target")

hrPlot2 <- ggplot(syndata_df,aes(Max_Heart_Rate,col=as.factor(Target),fill=as.factor(Target)))+
  geom_histogram(bins= 20,alpha=0.2)+
  guides(col=F)+
  scale_fill_tron()+
  ggtitle( label="Heart Rate vs Target", subtitle = "Synthetic Data") +
  labs(y="Count",x="Heart Rate",fill="Target")

hrPlot3 <- ggplot(mergedData,aes(Max_Heart_Rate,col=as.factor(Target),fill=as.factor(Target)))+
  geom_histogram(bins= 20,alpha=0.2)+
  guides(col=F)+
  scale_fill_tron()+
  ggtitle( label="Heart Rate vs Target", subtitle = "Merged Data") +
  labs(y="Count",x="Heart Rate",fill="Target")

hrPlot <- ggarrange(hrPlot1, hrPlot2, hrPlot3, ncol = 2, nrow = 2)
hrPlot

## PLOTS FOR ANIGNA VS TARGET
angPlot1 <- ggplot(clevelandHeart,aes(as.factor(Exercise_Induced_Anigna),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="Exercise Induced Anigna vs Target", subtitle = "Original Data") +
  labs(y="Count",x="Exercise Induced Anigna",fill="Target")

angPlot2 <- ggplot(syndata_df,aes(as.factor(Exercise_Induced_Anigna),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="Exercise Induced Anigna vs Target", subtitle = "Synthetic Data") +
  labs(y="Count",x="Exercise Induced Anigna",fill="Target")

angPlot3 <- ggplot(mergedData,aes(as.factor(Exercise_Induced_Anigna),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="Exercise Induced Anigna vs Target", subtitle = "Merged Data") +
  labs(y="Count",x="Exercise Induced Anigna",fill="Target")

angPlot <- ggarrange(angPlot1, angPlot2, angPlot3, ncol = 2, nrow = 2)
angPlot

## PLOTS FOR ST DEPRESSION VS TARGET
stPlot1 <- ggplot(clevelandHeart,aes(as.factor(Target),ST_Depressions,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="ST Depressions vs Target", subtitle = "Original Data") +
  labs(y="Count",x="ST Depressions",fill="Target")
stPlot1

stPlot2 <- ggplot(syndata_df,aes(as.factor(Target),ST_Depressions,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="ST Depressions vs Target", subtitle = "Synthetic Data") +
  labs(y="Count",x="ST Depressions",fill="Target")

stPlot3 <- ggplot(mergedData,aes(as.factor(Target),ST_Depressions,fill=as.factor(Target)))+
  geom_boxplot(alpha=0.2)+
  scale_fill_tron()+
  ggtitle( label="ST Depressions vs Target", subtitle = "Merged Data") +
  labs(y="Count",x="ST Depressions",fill="Target")

stPlot <- ggarrange(stPlot1, stPlot2, stPlot3, ncol = 2, nrow = 2)
stPlot

## PLOTS FOR ST DEPRESSION SLOPE VS TARGET
stsPlot1 <- ggplot(clevelandHeart,aes(as.factor(ST_Depressions_Slope),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="ST Depressions Slope vs Target", subtitle = "Original Data") +
  labs(y="Count",x="ST Depressions Slope",fill="Target")

stsPlot2 <- ggplot(syndata_df,aes(as.factor(ST_Depressions_Slope),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="ST Depressions Slope vs Target", subtitle = "Synthetic Data") +
  labs(y="Count",x="ST Depressions Slope",fill="Target")

stsPlot3 <- ggplot(mergedData,aes(as.factor(ST_Depressions_Slope),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="ST Depressions Slope vs Target", subtitle = "Merged Data") +
  labs(y="Count",x="ST Depressions Slope",fill="Target")

stsPlot <- ggarrange(stsPlot1, stsPlot2, stsPlot3, ncol = 2, nrow = 2)
stsPlot


## PLOTS FOR MAJOR VESSELS COLORED VS TARGET
mvPlot1 <- ggplot(clevelandHeart,aes(as.factor(Major_Vessels_Colored),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Major Vessels Colored vs Target", subtitle = "Original Data") +
  labs(y="Count",x="Major Vessels Colored",fill="Target")

mvPlot2 <- ggplot(syndata_df,aes(as.factor(Major_Vessels_Colored),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Major Vessels Colored vs Target", subtitle = "Synthetic Data") +
  labs(y="Count",x="Major Vessels Colored",fill="Target")

mvPlot3 <- ggplot(mergedData,aes(as.factor(Major_Vessels_Colored),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Major Vessels Colored vs Target", subtitle = "Merged Data") +
  labs(y="Count",x="Major Vessels Colored",fill="Target")



mvPlot <- ggarrange(mvPlot1, mvPlot2, mvPlot3, ncol = 2, nrow = 2)
mvPlot

## PLOTS FOR THALLIUM TEST VS TARGET
ttPlot1 <- ggplot(clevelandHeart,aes(as.factor(Thallium_Test),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Thallium Test vs Target", subtitle = "Original Data") +
  labs(y="Count",x="Thallium Test",fill="Target")

ttPlot2 <- ggplot(syndata_df,aes(as.factor(Thallium_Test),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Thallium Test vs Target", subtitle = "Synthetic Data") +
  labs(y="Count",x="Thallium Test",fill="Target")

ttPlot3 <- ggplot(mergedData,aes(as.factor(Thallium_Test),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Thallium Test vs Target", subtitle = "Merged Data") +
  labs(y="Count",x="Thallium Test",fill="Target")

ttPlot <- ggarrange(ttPlot1, ttPlot2, ttPlot3, ncol = 2, nrow = 2)
ttPlot

## PLOT FOR TARGET VS COUNT
tarPlot1 <- ggplot(clevelandHeart,aes(as.factor(Target),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Target Count", subtitle = "Original Data") +
  labs(y="Count",x="Target",fill="Target")

tarPlot2 <- ggplot(syndata_df,aes(as.factor(Target),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Target Count", subtitle = "Synthetic Data") +
  labs(y="Count",x="Target",fill="Target")

tarPlot3 <- ggplot(mergedData,aes(as.factor(Target),fill=as.factor(Target)))+
  geom_bar(stat="count", alpha = 0.2)+
  scale_fill_tron()+
  ggtitle( label="Target Count", subtitle = "Merged Data") +
  labs(y="Count",x="Target",fill="Target")

tarPlot <- ggarrange(tarPlot1, tarPlot2, tarPlot3, ncol = 2, nrow = 2)
tarPlot

## TEST AND TRAIN DATA SPLIT
## uncomment this line in initial run
 smp_size <- floor(0.75 * nrow(clevelandHeart))
# set the seed to make your partition reproducible
set.seed(189028)
train_ind <- sample(seq_len(nrow(clevelandHeart)), size = smp_size)
train <- clevelandHeart[train_ind, ]
test <- clevelandHeart[-train_ind, ]
knn_model_1 <- knn(train,test,cl=train$Target,k=6)
print("kNN on Original Dataset  = ")
print(table(knn_model_1, test$Target))
print(paste("Accuracy of kNN on Original Dataset = ", (25 + 22)*100/74,"%"))
print(paste("Error Rate of kNN on Original Dataset = ", (14 + 13)*100/74,"%"))
## uncomment this line in initial run
smp_size_syn <- floor(0.75 * nrow(syndata_df))
set.seed(189028)
syn_train_ind <- sample(seq_len(nrow(syndata_df)), size = smp_size_syn)
syn_train <- syndata_df[syn_train_ind, ]
syn_test <- syndata_df[-syn_train_ind, ]
knn_model_2 <- knn(syn_train,syn_test,cl=syn_train$Target,k=6)
print("Accuracy of kNN on Synthetic Dataset = ")
print(table(knn_model_2, syn_test$Target))
print(paste("Accuracy of kNN on Synthetic Dataset = ", (20 + 27)*100/74,"%"))
print(paste("Error Rate of kNN on Synthetic Dataset = ", (18 + 9)*100/74,"%"))

## uncomment this line in initial run
# smp_size_merged <- floor(0.75 * nrow(mergedData))
set.seed(189028)
merged_train_ind <- sample(seq_len(nrow(mergedData)), size = smp_size_merged)
merged_train <- mergedData[merged_train_ind, ]
merged_test <- mergedData[-merged_train_ind, ]
knn_model_3 <- knn(merged_train,merged_test,cl=merged_train$Target,k=6)
print("kNN on Merged Dataset")
print(table(knn_model_3, merged_test$Target))
print(paste("Accuracy of kNN on Merged Dataset = ", (59 + 42)*100/148,"%"))
print(paste("Error Rate of kNN on Merged Dataset = ", (22 + 25)*100/148,"%"))

## Support Vector Machines
SVM_model_1 = svm(Target ~ ., train, type = 'C-classification', kernel='linear')
pred = predict(SVM_model_1, test[-14])
print("SVM on Original Dataset")
print(table(test[,14], pred))
print(paste("Accuracy of SVM on Original Dataset = ", (37 + 24)*100/74,"%"))
print(paste("Error Rate of kNN on Original Dataset = ", (12 + 1)*100/74,"%"))

SVM_model_2 = svm(Target ~ ., syn_train, type = 'C-classification', kernel='linear')
pred = predict(SVM_model_2, syn_test[-14])
print("SVM on Synthetic Dataset")
print(table(syn_test[,14], pred))
print(paste("Accuracy of SVM on Synthetic Dataset = ", (37 + 22)*100/74,"%"))
print(paste("Error Rate of kNN on Synthetic Dataset = ", (8 + 7)*100/74,"%"))

SVM_model_3 = svm(Target ~ ., merged_train, type = 'C-classification', kernel='linear')
pred = predict(SVM_model_3, merged_test[-14])
print("SVM on Merged Dataset")
print(table(merged_test[,14], pred ))
print(paste("Accuracy of SVM on Merged Dataset = ", (73 + 52)*100/148,"%"))
print(paste("Error Rate of kNN on Merged Dataset = ", (11 + 12)*100/148,"%"))

## LOGISTIC REGRESSION
LR_model_1 = glm (Target ~ ., data = train, family = binomial)
predict <- predict(LR_model_1, test[-14], type = 'response')
print("Logistic Regression on Original Dataset")
print(table(test$Target, predict > 0.7))
print(paste("Accuracy of SVM on Original Dataset = ", (37 + 18)*100/74,"%"))
print(paste("Error Rate of kNN on Original Dataset = ", (18 + 1)*100/74,"%"))

LR_model_2 = glm (Target ~ ., data = syn_train, family = binomial)
predict <- predict(LR_model_2, syn_test[-14], type = 'response')
print("Logistic Regression on Synthetic Dataset")
print(table(syn_test$Target, predict > 0.7))
print(paste("Accuracy of LogR on Synthetic Dataset = ", (25 + 30)*100/74,"%"))
print(paste("Error Rate of LogR on Synthetic Dataset = ", (15 + 4)*100/74,"%"))

LR_model_3 = glm (Target ~ ., data = merged_train, family = binomial)
predict <- predict(LR_model_3, merged_test[-14], type = 'response')
print("Logistic Regression on Merged Dataset")
print(table(merged_test$Target, predict > 0.7))
print(paste("Accuracy of LogR on Merged Dataset = ", (79 + 43)*100/148,"%"))
print(paste("Error Rate of LogR on Merged Dataset = ", (21 + 5)*100/148,"%"))

#QDA Model
qda_model_original = qda(Target ~ ., data = train)
qda_predictions = predict(qda_model_original, test)
print("QDA on Original Dataset")
print(table(qda_predictions$class, test$Target))
print(paste("Accuracy of QDA on Original Dataset = ", (33 + 24)*100/74,"%"))
print(paste("Error Rate of QDA on Original Dataset = ", (12 + 5)*100/74,"%"))

qda_model_syn = qda(Target ~ ., data = syn_train)
qda_syn_prediction = predict(qda_model_syn, syn_test)
print("QDA on Synthetic Dataset")
print(table(qda_syn_prediction$class, syn_test$Target))
print(paste("Accuracy of QDA on Synthetic Dataset = ", (22 + 35)*100/74,"%"))
print(paste("Error Rate of QDA on Synthetic Dataset = ", (10 + 7)*100/74,"%"))

qda_model_merged = qda(Target ~ ., data = merged_train)
qda_merged_prediction = predict(qda_model_merged, merged_test)
print("QDA on Synthetic Dataset")
print(table(qda_merged_prediction$class, merged_test$Target))
print(paste("Accuracy of QDA on Synthetic Dataset = ", (73 + 54)*100/148,"%"))
print(paste("Error Rate of QDA on Synthetic Dataset = ", (10 + 11)*100/148,"%"))

# Decision Tree
decision_tree_model = rpart(Target~., data = train, method = "class")
rpart.plot(decision_tree_model)
predictions = predict(decision_tree_model, test[,-14], type="class")
print("Decision Tree on Original Dataset")
print(table(predictions, test$Target))
print(paste("Accuracy of Decision Tree on Original Dataset = ", (32 + 22)*100/74,"%"))
print(paste("Error Rate of Decision Tree on Original Dataset = ", (14 + 6)*100/74,"%"))

decision_tree_model_syn = rpart(Target~., data = syn_train, method = "class")
rpart.plot(decision_tree_model_syn)
syn_predictions = predict(decision_tree_model_syn, syn_test[,-14], type="class")
print("Decision Tree on Original Dataset")
print(table(syn_predictions, syn_test$Target))
print(paste("Accuracy of Decision Tree on Synthetic Dataset = ", (36 + 22)*100/74,"%"))
print(paste("Error Rate of Decision Tree on Synthetic Dataset = ", (7 + 9)*100/74,"%"))

decision_tree_model_merged = rpart(Target~., data = merged_train, method = "class")
rpart.plot(decision_tree_model_merged)
merged_predictions = predict(decision_tree_model_merged, merged_test[,-14], type="class")
print("Decision Tree on Merged Dataset")
print(table(merged_predictions, merged_test$Target))
print(paste("Accuracy of Decision Tree on Merged Dataset = ", (73 + 53)*100/148,"%"))
print(paste("Error Rate of Decision Tree on Merged Dataset = ", (11 + 11)*100/148,"%"))

# Pruning the tree 
merged_prune <- prune(decision_tree_model_merged, cp = 0.03)
rpart.plot(merged_prune)
merged_predictions = predict(merged_prune, merged_test[,-14], type="class")
print("Pruned Decision Tree on Merged Dataset")
print(table(merged_predictions, merged_test$Target))
print(paste("Accuracy of Pruned Decision Tree on Merged Dataset = ", (72 + 51)*100/148,"%"))
print(paste("Error Rate of Pruned Decision Tree on Merged Dataset = ", (12 + 13)*100/148,"%"))

#Bagging the tree
library("ipred")
set.seed(189028)
merged_prune_bag <- bagging(
  formula = Target ~ .,
  data = merged_train,
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)
print(merged_prune_bag)
pred <- predict(merged_prune_bag, merged_test[-14], type="class")
pred
for(i in 1:148){
   ifelse(pred[i] >= 0.5, pred[i] <- 1, pred[i] <- 0)
}
pred
result <- data.frame(original = merged_test$Target, pred)
print("Bagged Decision Tree on Merged Dataset")
print(table(merged_test$Target,pred))
print(paste("Accuracy of Bagged Decision Tree on Original Dataset = ", (75 + 55)*100/148,"%"))
print(paste("Error Rate of Bagged Decision Tree on Original Dataset = ", (9 + 9)*100/148,"%"))

