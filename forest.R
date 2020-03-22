# libraries ---------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(ggcorrplot)

# data --------------------------------------------------------------------
#read in data
df_raw <- read_csv('./data/covtype.data', col_names=FALSE)

#Looking at the first few rows of the data
df_raw %>% head()

df_raw %>% glimpse()
#Observations: 581,012
#Variables: 55
# none of the columns in the dataset were labeled 
#So, I looked into column names from covtype.info file and added column names to the dataset

temp_name <- c('Elevation', 'Aspect', 'Slope', 'Horizontal_Distance_To_Hydrology', 'Vertical_Distance_To_Hydrology', 
             'Horizontal_Distance_To_Roadways', 'Hillshade_9am', 'Hillshade_Noon', 'Hillshade_3pm',
             'Horizontal_Distance_To_Fire_Points', 'Wilderness_Area1', 'Wilderness_Area2', 'Wilderness_Area3',
             'Wilderness_Area4')

# create soil column names
num <-1:40
soil_name <- paste('Soil_Type', num, sep='')
col_names <- c(temp_name, soil_name, 'Cover_Type')

# add column names to the dataframe
df <- df_raw
colnames(df) <- col_names

# Exploratory Data Analysis -----------------------------------------------
#check dimension and datatype of dataframe
df %>% glimpse() #check data structure, check is there a missing data 
#all feature data type are numeric
#some features should be categorical (wilderness area, soil type) not numeric
#Wilderness_Area variable was recorded as 4 binary variables
#Soil_Type variable was recorded as 40 binary variables 

#check is there is a missing value
any(is.na(df)) # no missing value

# #change some columns to categorical columns
# df <- df %>% 
#   mutate_each(funs(factor), c(starts_with("Wild"), starts_with("Soil"), "Cover_Type"))

#change Cover_Type to categorical 
df <- df %>% 
  mutate_each(funs(factor), "Cover_Type")

# #Change level names of Cover_Type 
# Forest Cover Type Classes:	
# 1 -- Spruce/Fir
# 2 -- Lodgepole Pine
# 3 -- Ponderosa Pine
# 4 -- Cottonwood/Willow
# 5 -- Aspen
# 6 -- Douglas-fir
# 7 -- Krummholz

#levels(df$Cover_Type)
#levels(df$Cover_Type) <- c('Spruce/Fir', 'Lodgepole Pine', 'Ponderosa Pine','Cottonwood/Willow', 'Aspen','Douglas-fir', 'Krummholz')
                                              
#gather some columns for plotting 

#summary statistics
#summary(df) 
library(psych)
df %>% select(Elevation:Horizontal_Distance_To_Fire_Points) %>%
  describe()

#- Elevation: all cover types are at quite high elevation with minimum elevation at 1859 meter
#- Slope: range of slope is quite large with minimum 0 and maximum 66 degree  
#- high slope, mountainous areas
#- Wilderness_Area1: mostly in wilderness area 1 (mean 0.4489) and 3 (mean 0.4361)
#- Hillshade_3pm 
#- use a count or a histogram - median and mean of the response variable is class 2

# standard deviations of numerical features
sd_numeric <- df %>% 
  select(Elevation:Horizontal_Distance_To_Fire_Points) %>%
  summarise_each(funs(sd))
print(sd_numeric)

#summarize(avg=mean(var2), n=n(), sd=sd(var2))
#summarise_all(list(min, max))

#ratio of Cover_Type
cover_ratio <- df %>% 
  group_by(Cover_Type) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

#correlation of continuous features
df_sub <- df %>%
  select(-starts_with("Soil"), -starts_with("Wild"), -'Cover_Type')
df_sub_corr <- df_sub %>% 
  cor() 
#df_sub_corr %>% ggcorrplot()
df_sub_corr %>% ggcorrplot(type = "lower", outline.col = "white")

# #correlation
# df_corr <- cor(df)
# cover_type_corr <- df_corr[55,1:54]
# range(cover_type_corr)
# cover_type_corr[abs(cover_type_corr) > 0.2]

#bar plot of cover type shows that cover type 1,2 dominate 

# df %>% select(Cover_Type) %>% mutate(Cover_type = as.numeric(Cover %>% hist()
hist(df$Elevation)
hist(df$Slope)
hist(df$Horizontal_Distance_To_Hydrology)
hist(df$Vertical_Distance_To_Hydrology)

#Box plot of numeric features 
df %>%
  select(Elevation) %>%
  ggplot(aes(x=Elevation)) +
  geom_boxplot()


#boxplot of Elevation versus cover type, each cover type has median at different elevation
ggplot(df, aes(x=Cover_Type, y=Elevation)) +
  geom_boxplot()
#density plot of elevation versus cover type (overlap of elevation, can't build model with this feature alone)
ggplot(df, aes(x=Elevation, color=as.factor(Cover_Type)), alpha=0.05) + 
  geom_density()

#boxplot of slope versus cover type
ggplot(df, aes(x=as.factor(Cover_Type), y=Slope)) +
  geom_boxplot()

#boxplot of Horizontal_Distance_To_Hydrology versus cover type
ggplot(df, aes(x=as.factor(Cover_Type), y=Horizontal_Distance_To_Hydrology)) +
  geom_boxplot()

#boxplot of Vertical_Distance_To_Hydrology versus cover type
ggplot(df, aes(x=as.factor(Cover_Type), y=Vertical_Distance_To_Hydrology)) +
  geom_boxplot()

#boxplot of Horizontal_Distance_To_Roadways versus cover type
ggplot(df, aes(x=as.factor(Cover_Type), y=Horizontal_Distance_To_Roadways)) +
  geom_boxplot()

#boxplot of Hillshade_9am versus cover type (0 = darkest area, 255 = brightest area)
ggplot(df, aes(x=as.factor(Cover_Type), y=Hillshade_9am)) +
  geom_boxplot()

#boxplot of Hillshade_Noon versus cover type (0 = darkest area, 255 = brightest area)
ggplot(df, aes(x=as.factor(Cover_Type), y=Hillshade_Noon)) +
  geom_boxplot()

#boxplot of Hillshade_3pm versus cover type (0 = darkest area, 255 = brightest area)
ggplot(df, aes(x=as.factor(Cover_Type), y=Hillshade_3pm)) +
  geom_boxplot()

#boxplot of Horizontal_Distance_To_Fire_Points versus cover type 
ggplot(df, aes(x=as.factor(Cover_Type), y=Horizontal_Distance_To_Fire_Points)) +
  geom_boxplot()

#bar plot of wilderness area and propotion with each cover type (some forest cover type aren't present in all areas)

#bar plots of soil type and propotion with each cover type

#Aspect and cover type plot


# create new features -----------------------------------------------------


#create climatic zone feature based on soil types

#create geologic zone feature based on soil types

#(df_['Vertical_Distance_To_Hydrology']**2+df_['Horizontal_Distance_To_Hydrology']**2)**0.5

#1. Describe the dataset and any interesting variables (feel free to include response)
#Forest cover type dataset
#It has 581,012 observations and 55 features, all are numeric with no missing data
#interesting variables
#reponse variable is Cover_Type, there are seven forest cover types
#histogram of Cover_Type shows that major cover types are class 1 and 2 with the average being type 2 
#mostly in wilderness area 1 (mean 0.4489) and 3 (mean 0.4361)


#2. Provide up to 5 key insights that you learned from this dataset

#1. density plot of elevation versus cover type (overlap of elevation, can't build model with this feature alone)

#3. Highlight challenges in the dataset and the plans to mitigate those
#challenges of the data
#- overlap of features
#- from box plots, features of some cover type have quite a few outliers, meaning they can mix in with other cover types 
#- no missing data

#If there are missing data, how would you address this?
#- if there aren't too many data missing compare to numbers of observations, remove rows with missing data
#- if the effect of that column isn't significant for a model, remove the whole column
#- imputation, replace missing data with median (median imputation) or mean 
#- #keep missing data as 'missing' category: coarse classification 

#dealing with outliers: 
#cleaning outliers 1) use expert suggestion
#2) use rule of thump: outlier if bigger or smaller than > Q3 + 1.5*(IQR)
#IQR(interquartile range)


## Feature Engineering
#features scaling: scale numeric features to range 0 to 1
# Normalization: the numeric features can be scaled to range between 0 and 1. It is useful when the data is uniform across overall ranges and has few outliers.
# + new_feature = (x - min(x))/(max(x) - min(x))
#load caret package
library(caret)
processed_var <- preProcess(df %>% select(Elevation:Horizontal_Distance_To_Fire_Points), method = 'range')
df_scaled <- predict(processed_var, df)

##Split train/test set
#load rsample package 
library(rsample)
#spliting data to train and test sets
set.seed(20)
df_split <- initial_split(df_scaled, prop = 0.75)
df_train <- training(df_split)
df_test <- testing(df_split)

# Modeling strategy -------------------------------------------------------
#Multinomial Logistic Regression
#load library
library(nnet)
start_time <- Sys.time()
model_log <- multinom(Cover_Type ~ Elevation + Horizontal_Distance_To_Roadways, data = df_train)
#model_log <- multinom(Cover_Type ~ ., data = df_train)
end_time <- Sys.time()
summary(model_log) #very slow
#str(model_log)
coef(model_log)

#make predictions
#pred <- predict(model_log, df_test, type='prob')
pred <- predict(model_log, df_test)

#create confusion matrix
con_mat <- table(df_test$Cover_Type, pred) 
print(con_mat)

#Then, calculate model accuracy and f1 score
#```{r accuracy}
#calculate accuracy
accuracy_log <- sum(diag(con_mat))/sum(con_mat)
print(paste('accuracy = ', accuracy_log))

#calculate f1 score
precision = matrix(NA, nrow=7)
recall = matrix(NA, nrow = 7)
for (i in 1:7) {
  precision[i] <- round(con_mat[i,i]/colSums(con_mat)[i],3)
  recall[i] <- round(con_mat[i,i]/rowSums(con_mat)[i],3)
}

f1 = 2 * (precision * recall) / (precision + recall)

print('f1 scores of all classes')
print(f1)

print(paste('average f1 score of all classes', round(mean(f1),2)))

# multinomial using caret

#```{r log_model_caret}
#train model with caret package
set.seed(42)
df_ind = createDataPartition(df_scaled$Cover_Type, p = 0.75, list = FALSE)
df_train2 = df_scaled[df_ind, ]
df_test2 = df_scaled[-df_ind, ]
model_log_caret <- train(
  Cover_Type ~ .,
  data = df_train2,
  method = "multinom",
  trControl = trainControl(method = "cv", number = 2),
  #  preProcess = c("pca"),
  #  preProcess = c("center", "scale"),
  trace = FALSE
)

pred_log_model_caret <- predict(model_log_caret, df_test2)
con_mat_caret <- table(df_test2$Cover_Type, pred_log_model_caret)
con_mat_caret
#calculate accuracy
accuracy_log2 <- sum(diag(con_mat_caret))/sum(con_mat_caret)
print(paste('accuracy = ', accuracy_log))

#calculate f1 score
precision2 = matrix(NA, nrow=7)
recall2 = matrix(NA, nrow = 7)
for (i in 1:7) {
  precision2[i] <- round(con_mat_caret[i,i]/colSums(con_mat_caret)[i],3)
  recall2[i] <- round(con_mat_caret[i,i]/rowSums(con_mat_caret)[i],3)
}

f1_caret = 2 * (precision2 * recall2) / (precision2 + recall2)

print('f1 scores of all classes')
print(f1_caret)

print(paste('average f1 score of all classes', round(mean(f1_caret),2)))

#```


#Decision tree
#* Fitting decision tree model
#```{r decision_tree}
library(rpart)
library(Metrics)
n <- nrow(df)
n_train <- round(0.8 * n)
train_ind <- sample(1:n, n_train)
df_train_tree <- df[train_ind, ]
df_test_tree <- df[-train_ind,]
model_tree <- rpart(Cover_Type ~ ., data = df_train_tree, 
                    method = "class")
pred_tree <- predict(model_tree, df_test_tree, type = "class")
con_mat_tree <- confusionMatrix(pred_tree, df_test_tree$Cover_Type)
con_mat_tree
#```

#Fit bagging tree 
library(ipred)
model_bag <- bagging(Cover_Type ~ ., data = df_train_tree) #take too long

#Random Forest
library(caret)
#library(mlbench)
set.seed(42)
model_ran <- train(Cover_Type ~ ., tuneLength = 1, data = df_scaled, method = 'ranger',
                   trControl=trainControl(method = 'cv', number = 2, verboseIter = TRUE)) 




# Train a random forest model ---------------------------------------------
###Train a random forest model
#```{r model_random_forest}
library(caret)
set.seed(42)
model_ran <- train(Cover_Type ~ ., tuneLength = 1, data = df_train, method = 'ranger',
                   trControl=trainControl(method = 'cv', number = 2, verboseIter = TRUE)) 
model_ran
#```

### Make a prediction
#```{r predict_forest}
pred_ran <- predict(model_ran, df_test)
con_mat_ran <- confusionMatrix(pred_ran, df_test$Cover_Type)
print(con_mat_ran) 
#```
#```{r model_forest2}
set.seed(64)
tunegrid <- expand.grid(.mtry=8, 
                        .splitrule="gini",
                        .min.node.size=1)
model_ran2 <- train(Cover_Type ~ ., tuneLength = 1, data = df_train, method = 'ranger', 
                    tuneGrid=tunegrid,
                    trControl=trainControl(method = 'cv', number = 2, verboseIter = TRUE)) 

print(model_ran2)
#```



#```{r model_forest3}
set.seed(64)
tunegrid <- expand.grid(.mtry=14, 
                        .splitrule="gini",
                        .min.node.size=1)
model_ran3 <- train(Cover_Type ~ ., tuneLength = 1, data = df_train, method = 'ranger', 
                    tuneGrid=tunegrid,
                    #                    num.trees = 500,
                    trControl=trainControl(method = 'cv', number = 2, verboseIter = TRUE)) 
print(model_ran3)
#```

#```{r predict_forest3}
pred_ran3 <- predict(model_ran3, df_test)
con_mat_ran3 <- confusionMatrix(pred_ran3, df_test$Cover_Type)
print(con_mat_ran3) 
#```



# Results -----------------------------------------------------------------

#+ accuracy: numbers of correct predictions/number of total data points
#+ sensitivity (true positive rate or recall): measures the proportion of actual positives that are correctly identified
#+specificity (true negative rate): measures the proportion of actual negatives that are correctly identified
