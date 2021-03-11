library(tidyverse) 
library(data.table)
library(rstudioapi)
library(recipes)
library(caret)
library(skimr)
library(stringr)
library(purrr)
library(inspectdf)
library(mice)
library(dplyr)
library(graphics)
library(Hmisc)
library(glue)
library(highcharter)
library(plotly)
library(h2o) 


raw <- fread("crimes.csv",na="")

raw %>% skim()

df.num <- raw %>% 
  select_if(is.numeric) %>% 
  select(ViolentCrimesPerPop,everything())


raw %>% glimpse()
#Outliers

num_vars <- df.num %>% 
  select(-ViolentCrimesPerPop) %>% 
  names()

for_vars <- c()

num_vars %>% length()
#Detection of variables which contain at least one outlier
for(b in 1:length(num_vars)){
  OutVals <- boxplot(df.num[[num_vars[b]]],plot = F)$out
  if(length(OutVals>0)){
    for_vars[b] <- num_vars[b]
    
  }
  
}

for_vars <- for_vars %>% as.data.frame() %>% drop_na() %>% pull(.) %>% as.character()
for_vars %>% length()

#Fixing the outliers with upper and lower plunge values
for(o in for_vars){
  OutVals <- boxplot(df.num[[o]],plot = F)$out
  mean <- mean(df.num[[o]],na.rm=T)
  
  o3 <- ifelse(OutVals>mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% t() %>% .[1,]
  o1 <- ifelse(OutVals<mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% t() %>% .[1,]
  
  val3 <- quantile(df.num[[o]],0.75,na.rm = T) + 1.5*IQR(df.num[[o]],na.rm = T)
  df.num[which(df.num[[o]] %in% o3),o] <- val3
  
  val1 <- quantile(df.num[[o]],0.25,na.rm = T) - 1.5*IQR(df.num[[o]],na.rm = T)
  df.num[which(df.num[[o]] %in% o1),o] <- val1
  
  
}


#--------------------------------Multicolleniarity------------------------------------------

target <- "ViolentCrimesPerPop"
variables <- df.num %>% select(-ViolentCrimesPerPop) %>% names()


f <- as.formula(paste(target,paste(variables,collapse = " + "),sep = " ~ "))
glm <- glm(f,data = df.num)

glm %>% summary()

#By heart??
coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
variables <- variables[!variables %in% coef_na]

f <- as.formula(paste(target,paste(variables,collapse = " + "),sep = " ~ "))
glm <- glm(f,data = df.num)

glm %>% summary()


#Number 1
#VIF

library(faraway)

glm %>% vif() %>% sort(decreasing = T) %>% length()
while(glm %>% vif() %>% sort(decreasing = T) %>% .[1] >= 1.5){
  afterVIF <- glm %>% vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target,paste(afterVIF,collapse = " + "),sep = " ~ "))
  glm <- glm(f,data = df.num)
}
glm %>% vif() %>% sort(decreasing = T) %>% length()

variables <- glm %>% vif() %>% sort(decreasing = T) %>% names() 

df <- df.num %>% select(ViolentCrimesPerPop,variables)
df %>% skim()

#Number 2
#Standardize and normalise the features

df <- df %>% scale() %>% as.data.frame()


library(rJava)
Sys.setenv(JAVA_HOME= "C:\\Program Files\\Java\\jre1.8.0_271")
Sys.getenv("JAVA_HOME")

h2o.init(nthreads = -1, max_mem_size = '2g', ip = "127.0.0.1", port = 54321)


h2o_data <- df %>% as.h2o()

#Number 3
#Splitting the data

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8,seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

#train 
#test

#train <- train[,-1] %>% scale() %>% as.data.frame() %>% as.h2o()
#test <- test[,-1] %>% scale() %>% as.data.frame() %>% as.h2o()

target <- "ViolentCrimesPerPop"
variables <- df %>% select(-ViolentCrimesPerPop) %>% names()

#Number 4
#Building the model/Eliminating p-values higher than 0.05

model <- h2o.glm(
  x=variables,y=target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10,seed=123,
  lambda = 0,compute_p_values = T)
  
#variables
  
#model %>% View()

model@model$coefficients_table %>%
  as.data.frame() %>%
  select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

while(model@model$coefficients_table %>%
      as.data.frame() %>%
      select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] > 0.05) {
  model@model$coefficients_table %>%
    as.data.frame() %>%
    select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  variables <- variables[variables!=v]
  
  train_h2o <- train %>% as.data.frame() %>% select(target,variables) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>% select(target,variables) %>% as.h2o()
  
  model <- h2o.glm(
    x = variables, 
    y = target,
    training_frame = train,
    validation_frame = test,
    nfolds = 10, seed = 123,
    lambda = 0, compute_p_values = T)
}

model@model$coefficients_table %>%
  as.data.frame() %>%
  select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))


f <- as.formula(paste(target, paste(variables, collapse = " + "), sep = " ~ "))
glm <- train_h2o %>% as.data.frame() %>% glm(f, data = .)


glm %>% summary()

#Predicting the test results
y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict

test_set <- test %>% as.data.frame()
residuals <- test_set$ViolentCrimesPerPop - y_pred$predict


#Number 5
#RMSE
RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test_set$ViolentCrimesPerPop)

tss = sum((test_set$ViolentCrimesPerPop - y_test_mean)^2)
rss = sum(residuals^2)

R2 = 1 - (rss/tss) ; R2

n <- test_set %>% nrow()
k <- variables %>% length()

#Adjusted_R2
Adjusted_R2 = 1 - (1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)

#Check Overfitting
y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
residuals <- train_set$ViolentCrimesPerPop - y_pred_train$predict


RMSE_train = sqrt(mean(residuals^2))

y_train_mean = mean(train_set$ViolentCrimesPerPop)

tss = sum((train_set$ViolentCrimesPerPop - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1 - rss/tss
R2_train

n <- train_set %>% nrow()
k <- variables %>% length()

Adjusted_R2_train = 1 - (1-R2_train)*((n-1)/(n-k-1)) 


#Compare
tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),
       
       Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2)

#There is no overfitting 

