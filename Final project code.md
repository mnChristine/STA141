---
title: "STA141AFinalproject"
author: "Ning Mu 919588943"
date: "2024-03-18"
output: html_document
---
```{r,echo=FALSE}
library(knitr)
opts_chunk$set(fig.cap="",
               fig.pos = "H",
               #out.extra = "",
               dpi=1500,
               warning = FALSE, 
               message = F,
               echo = F)
```

# Abstract

The aim of this paper is to predict the relationship between visual stimuli and brain activity, decision-making and action in mice. Data collected by Steinmetz et al. (2019), who performed 39 experiments on 10 mice, 18 of which were targeted to 4 mice each (Cori forssman and Hence Lederberg), were analyzed. In this paper, I will explore the features of the dataset to establish our prediction goals by extracting common patterns across trials and/or (dealing with differences between trials) integrating the data across trials. Finally, a prediction model will be developed to predict the outcome (i.e., the type of feedback), demonstrating that the model chosen for this study works well by performing on the test set.

# Section 1: Introduction

In the study conducted by Steinmetz et al. (2019), experiments were performed on a total of 10 mice over 39 sessions. Each session comprised several hundred trials, during which visual stimuli were randomly presented to the mouse on two screens positioned on both sides of it. The stimuli varied in terms of contrast levels, which took values in {0, 0.25, 0.5, 1}, with 0 indicating the absence of a stimulus. The mice were required to make decisions based on the visual stimuli, using a wheel controlled by their forepaws. A reward or penalty (i.e., feedback) was subsequently administered based on the outcome of their decisions. In particular, 

- When left contrast > right contrast, success (1) if turning the wheel to the right and failure (-1) otherwise.  
- When right contrast > left contrast, success (1) if turning the wheel to the left and failure (-1) otherwise.  
- When both left and right contrasts are zero, success (1) if holding the wheel still and failure (-1) otherwise. 
- When left and right contrasts are equal but non-zero, left or right will be randomly chosen (50%) as the correct choice. 


The activity of the neurons in the mice's visual cortex was recorded during the trials and made available in the form of spike trains, which are collections of timestamps corresponding to neuron firing. In this project, I focus specifically on the spike trains of neurons from the onset of the stimuli to 0.4 seconds post-onset. In addition, we only use 18 sessions (Sessions 1 to 18) from four mice: Cori, Frossman, Hence, and Lederberg.


# Section 2: Exploratory analysis

## Data structure 

A total of 18 RDS files are provided that contain the records from 18 sessions. In each RDS file, you can find the name of mouse from `mouse_name` and date of the experiment from `date_exp`. Five variables are available for each trial, namely 

- `feedback_type`: type of the feedback, 1 for success and -1 for failure
- `contrast_left`: contrast of the left stimulus
- `contrast_right`: contrast of the right stimulus
- `time`: centers of the time bins for `spks`  
- `spks`: numbers of spikes of neurons in the visual cortex in time bins defined in `time`
- `brain_area`: area of the brain where each neuron lives.

To get more detailed information, I created the following table in order to show the mouse name, date exp n_brain_area, n_neurons n_trials, success_rate for each of the 18 sessions.

```{r}
session=list()
for(i in 1:18){
  session[[i]]=readRDS(paste('./sessions/session',i,'.rds',sep=''))
}
library(tidyverse)
n.session=length(session)

meta <- tibble(
  mouse_name = rep('name',n.session),
  date_exp =rep('dt',n.session),
  n_brain_area = rep(0,n.session),
  n_neurons = rep(0,n.session),
  n_trials = rep(0,n.session),
  success_rate = rep(0,n.session),
)


for(i in 1:n.session){
  
  tmp = session[[i]];
  meta[i,1]=tmp$mouse_name;
  meta[i,2]=tmp$date_exp;
  meta[i,3]=length(unique(tmp$brain_area));
  meta[i,4]=dim(tmp$spks[[1]])[1];
  meta[i,5]=length(tmp$feedback_type);
  meta[i,6]=mean(tmp$feedback_type+1)/2;
  }
kable(meta, format = "html", table.attr = "class='table table-striped'",digits=2) 

```

Take the 11th trial in Session 5 for example, we can see that the left contrast for this trial is `r 
session[[5]]$contrast_left[11]`  the right contrast is `r 
session[[5]]$contrast_right[11]`, and the feedback (i.e., outcome) of the trial is `r session[[5]]$feedback_type[11]`. There are a total of `r length(session[[5]]$brain_area)` neurons in this trial from `r length(unique(session[[5]]$brain_area))` areas of the brain. The spike trains of these neurons are stored in `session[[5]]$spks[[11]]` which is a `r dim(session[[5]]$spks[[11]])[1]` by `r dim(session[[5]]$spks[[11]])[2]` matrix with each entry being the number of spikes of one neuron (i.e., row) in each time bin (i.e., column).



## Question of interest


The primary objective of this project is to build a predictive model to predict the outcome (i.e., feedback type) of each trial using the neural activity data (i.e., spike trains in `spks`), along with the stimuli (the left and right contrasts). Given the complexity of the data (and that this is a course project), we break the predictive modeling into three parts as follows. 

Part 1. Exploratory data analysis. In this part, we will explore the features of the data sets in order to build our prediction model. In particular, we would like to (i) describe the data structures across sessions (e.g., number of neurons, number of trials, stimuli conditions, feedback types), (ii) explore the neural activities during each trial, (iii) explore the changes across trials, and (iv) explore homogeneity and heterogeneity across sessions and mice. 

In order to better explore the neural activities during each trial and explore the changes across trials, as an example, I have gathered two table to compare the difference between trial1 and 2.

```{r}
get_trail_data <- function(session_id, trail_id){
  spikes <- session[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }
  trail_tibble <- tibble("neuron_spike" = rowSums(spikes))  %>%  
    add_column("brain_area" = session[[session_id]]$brain_area ) %>% 
    group_by(brain_area) %>% summarize( region_sum_spike = sum(neuron_spike),
                                        region_count = n(),region_mean_spike = mean(neuron_spike)) 
  trail_tibble  = trail_tibble%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= session[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= session[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= session[[session_id]]$feedback_type[trail_id])
  trail_tibble
}
```


```{r}
library(tidyverse)
trail_tibble_1_2 <- get_trail_data(1,2)
trail_tibble_1_2
```

```{r}
get_session_data <- function(session_id){
  n_trail <- length(session[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trail_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- do.call(rbind, trail_list)
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}
```


```{r}
session_list = list()
for (session_id in 1: 18){
  session_list[[session_id]] <- get_session_data(session_id)
}
full_tibble <- do.call(rbind, session_list)
full_tibble$success <- full_tibble$feedback_type == 1
full_tibble$success <- as.numeric(full_tibble$success)
full_tibble$contrast_diff <- abs(full_tibble$contrast_left-full_tibble$contrast_right)
```

```{r}
session_1 <- get_session_data(1)
head(session_1)
```
In the table above, I have listed the brain_area, region_sum_spike, region_count, region_mean_spike, and so on for trials 1 and 2. We learned that the regions of the brain-area that were stimulated were different in different trials, and for the same brain-area due to different, different regions of the brain-area are stimulated in different trials, for the same brain-area the region_sum_spike is affected by different trails, etc.



In the next step I need to create a new dataset in order to be more specific for the subsequent analysis.
```{r}
binename <- paste0("bin", as.character(1:40))

get_trail_functional_data <- function(session_id, trail_id){
  spikes <- session[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }

  trail_bin_average <- matrix(colMeans(spikes), nrow = 1)
  colnames(trail_bin_average) <- binename
  trail_tibble  = as_tibble(trail_bin_average)%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= session[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= session[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= session[[session_id]]$feedback_type[trail_id])
  
  trail_tibble
}
get_session_functional_data <- function(session_id){
  n_trail <- length(session[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trail_functional_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- as_tibble(do.call(rbind, trail_list))
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

```

```{r}
session_list = list()
for (session_id in 1: 18){
  session_list[[session_id]] <- get_session_functional_data(session_id)
}
full_functional_tibble <- as_tibble(do.call(rbind, session_list))
full_functional_tibble$session_id <- as.factor(full_functional_tibble$session_id )
full_functional_tibble$contrast_diff <- abs(full_functional_tibble$contrast_left-full_functional_tibble$contrast_right)

full_functional_tibble$success <- full_functional_tibble$feedback_type == 1
full_functional_tibble$success <- as.numeric(full_functional_tibble$success)
```

```{r}
full_functional_tibble <- as_tibble(do.call(rbind, session_list))
full_functional_tibble$session_id <- as.factor(full_functional_tibble$session_id )
full_functional_tibble$contrast_diff <- abs(full_functional_tibble$contrast_left-full_functional_tibble$contrast_right)

full_functional_tibble$success <- full_functional_tibble$feedback_type == 1
full_functional_tibble$success <- as.numeric(full_functional_tibble$success)
full_functional_tibble$trail_group = cut(full_functional_tibble$trail_id, breaks = seq(0, max(full_functional_tibble$trail_id), by = 25),include.lowest = TRUE)
levels(full_functional_tibble$trail_group) <- seq(0, max(full_functional_tibble$trail_id), by = 25)[2:18]

```


In order to explore homogeneity and heterogeneity across sessions and mice, I want to visualize success rate change over individual session and seach mouse, which can make it easy for people to read and get the information.
Firstly, we need to get the success rate is binned for each 25 trails.
```{r,echo=FALSE}
full_functional_tibble$trail_group = cut(full_functional_tibble$trail_id, breaks = seq(0, max(full_functional_tibble$trail_id), by = 25),include.lowest = TRUE)
levels(full_functional_tibble$trail_group) <- seq(0, max(full_functional_tibble$trail_id), by = 25)[2:18]
```

Then I create a graph to show the success rate change over time for individual sessions:

```{r,echo=FALSE}
success_rate <- aggregate(success ~ session_id + trail_group, data = full_functional_tibble, FUN = function(x) mean(x) )
ggplot(success_rate, aes(x = trail_group, y = success)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~session_id, ncol=3) +
      theme_bw()

```
A new graph to show the success rate change over each mouse.

```{r,echo=FALSE}
success_rate <- aggregate(success ~ mouse_name + trail_group, data = full_functional_tibble, FUN = function(x) mean(x) )
ggplot(success_rate, aes(x = trail_group, y = success)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~mouse_name) +
      theme_bw()
```
We can see that the success rate in both different sessions and different rats is slowly decreasing. We can take into account the fatigue factor of the test subjects and so on. Meanwhile, according to the graph ,which show the success rate change over each mouse, we can know hench this kind of mouse's success rate is more stable, more suitable for experimental subjects.
# Section 3: Data integration

Part 2. Data integration. Using the findings in Part 1, we will propose an approach to combine data across trials by (i) extracting the shared patters across sessions and/or (ii) addressing the differences between sessions. The goal of this part is to enable the borrowing of information across sessions to enhance the prediction performance in Part 3. 

```{r}
predictive_feature <- c("session_id","trail_id","contrast_right","contrast_left", "contrast_diff" ,binename)
head(full_functional_tibble[predictive_feature])
```

```{r}
predictive_dat <- full_functional_tibble[predictive_feature]
#predictive_dat$success <- as.numeric(predictive_dat$success)
predictive_dat$trail_id <- as.numeric(predictive_dat$trail_id)
label <- as.numeric(full_functional_tibble$success)
X <- model.matrix(~., predictive_dat)
```

```{r}
predictive_dat <- full_functional_tibble[predictive_feature]
#predictive_dat$success <- as.numeric(predictive_dat$success)
predictive_dat$trail_id <- as.numeric(predictive_dat$trail_id)
label <- as.numeric(full_functional_tibble$success)
X <- model.matrix(~., predictive_dat[,-c(1:2)])
```

Part 3. Model training and prediction. Finally, we will build a prediction model to predict the outcome (i.e., feedback types). The performance will be evaluated on two test sets of 100 trials randomly selected from Session 1 and Session 18, respectively. The test sets will be released on the day of submission when you need to evaluate the performance of your model. 

# Section 4: Predictive modeling

Some advantages of XGBoost algorithm in classification modeling:
Accuracy: XGBoost shows high accuracy on a wide range of classification problems, especially when dealing with complex and nonlinear relationships.
Speed: The speed of the xgboost model is very fast compared to other gradient boosting algorithms.
Flexibility: XGBoost supports a wide range of loss functions, which can be used for different classification problems, such as binary classification, multiple classification, and sorting problems.

Memory efficiency: XGBoost is more memory efficient when dealing with large datasets compared to other integrated learning methods.

Automatic Feature Importance: XGBoost provides automatic feature importance scoring, the model is easy to interpret and helps users understand which features have the greatest impact on the prediction results.
Combining the advantages of the above models, therefore, the xgboost model is considered for binary classification in this study.

First, divide the training set and test set, the training set is used to train the model and the test set is used to validate the model.

```{r}
# split
set.seed(123) # for reproducibility
library(caret)
trainIndex <- createDataPartition(label, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_df <- predictive_dat[trainIndex, ]
train_X <- X[trainIndex,]
test_df <- predictive_dat[-trainIndex, ]
test_X <- X[-trainIndex,]

train_label <- label[trainIndex]
test_label <- label[-trainIndex]
```

Set the mesh parameters of the model through the expand.grid function.In order to ensure the optimal performance of the experimental model, a grid search technique was employed, which allows for a comprehensive exploration of parameter combinations in order to precisely define the optimal parameter settings for the model.

```{r,results="hide"}
grid =
  expand.grid(
    nrounds = c(75, 100),
    colsample_bytree = 1,
    min_child_weight = 1,
    eta = c(0.01, 0.1, 0.3),
    gamma = c(0.5, 0.25),
    subsample = 0.5,
    max_depth = c(2, 3)
    )
grid
library(caret)
cntrl = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "final"                                                        
)
train.xgb = train(
  x = train_X,
  y = train_label,#
  trControl = cntrl,
  tuneGrid = grid,
  method = "xgbTree"
)

```

```{r}
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "error",
                eta                 = 0.1, 
                max_depth           = 2, 
                subsample           = 0.5,
                colsample_bytree    = 1,
                gamma               = 0.5
                )
```


The "xgb.DMatrix" function processes data by combining matrices and labeled lists into a data format that meets the requirements

```{r}
library(xgboost)
train.mat <- xgb.DMatrix(data = train_X, 
                         label = train_label)
```

Train the model with the xgb.train function, where data includes input data and labeled data, and nrounds iterations.

```{r}
set.seed(1)
library(xgboost)
xgb.fit <- xgb.train(params = param, data = train.mat, nrounds = 100)
xgb.fit
```


```{r}
pred <- predict(xgb.fit, test_X)
impMatrix <- xgb.importance(feature_names = dimnames(test_X)[[2]], model = xgb.fit)
head(impMatrix)
```

Draw a variable importance diagram, where impMatrix is the important feature matrix,which shows the importance of your independent variable.Indicating which independent variable has a greater effect on the dependent variable.

```{r}
xgb.plot.importance(impMatrix, main = "Gain by Feature")
```


Then I draw ROC curve using the plotROC function in the InformationValue package to easily get the AUROC number.

```{r}
library(InformationValue)
plotROC(test_label,pred)
```

# Section 5: Prediction performance on the test sets

100 randomly selected experimental tests from Phase 1

```{r}
set.seed(12)
predictive_dat1 <- full_functional_tibble[predictive_feature] %>% 
  mutate(success = full_functional_tibble$success) %>% 
  filter(session_id == 1) %>% 
  sample_n(100)
X1 <- model.matrix(~., predictive_dat1  |> select(-session_id,-success,-trail_id))
```

Prediction results:

Define model evaluation function

```{r}
performance<-function(table,n=2){
  if(!all(dim(table)==c(2,2)))
    stop("MUST be a 2*2 table")
  tn = table[2,2]
  fp = table[2,1]
  fn = table[1,2]
  tp = table[1,1]
  sensitivity = tp / ( tp + fn )
  specificity = tn / ( tn + fp )
  positive = tp / (tp + fp)
  negative = tn / (tn + fn)
  hitrate = (tp + tn)/(tp + tn + fp + fn)
  result<- paste("Sensitivity = ",round(sensitivity,n),
                 "\nSpecificity = ",round(specificity,n),
                 "\nPositive predictive Value = ",round(positive,n),
                 "\nNegative predictive Value = ",round(negative,n),
                 "\nAccuracy = ",round(hitrate,n),
                 "\n",sep="")
  cat(result)
}
```

Model prediction, obtaining evaluation indicators for the model

```{r}
pred1 <- predict(xgb.fit,X1)
predclass <- ifelse(pred1 > 0.5,1,0)
tab1 <- table(predictive_dat1$success,predclass)
performance(tab1)
```
Since all the pictures of the ROC curve are roughly similar to the above figure, in order to simplify the output form, the figure is displayed in the form of data, which more clearly expresses the value of Area under the curve.
```{r}
library(pROC)
pred1 <- predict(xgb.fit,X1)
auroc1 <- roc(predictive_dat1$success,pred1)
auroc1
```

100 randomly selected experimental tests from Phase 18

```{r}
# split
set.seed(123) 
predictive_dat18 <- full_functional_tibble[predictive_feature] %>% 
  mutate(success = full_functional_tibble$success) %>% 
  filter(session_id == 18) %>% 
  sample_n(100)
X18 <- model.matrix(~., predictive_dat1  |> select(-session_id,-success,-trail_id))
```

Model prediction, obtaining evaluation indicators for the model

```{r}
pred18 <- predict(xgb.fit,X18)
predclass18 <- ifelse(pred18 > 0.5,1,0)
tab18 <- table(predictive_dat18$success,predclass18)
performance(tab18)
```

show the ROC curve of the model and obtain the AUC index of the model
```{r}
library(pROC)
pred18 <- predict(xgb.fit,X18)
auroc18 <- roc(predictive_dat18$success,pred18)
auroc18
```


# Section 6: Discussion 

In this experiment, in exploring methods for analyzing neural activity data in mice, I chose to employ the xgboost model, a machine learning algorithm popular for its powerful classification and regression analysis capabilities. As a result of this process, an xgboost model was constructed that performed well on the test set.The evaluation of the model performance in this project not only focuses on the algorithm output, but also includes a careful analysis of the auc metrics, accuracy metrics and roc curves. These two metrics are indispensable tools in the evaluation of classification models, where auc measures the model's ability to rank positively classified samples, and roc curve visualizes the model's performance under different decision thresholds. The experimentally constructed xgboost model exhibits excellent auc metrics and roc curves in the test set, which fully demonstrates the high accuracy of its predictions.

The reliability of the model is further validated by experiments. The experimental results consistently showed that the xgboost model had convincing accuracy in predicting mouse behavior, which further confirmed the practical value of the model.

In conclusion, the xgboost model in this study successfully predicted the behavioral patterns of mice under various stimulus conditions, and the results of the study undoubtedly provide a powerful tool for research in related fields, and also have important reference value and significance for future related fields.

# Shortcomings and improvements

This study has the following shortcomings.

1, the study only chooses xgboost model to analyze, the model selection is relatively single, lack of other models for comparison.

2, The sample data of the experiment is not large enough, which may make the results unstable.

3, There may be outliers and other situations in the data, and the study is not sufficiently cleaned.

Improvement measures:

1, the follow-up study can consider adding random forest model, decision tree model, logistic regression model, etc. to go for comparison.

2, increase the sample size of the experimental data, making the experimental results more accurate.

3、Sufficient cleaning of data.

# Predict test1
Because the model has already be trianed and this new dataset is just for testing, so we cannot  use the test data to train our model.I started by preprocessing my test data set and then finally made it output the predicted results
```{r}
library(tidyverse)
library(caret) 
library(xgboost)
library(pROC)
session=list()
for(i in 1){
  session[[i]]=readRDS("./test/test1.rds")
  print(session[[i]]$mouse_name)
  
  print(session[[i]]$date_exp)
  
}
get_trail_data <- function(session_id, trail_id){
  spikes <- session[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }
  trail_tibble <- tibble("neuron_spike" = rowSums(spikes))  %>%  
    add_column("brain_area" = session[[session_id]]$brain_area ) %>% 
    group_by(brain_area) %>% summarize( region_sum_spike = sum(neuron_spike),
                                        region_count = n(),region_mean_spike = mean(neuron_spike)) 
  trail_tibble  = trail_tibble%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= session[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= session[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= session[[session_id]]$feedback_type[trail_id])
  trail_tibble
}

get_session_data <- function(session_id){
  n_trail <- length(session[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trail_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- do.call(rbind, trail_list)
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

session_1 <- get_session_data(1)

session_list = list()
for (session_id in 1:1){
  session_list[[session_id]] <- get_session_data(session_id)
}
full_tibble <- do.call(rbind, session_list)
full_tibble$success <- full_tibble$feedback_type == 1
full_tibble$success <- as.numeric(full_tibble$success)
full_tibble$contrast_diff <- abs(full_tibble$contrast_left-full_tibble$contrast_right)

binename <- paste0("bin", as.character(1:40))

get_trail_functional_data <- function(session_id, trail_id){
  spikes <- session[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }
  trail_bin_average <- matrix(colMeans(spikes), nrow = 1)
  colnames(trail_bin_average) <- binename
  trail_tibble  = as_tibble(trail_bin_average)%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= session[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= session[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= session[[session_id]]$feedback_type[trail_id])
  
  trail_tibble
}
get_session_functional_data <- function(session_id){
  n_trail <- length(session[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trail_functional_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- as_tibble(do.call(rbind, trail_list))
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

session_list = list()
for (session_id in 1: 1){
  session_list[[session_id]] <- get_session_functional_data(session_id)
}
full_functional_tibble <- as_tibble(do.call(rbind, session_list))
full_functional_tibble$session_id <- as.factor(full_functional_tibble$session_id )
full_functional_tibble$contrast_diff <- abs(full_functional_tibble$contrast_left-full_functional_tibble$contrast_right)

full_functional_tibble$success <- full_functional_tibble$feedback_type == 1
full_functional_tibble$success <- as.numeric(full_functional_tibble$success)


predictive_feature <- c("session_id","trail_id","contrast_right",
                        "contrast_left",
                        "contrast_diff" ,binename)
predictive_test1 <- full_functional_tibble[predictive_feature]

Xtest1 <- model.matrix(~., predictive_test1  |> 
                      select(-session_id,-trail_id))
predtest1 <- predict(xgb.fit,Xtest1 )
predclasstest1 <- ifelse(predtest1 > 0.5,1,0)
predclasstest1
```

# Predict2 test2

```{r}
library(tidyverse)
library(caret) 
library(xgboost)
library(pROC)
session=list()
for(i in 1){
  session[[i]]=readRDS("./test/test2.rds")
  print(session[[i]]$mouse_name)
  
  print(session[[i]]$date_exp)
  
}
get_trail_data <- function(session_id, trail_id){
  spikes <- session[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }
  trail_tibble <- tibble("neuron_spike" = rowSums(spikes))  %>%  
    add_column("brain_area" = session[[session_id]]$brain_area ) %>% 
    group_by(brain_area) %>% summarize( region_sum_spike = sum(neuron_spike),
                                        region_count = n(),region_mean_spike = mean(neuron_spike)) 
  trail_tibble  = trail_tibble%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= session[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= session[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= session[[session_id]]$feedback_type[trail_id])
  trail_tibble
}

get_session_data <- function(session_id){
  n_trail <- length(session[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trail_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- do.call(rbind, trail_list)
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

session_1 <- get_session_data(1)

session_list = list()
for (session_id in 1:1){
  session_list[[session_id]] <- get_session_data(session_id)
}
full_tibble <- do.call(rbind, session_list)
full_tibble$success <- full_tibble$feedback_type == 1
full_tibble$success <- as.numeric(full_tibble$success)
full_tibble$contrast_diff <- abs(full_tibble$contrast_left-full_tibble$contrast_right)

binename <- paste0("bin", as.character(1:40))

get_trail_functional_data <- function(session_id, trail_id){
  spikes <- session[[session_id]]$spks[[trail_id]]
  if (any(is.na(spikes))){
    disp("value missing")
  }
  trail_bin_average <- matrix(colMeans(spikes), nrow = 1)
  colnames(trail_bin_average) <- binename
  trail_tibble  = as_tibble(trail_bin_average)%>% add_column("trail_id" = trail_id) %>% add_column("contrast_left"= session[[session_id]]$contrast_left[trail_id]) %>% add_column("contrast_right"= session[[session_id]]$contrast_right[trail_id]) %>% add_column("feedback_type"= session[[session_id]]$feedback_type[trail_id])
  
  trail_tibble
}
get_session_functional_data <- function(session_id){
  n_trail <- length(session[[session_id]]$spks)
  trail_list <- list()
  for (trail_id in 1:n_trail){
    trail_tibble <- get_trail_functional_data(session_id,trail_id)
    trail_list[[trail_id]] <- trail_tibble
  }
  session_tibble <- as_tibble(do.call(rbind, trail_list))
  session_tibble <- session_tibble %>% add_column("mouse_name" = session[[session_id]]$mouse_name) %>% add_column("date_exp" = session[[session_id]]$date_exp) %>% add_column("session_id" = session_id) 
  session_tibble
}

session_list = list()
for (session_id in 1: 1){
  session_list[[session_id]] <- get_session_functional_data(session_id)
}
full_functional_tibble <- as_tibble(do.call(rbind, session_list))
full_functional_tibble$session_id <- as.factor(full_functional_tibble$session_id )
full_functional_tibble$contrast_diff <- abs(full_functional_tibble$contrast_left-full_functional_tibble$contrast_right)
predictive_feature <- c("session_id","trail_id","contrast_right",
                        "contrast_left",
                        "contrast_diff" ,binename)
predictive_test2 <- full_functional_tibble[predictive_feature]
Xtest2 <- model.matrix(~., predictive_test2  |> 
                         select(-session_id,-trail_id))
predtest2 <- predict(xgb.fit,Xtest2 )
predclasstest2 <- ifelse(predtest2 > 0.5,1,0)
predclasstest2
```
# Reference {-}

Course_project_demo

Steinmetz, N.A., Zatka-Haas, P., Carandini, M. et al. Distributed coding of choice, action and engagement across the mouse brain. Nature 576, 266–273 (2019). https://doi.org/10.1038/s41586-019-1787-x

Chen, T., & Guestrin, C. (2016). XGBoost: A Scalable Tree Boosting System. Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining.

Allaire, JJ, Yihui Xie, Jonathan McPherson, Javier Luraschi, Kevin Ushey, Aron Atkins, Hadley Wickham, Joe Cheng, Winston Chang, and Richard Iannone. 2018. rmarkdown: Dynamic Documents for R. https://rmarkdown.rstudio.com.
