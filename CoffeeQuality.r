knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(ggthemes)
library(rpart.plot)

coffee <- read.csv('arabica_data_cleaned.csv', header = T, stringsAsFactors = F)

#Clean Color. Make empty and 'None' consistent.
coffee[which(coffee$Color=='None'),]$Color <- ''
#Make Bluish-Green and Blue-Green consistent.
coffee[which(coffee$Color=='Bluish-Green'),]$Color <- "Blue-Green"

#Clean Harvest.Year
#Remove non numerics
coffee$Harvest.Year <- coffee$Harvest.Year %>% str_replace_all('\\D', '')
#Select last for digits
coffee$Harvest.Year <- coffee$Harvest.Year %>% str_sub(-4, -1)
coffee$Harvest.Year <- coffee$Harvest.Year %>% as.numeric()
#Several from late 2009 early 2010 with odd entries.
coffee[which(coffee$Harvest.Year<2010),]$Harvest.Year <- 2010

#Assume no coffee is growing at the hight of Mt Everest
coffee[which(coffee$altitude_mean_meters>8800),]$altitude_mean_meters <- NA

#Remove three outliers for the sake of presenting readable graphs and 
#more accurate predictions within our models, at the cost of risking 
#less accurate predictions if the models were used against more real-world data.
coffee <- coffee %>% filter(Total.Cup.Points > 65)

#Convert categories to factors for ease of use within training functions.
coffee <- coffee %>% mutate(Variety = as.factor(Variety))
coffee <- coffee %>% mutate(Country.of.Origin = as.factor(Country.of.Origin))
coffee <- coffee %>% mutate(Color = as.factor(Color))
coffee <- coffee %>% mutate(Processing.Method = as.factor(Processing.Method))

#Remove NA values. If we leave them in we will either have to omit them when 
#creating a model and prediction, or we will have to preprocess to fill them in 
#with likely values. For the sake of cleanliness and keeping the dataset more natural, 
#we will omit the NAs at the cost of losing 250 rows.
coffee <- na.omit(coffee)

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = coffee$Total.Cup.Points, times = 1, p = 0.2, list = FALSE)
test_set <- coffee[test_index,]
train_set <- coffee[-test_index,]

levels(train_set$Variety) <- levels(coffee$Variety)
levels(test_set$Variety) <- levels(coffee$Variety)
levels(train_set$Country.of.Origin) <- levels(coffee$Country.of.Origin)
levels(test_set$Country.of.Origin) <- levels(coffee$Country.of.Origin)
levels(train_set$Processing.Method) <- levels(coffee$Processing.Method)
levels(test_set$Processing.Method) <- levels(coffee$Processing.Method)

mu <- mean(coffee$Total.Cup.Points)
mu_hat <- mean(train_set$Total.Cup.Points)

RMSE <- function(true_quality, predicted_quality){
  sqrt(mean((true_quality - predicted_quality)^2, na.rm = T))
}

mean(coffee$Altitude, na.rm = T)
sd(coffee$Altitude, na.rm = T)
cor(coffee$Total.Cup.Points, coffee$Altitude)

mean(coffee$Harvest.Year, na.rm = T)
cor(coffee$Total.Cup.Points, coffee$Harvest.Year, use = 'complete.obs')

#Exploratory Analysis

#About 85% of entries have Total.Cup.Points greater than 80
mean(coffee$Total.Cup.Points > 80)

#Graph of our target variable, which has a fairly normal distribution.
coffee %>% ggplot(aes(Total.Cup.Points)) 
  + geom_histogram(binwidth = 0.5) 
  + theme_economist() 
  + xlab('Total Cup Points') 
  + ylab('Count') 
  + ggtitle('Distribution of Target Variable')

#Predictor Exploration
#Number of Countries
length(unique(coffee$Country.of.Origin))
#Number of ratings by country
coffee %>% group_by(Country.of.Origin) %>% count()

#Number of ratings by harvest year
coffee %>% group_by(Harvest.Year) %>% count()

#Mean altitude
mean(coffee$altitude_mean_meters, na.rm = T)
sd(coffee$altitude_mean_meters, na.rm = T)
cor(coffee$Total.Cup.Points, coffee$altitude_mean_meters, use = 'complete.obs')

mean(coffee$Harvest.Year, na.rm = T)
mean(coffee$Moisture, na.rm = T)
sd(coffee$Moisture, na.rm = T)

mean(coffee$Category.One.Defects==0)
mean(coffee$Category.Two.Defects==0)
cor(coffee$Total.Cup.Points, coffee$Category.One.Defects, use = 'complete.obs')
cor(coffee$Total.Cup.Points, coffee$Category.Two.Defects, use = 'complete.obs')

#Explore Variety
length(unique(coffee$Variety))
coffee %>% group_by(Variety) %>% count()
cor(as.numeric(as.factor(coffee$Variety)), coffee$Total.Cup.Points)

predictors <- c('altitude_mean_meters', 
                'Color', 
                'Country.of.Origin', 
                'Harvest.Year', 
                'Moisture', 
                'Processing.Method', 
                'Variety', 
                'Category.One.Defects', 
                'Category.Two.Defects')

#Print relationship between target and predictors
attach(coffee)
par(mfrow=c(3,3))
plot(altitude_mean_meters, Total.Cup.Points, ylab='Points')
plot(as.factor(coffee$Color), coffee$Total.Cup.Points, xlab='Color', ylab='Points', )
plot(coffee$Country, coffee$Total.Cup.Points, xlab='Country', ylab='Points')
plot(coffee$Harvest.Year, coffee$Total.Cup.Points, xlab='Year', ylab='Points')
plot(coffee$Moisture, coffee$Total.Cup.Points, xlab='Moisture', ylab='Points')
plot(as.factor(coffee$Processing.Method), coffee$Total.Cup.Points, xlab='Process', ylab='Points')
plot(as.factor(coffee$Variety), coffee$Total.Cup.Points, xlab='Variety', ylab='Points')
plot(coffee$Category.One.Defects, coffee$Total.Cup.Points, xlab='Cat One Defects', ylab='Points')
plot(coffee$Category.Two.Defects, coffee$Total.Cup.Points, xlab='Cat Two Defects', ylab='Points')

#Print metrics for numeric predictors
df <- data.frame(row.names = c('Altitude', 'Harvest.Year', 'Moisture', 'Category.One.Defects', 'Category.Two.Defects'))
df[,1] <- c(mean(coffee$altitude_mean_meters, na.rm = T),   
            mean(coffee$Harvest.Year, na.rm = T), 
            mean(coffee$Moisture, na.rm = T), 
            mean(coffee$Category.One.Defects, na.rm = T), 
            mean(coffee$Category.Two.Defects, na.rm = T))
df[,2] <- c(sd(coffee$altitude_mean_meters, na.rm = T), 
            sd(coffee$Harvest.Year, na.rm = T), 
            sd(coffee$Moisture, na.rm = T), 
            sd(coffee$Category.One.Defects, na.rm = T), 
            sd(coffee$Category.Two.Defects, na.rm = T))
df[,3] <- c(cor(coffee$altitude_mean_meters, coffee$Total.Cup.Points, use = 'complete.obs'), 
            cor(coffee$Harvest.Year, coffee$Total.Cup.Points, use = 'complete.obs'),
            cor(coffee$Moisture, coffee$Total.Cup.Points),
            cor(coffee$Category.One.Defects, coffee$Total.Cup.Points, use = 'complete.obs'),
            cor(coffee$Category.Two.Defects, coffee$Total.Cup.Points, use = 'complete.obs'))
colnames(df) <- c('Mean', 'SD', 'Correlation with Target')
knitr::kable(df, digits = 2, caption = 'Metrics for Numeric Predictors')

#Print baseline model
data.frame(Train_Mean=mu_hat, 
           Test_Mean=mean(test_set$Total.Cup.Points), 
           Baseline_RMSE=RMSE(test_set$Total.Cup.Points, mu_hat)) %>% knitr::kable()

#Construct and print results of linear model
model1 <- train(Total.Cup.Points ~ altitude_mean_meters + Color + Country.of.Origin 
                + Harvest.Year + Moisture + Processing.Method + Variety 
                + Category.One.Defects + Category.Two.Defects, 
                data = train_set, method='lm')
model1summary <- summary(model1)
predictions1 <- predict(model1, test_set)
model1rmse <- RMSE(test_set$Total.Cup.Points, predictions1)

data.frame(method = 'Simple LM', 
           RMSE = model1rmse, 
           RSquared = model1summary$r.squared) %>% knitr::kable()

#Tune parameters for a knn model
control <- trainControl(method = "cv", number = 10)
ks <- seq(5,50,2)
knn_accuracy <- map_df(ks, function(k){
  fit_knn <- train(Total.Cup.Points ~ altitude_mean_meters + Color + Country.of.Origin + 
                     Harvest.Year + Moisture + Processing.Method + Variety + 
                     Category.One.Defects + Category.Two.Defects, 
                   data = train_set, 
                   method = 'knn', 
                   na.action = na.omit, 
                   tuneGrid = data.frame(k = k),
                   trControl = control)
  predictions_train <- predict(fit_knn, train_set)
  train_error <- RMSE(test_set$Total.Cup.Points, predictions_train)
  predictions_test <- predict(fit_knn, test_set)
  test_error <- RMSE(test_set$Total.Cup.Points, predictions_test)
  
  tibble(train = train_error, test = test_error)
})

#Print test vs train set rmse
knn_accuracy %>% mutate(k = ks) %>%
  gather(set, knn_accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, knn_accuracy, color = set)) + 
  geom_line() +
  geom_point() +
  theme_economist() +
  ylab('RMSE')

#Construct and print results of knn model
fit_knn <- train(Total.Cup.Points ~ altitude_mean_meters + Color + Country.of.Origin + 
                   Harvest.Year + Moisture + Processing.Method + Variety + 
                   Category.One.Defects + Category.Two.Defects, 
                 data = train_set, 
                 method = 'knn', 
                 na.action = na.omit, 
                 tuneGrid = data.frame(k = ks[which.min(knn_accuracy$train)]))
predict_knn <- predict(fit_knn, test_set)
rmse_knn <- RMSE(test_set$Total.Cup.Points, predict_knn)
data.frame(method='KNN', RMSE=rmse_knn, Best_K=ks[which.min(knn_accuracy$test)]) %>% knitr::kable()

#The long one
#Construct a regularized model with lambdas for each predictor
lambdas <- c(40:60)
regularized_rmses <- sapply(lambdas, function(lambda){
  Altitude_Avg <- train_set %>% 
    group_by(altitude_mean_meters) %>% 
    summarize(b_alt = sum(Total.Cup.Points - mu)/(n()+lambda))
  
  Color_Avg <- train_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    group_by(Color) %>% 
    summarize(b_colr = sum(Total.Cup.Points - mu - b_alt)/(n()+lambda))
  
  Country_Avg <- train_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    left_join(Color_Avg, by="Color") %>%
    group_by(Country.of.Origin) %>% 
    summarize(b_cntry = sum(Total.Cup.Points - mu - b_alt - b_colr)/(n()+lambda))
  
  Year_Avg <- train_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    left_join(Color_Avg, by="Color") %>%
    left_join(Country_Avg, by="Country.of.Origin") %>%
    group_by(Harvest.Year) %>% 
    summarize(b_yr = sum(Total.Cup.Points - mu - b_alt - b_colr - b_cntry)/(n()+lambda))
  
  Moisture_Avg <- train_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    left_join(Color_Avg, by="Color") %>%
    left_join(Country_Avg, by="Country.of.Origin") %>%
    left_join(Year_Avg, by="Harvest.Year") %>%
    group_by(Moisture) %>% 
    summarize(b_mstr = sum(Total.Cup.Points - mu - b_alt - b_colr - b_cntry - b_yr)/(n()+lambda))
  
  Method_Avg <- train_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    left_join(Color_Avg, by="Color") %>%
    left_join(Country_Avg, by="Country.of.Origin") %>%
    left_join(Year_Avg, by="Harvest.Year") %>%
    left_join(Moisture_Avg, by="Moisture") %>%
    group_by(Processing.Method) %>% 
    summarize(b_mthd = sum(Total.Cup.Points - mu - b_alt - b_colr - b_cntry - b_yr - b_mstr)/(n()+lambda))
  
  Variety_Avg <- train_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    left_join(Color_Avg, by="Color") %>%
    left_join(Country_Avg, by="Country.of.Origin") %>%
    left_join(Year_Avg, by="Harvest.Year") %>%
    left_join(Moisture_Avg, by="Moisture") %>%
    left_join(Method_Avg, by="Processing.Method") %>%
    group_by(Variety) %>% 
    summarize(b_vrty = sum(Total.Cup.Points - mu - b_alt - b_colr - b_cntry - b_yr - b_mstr - b_mthd)/(n()+lambda))
  
  CatOne_Avg <- train_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    left_join(Color_Avg, by="Color") %>%
    left_join(Country_Avg, by="Country.of.Origin") %>%
    left_join(Year_Avg, by="Harvest.Year") %>%
    left_join(Moisture_Avg, by="Moisture") %>%
    left_join(Method_Avg, by="Processing.Method") %>%
    left_join(Variety_Avg, by="Variety") %>%
    group_by(Category.One.Defects) %>% 
    summarize(b_ctone = sum(Total.Cup.Points - mu - b_alt - b_colr - b_cntry - b_yr - b_mstr - b_mthd - b_vrty)/(n()+lambda))
  
  CatTwo_Avg <- train_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    left_join(Color_Avg, by="Color") %>%
    left_join(Country_Avg, by="Country.of.Origin") %>%
    left_join(Year_Avg, by="Harvest.Year") %>%
    left_join(Moisture_Avg, by="Moisture") %>%
    left_join(Method_Avg, by="Processing.Method") %>%
    left_join(Variety_Avg, by="Variety") %>%
    left_join(CatOne_Avg, by="Category.One.Defects") %>%
    group_by(Category.Two.Defects) %>% 
    summarize(b_cttwo = sum(Total.Cup.Points - mu - b_alt - b_colr - b_cntry - b_yr - b_mstr - b_mthd - b_vrty - b_ctone)/(n()+lambda))
  
  predictionsReg <- test_set %>% 
    left_join(Altitude_Avg, by="altitude_mean_meters") %>%
    left_join(Color_Avg, by="Color") %>%
    left_join(Country_Avg, by="Country.of.Origin") %>%
    left_join(Year_Avg, by="Harvest.Year") %>%
    left_join(Moisture_Avg, by="Moisture") %>%
    left_join(Method_Avg, by="Processing.Method") %>%
    left_join(Variety_Avg, by="Variety") %>%
    left_join(CatOne_Avg, by="Category.One.Defects") %>%
    left_join(CatTwo_Avg, by="Category.Two.Defects") %>%
    summarize(pred = mu 
              + b_alt 
              + b_colr 
              + b_cntry 
              + b_yr 
              + b_mstr 
              + b_mthd
              + b_vrty
              + b_ctone
              + b_cttwo
    ) %>%
    .$pred
  
  
  RMSE(test_set$Total.Cup.Points, predictionsReg)
})
data.frame(method="Regularized", 
           RMSE=min(regularized_rmses), 
           Lambda=lambdas[which.min(regularized_rmses)]) %>% knitr::kable()

#Create simple rpart for demonstration purposes
model_simple_rpart <- train(Total.Cup.Points ~ Category.Two.Defects, 
                            data = train_set, 
                            method = 'rpart', 
                            na.action = na.omit, 
                            tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)))
predictions_simple_rpart <- predict(model_simple_rpart, test_set)
model_simple_rpart_rsme <- RMSE(test_set$Total.Cup.Points, predictions_simple_rpart)
data.frame(method = 'Simple RPART', RMSE = model_simple_rpart_rsme, cp=model_simple_rpart$bestTune) %>% knitr::kable()

#And examine it
rpart.plot(model_simple_rpart$finalModel)

#See how our knn predictions align with the data, taking steps
train_set %>% mutate(y_hat = predict(model_simple_rpart$finalModel)) %>% ggplot() 
+ geom_point(aes(Category.Two.Defects, Total.Cup.Points)) 
+ geom_step(aes(Category.Two.Defects, y_hat), col="red") 
+ theme_economist()

#Construct a full knn model with all predictors
#We will have many predictors as the categorical columns will be expanded
model_rpart <- train(Total.Cup.Points ~ altitude_mean_meters 
                     + Color + Country.of.Origin + Harvest.Year + Moisture 
                     + Processing.Method + Variety + Category.One.Defects 
                     + Category.Two.Defects, 
                     data = train_set, 
                     method = 'rpart', 
                     na.action = na.omit,
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)))
predictions_rpart <- predict(model_rpart, test_set)
model_rpart_rsme <- RMSE(test_set$Total.Cup.Points, predictions_rpart)
data.frame(method = 'RPART', RMSE = model_rpart_rsme) %>% knitr::kable()

#And exampine the model visually to see the most important predictors
rpart.plot(model_rpart$finalModel)

#Tune a random forest to find best mtry
control <- trainControl(method="cv", number = 10)
grid <- data.frame(mtry = seq(1,51,5))
fit_rf <- train(Total.Cup.Points ~ altitude_mean_meters + Color + Country.of.Origin + 
                  Harvest.Year + Moisture + Processing.Method + Variety + 
                  Category.One.Defects + Category.Two.Defects, 
                data = train_set, 
                method = 'rf', 
                na.action = na.omit,
                trControl = control,
                ntree = 150,
                tuneGrid = data.frame(mtry = grid$mtry))
ggplot(fit_rf) + theme_economist()

#Construct a random forest with the tuned mtry
model_rf <- train(Total.Cup.Points ~ altitude_mean_meters + Color + Country.of.Origin +
                    Harvest.Year + Moisture + Variety + 
                    Category.One.Defects + Category.Two.Defects, 
                  data = train_set, 
                  method = 'rf', 
                  na.action = na.omit,
                  tuneGrid = data.frame(mtry = fit_rf$bestTune),
                  ntree=500)
predictions_rf <- predict(model_rf, test_set)
model_rf_rsme <- RMSE(test_set$Total.Cup.Points, predictions_rf)
data.frame(method = 'rf', RMSE = model_rf_rsme, mtry=fit_rf$bestTune) %>% knitr::kable()

#Examine how the errors decline and stabilize as the forest grows
plot(model_rf$finalModel)

#Compare models
data.frame(model=c('lm','knn','regularization','rpart','rf'), 
           RMSE=c(model1rmse,rmse_knn,min(regularized_rmses),
                  model_rpart_rsme,model_rf_rsme))
