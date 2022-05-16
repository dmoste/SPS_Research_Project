library(tidyverse)
library(inspectdf)
library(visdat)
library(mice)
library(ggplot2)
library(caret)
library(Hmisc)
library(plyr)
library(tidymodels)
library(parsnip)
library(workflows)
library(recipes)
library(kknn)
library(LiblineaR)
library(kernlab)
library(xgboost)

# Read in data
all_students <- read.csv("https://raw.githubusercontent.com/dmoste/SPS_Research_Project/main/studentData.csv",
                         header = TRUE,
                         na.strings = c("",NA))

inspectdf::inspect_types(all_students) %>%
  show_plot

all_students <- all_students %>% 
  mutate_if(is.character, as.factor)

# Remove extraneous variables
df <- all_students %>%
  filter(Grade_12_Year < 2022) %>%
  select(-c(Current_Grade, PGP:World_History._Modern,
            Math_8_Course:English_12_Course,
            Math_8_Final:English_Average, S_AppliedPhysics,
            S_APChemistry, S_APPhysics, S_APBiology,
            M_APCalculusBC, M_APCalculusAB))

df$ZIP <- as.factor(df$ZIP)

inspectdf::inspect_num(df %>% select(-Grade_12_Year)) %>% 
  show_plot()
inspectdf::inspect_cat(df) %>% 
  show_plot()

# Fix missing data
df <- df[1:1200,]

numericalELA <- df %>%
  select_if(is.numeric) %>%
  dplyr::select(starts_with('E'))

numericalMath <- df %>%
  select_if(is.numeric) %>%
  dplyr::select(starts_with('M'))

numericalSci <- df %>%
  select_if(is.numeric) %>%
  dplyr::select(starts_with('S'))

visdat::vis_miss(df, sort_miss = TRUE)

moused <- mice::mice(data = df,
                     m = 2,
                     method = "midastouch",
                     seed = 500)
df <- mice::complete(moused, 1)

numericalELAImputed <- df %>%
  select_if(is.numeric) %>%
  dplyr::select(starts_with('E'))

numericalMathImputed <- df %>%
  select_if(is.numeric) %>%
  dplyr::select(starts_with('M'))

numericalSciImputed <- df %>%
  select_if(is.numeric) %>%
  dplyr::select(starts_with('S'))

inspectdf::inspect_num(numericalELA, numericalELAImputed) %>% show_plot
inspectdf::inspect_num(numericalMath, numericalMathImputed) %>% show_plot
inspectdf::inspect_num(numericalSci, numericalSciImputed) %>% show_plot

visdat::vis_miss(df, sort_miss = TRUE)
df <- df[complete.cases(df), ]

inspectdf::inspect_num(df %>% select(-Grade_12_Year)) %>% 
  show_plot()

# Correct non-stationary time series data
byGradYear <- df %>%
  group_by(Grade_12_Year) %>%
  select(c(Grade_12_Year,M_Math6:E_APLiterature)) %>%
  summarise_all(mean) %>%
  pivot_longer(c("M_Math6":"E_APLiterature"),
               names_to = "Course",
               values_to = "mean") %>%
  filter(Grade_12_Year < 2021) %>%
  filter(Grade_12_Year > 2006) %>%
  mutate(Subject = rep(c(rep("Math", 7),
                         rep("Science", 7),
                         rep("English", 9)),14))

ggplot(data = byGradYear,
       aes(x = Grade_12_Year,
           y = mean,
           color = Course)) +
  geom_point(size = 2) + 
  geom_line(size = 2) +
  facet_wrap(~Subject) +
  labs(title = "Mean over Time",
       x = "Year",
       y = "Mean Score") +
  scale_color_manual(values = c("#CC6666", "#3333CC", "#66CC99", "#FF6699", "#000000",
                                "#CCFFFF", "#FFFF00", "#99FF33", "#CCCC33", "#CC6666",
                                "#3333CC", "#66CC99", "#FF6699", "#000000", "#CCFFFF",
                                "#FFFF00", "#CC6666", "#3333CC", "#66CC99", "#FF6699",
                                "#000000", "#CCFFFF", "#FFFF00", "#CC6666", "#3333CC"))

getForecasts <- function(x){
  subjectTrend <- byGradYear %>%
    filter(Course == x)
  fc <- lm(mean~Grade_12_Year, data = subjectTrend)
  slope <- as.numeric(fc$coefficients[2])
  newCol <- paste(x, "Stat", sep = "_")
  df[[newCol]] <<- df[[x]]-((df$Grade_12_Year-2006)*slope)
}
y <- lapply(byGradYear$Course[1:23], getForecasts)

byGradYearStat <- df %>%
  group_by(Grade_12_Year) %>%
  select(c(Grade_12_Year,M_Math6_Stat:E_APLiterature_Stat)) %>%
  summarise_all(mean) %>%
  pivot_longer(c("M_Math6_Stat":"E_APLiterature_Stat"),
               names_to = "Course",
               values_to = "mean") %>%
  filter(Grade_12_Year < 2021) %>%
  filter(Grade_12_Year > 2006) %>%
  mutate(Subject = rep(c(rep("Math", 7),
                         rep("Science", 7),
                         rep("English", 9)),14))

ggplot(data = byGradYearStat,
       aes(x = Grade_12_Year,
           y = mean,
           color = Course)) +
  geom_point(size = 2) + 
  geom_line(size = 2) +
  facet_wrap(~Subject) +
  labs(title = "Mean over Time",
       x = "Year",
       y = "Mean Score") +
  scale_color_manual(values = c("#CC6666", "#3333CC", "#66CC99", "#FF6699", "#000000",
                                "#CCFFFF", "#FFFF00", "#99FF33", "#CCCC33", "#CC6666",
                                "#3333CC", "#66CC99", "#FF6699", "#000000", "#CCFFFF",
                                "#FFFF00", "#CC6666", "#3333CC", "#66CC99", "#FF6699",
                                "#000000", "#CCFFFF", "#FFFF00", "#CC6666", "#3333CC"))

binnedDF <- df %>%
  mutate_if(is.numeric, round_any, accuracy = 10, floor) %>%
  mutate_if(is.numeric, mapvalues,
            from = c(-30, -20, -10, 0, 10, 20, 30, 40 ,50 ,60 ,70, 80, 90, 100),
            to = c("F", "F", "F", "F", "F", "F", "F", "F", "F", "D", "C", "B", "A", "A")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.numeric)

hist.data.frame(binnedDF)

# Setup course structure
courses <- c("M_Math6_Stat", "M_Math7_Stat", "M_Math8_Stat",
             "M_Algebra1_Stat", "M_Geometry_Stat", "M_Algebra2_Stat",
             "M_Claculus_Stat", "S_Science6_Stat", "S_Science7_Stat",
             "S_Science8_Stat", "S_EarthScience_Stat", "S_Biology_Stat",
             "S_Chemistry_Stat", "S_Physics_Stat", "E_ELA6_Stat", "E_ELA7_Stat",
             "E_ELA8_Stat", "E_ELA9_Stat", "E_ELA10_Stat", "E_ELA11_Stat",
             "E_ELA12_Stat", "E_APLanguage_Stat", "E_APLiterature_Stat")
levels <- c(6, 7, 8, 9, 10, 11,12, 6, 7, 8, 9, 10, 11, 12, 6, 7, 8, 9, 10, 11,12,
            11, 12)
courseGradeLevel <- data.frame(courses, levels)

# Build models
preprocessDF <- function(dFrame, course, method){
  features <- dFrame %>%
    select(-c(M_Math6:E_APLiterature)) %>%
    select(-Grade_12_Year) %>%
    select(-course)
  
  predicitveCourses <- courseGradeLevel %>%
    filter(levels < courseGradeLevel[which(courseGradeLevel == course, arr.ind = T)[1],]$levels)
  predictive <- features %>%
    select(c(Gender, Race.Hispanic, Language, Counselor, City, State, ZIP,
             HH_Language, Translation_Req, predicitveCourses$courses))
  
  processedDF <- preProcess(predictive, method = c("center", "scale", "BoxCox"))
  processedDF <- predict(processedDF, predictive)
  processedDF <- cbind(processedDF, dFrame[course])
  names(processedDF)[ncol(processedDF)] <- 'target'
  
  if(method == 'regression'){
    processedDF <- processedDF %>%
      mutate_if(is.factor, as.integer) %>%
      mutate_if(is.integer, as.numeric)
  }
  
  return(processedDF)
}
createSplits <- function(processedDF){
  set.seed(54321)
  splits <- processedDF %>%
    initial_split(strata = target)
  
  train <- training(splits)
  test <- testing(splits)
  
  set.seed(12345)
  val_set <- validation_split(train, 
                              strata = target, 
                              prop = 0.80)
  
  return(list('splits' = splits,
              'train' = train,
              'test' = test,
              'val_set' = val_set))
}

buildKNNModel <- function(splits, train, test, val_set, method){
  if(method == 'regression'){
    metrics <- metric_set(rmse)
    best_metric <- 'rmse'
    knn_rec <- recipe(target ~., data = train) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  } else{
    metrics <- metric_set(accuracy)
    best_metric <- 'accuracy'
    knn_rec <- recipe(target ~., data = train) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  }
  
  knn_mod <- nearest_neighbor(mode = method,
                              engine = "kknn",
                              neighbors = tune(),
                              weight_func = tune(),
                              dist_power = tune())
  
  knn_workflow <- workflow() %>%
    add_model(knn_mod) %>%
    add_recipe(knn_rec)

  knn_res <- knn_workflow %>%
    tune_grid(val_set,
              grid = 30,
              control = control_grid(save_pred = TRUE),
              metrics = metrics)
  
  knn_best <- knn_res %>% 
    select_best(metric = best_metric)

  n <- knn_best$neighbors
  w <- knn_best$weight_func
  d <- knn_best$dist_power
  
  last_knn_mod <- nearest_neighbor(neighbors = n,
                                   weight_func = w,
                                   dist_power = d) %>%
    set_engine('kknn',
               num.threads = parallel::detectCores(),
               importance = 'impurity') %>%
    set_mode(method)
  
  last_knn_workflow <- knn_workflow %>% 
    update_model(last_knn_mod)
  
  set.seed(345)
  last_knn_fit <- last_knn_workflow %>% 
    last_fit(splits)
  
  knn_model <- last_knn_fit$.workflow[[1]]
  
  if(method == 'regression'){
    knn_pred <- predict(knn_model, test) %>%
      dplyr::rename(preds = .pred)
    
    knn_results <- lm(test$target ~ knn_pred$preds)
    
    return(list('rSquared' = summary(knn_results)$adj.r.squared,
                'preds' = knn_pred$preds))
  } else{
    knn_pred <- predict(knn_model, test)
    cM <- confusionMatrix(as.factor(test$target), as.factor(knn_pred$.pred_class))
    
    return(list('accuracy' = cM$overall[[1]],
                'preds' = knn_pred$.pred_class))
  }
}
buildRFModel <- function(splits, train, test, val_set, method){
  if(method == 'regression'){
    metrics <- metric_set(rmse)
    best_metric <- 'rmse'
    rf_rec <- recipe(target ~., data = train) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  } else{
    metrics <- metric_set(accuracy)
    best_metric <- 'accuracy'
    rf_rec <- recipe(target ~., data = train) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  }
  
  rf_mod <- rand_forest(mode = method,
                        engine = "ranger",
                        mtry = tune(),
                        min_n = tune(),
                        trees = tune())
  
  rf_workflow <- workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(rf_rec)
  
  rf_res <- rf_workflow %>%
    tune_grid(val_set,
              grid = 30,
              control = control_grid(save_pred = TRUE),
              metrics = metrics)
  
  rf_best <- rf_res %>% 
    select_best(metric = best_metric)
  mt <- rf_best$mtry
  m <- rf_best$min_n
  t <- rf_best$trees
  
  last_rf_mod <- rand_forest(mtry = mt,
                             min_n = m,
                             trees = t) %>%
    set_engine('ranger',
               num.threads = parallel::detectCores(),
               importance = 'impurity') %>%
    set_mode(method)
  
  last_rf_workflow <- rf_workflow %>% 
    update_model(last_rf_mod)
  
  set.seed(345)
  last_rf_fit <- last_rf_workflow %>% 
    last_fit(splits)
  
  rf_model <- last_rf_fit$.workflow[[1]]
  
  if(method == 'regression'){
    rf_pred <- predict(rf_model, test) %>%
      dplyr::rename(preds = .pred)
    
    rf_results <- lm(test$target ~ rf_pred$preds)
    
    return(list('rSquared' = summary(rf_results)$adj.r.squared,
                'preds' = rf_pred$preds))
  } else{
    rf_pred <- predict(rf_model, test)
    cM <- confusionMatrix(as.factor(test$target), as.factor(rf_pred$.pred_class))
    
    return(list('accuracy' = cM$overall[[1]],
                'preds' = rf_pred$.pred_class))
  }
}
buildLSVMModel <- function(splits, train, test, val_set, method){
  if(method == 'regression'){
    metrics <- metric_set(rmse)
    best_metric <- 'rmse'
    lsvm_rec <- recipe(target ~., data = train) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  } else{
    metrics <- metric_set(accuracy)
    best_metric <- 'accuracy'
    lsvm_rec <- recipe(target ~., data = train) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  }
  
  lsvm_mod <- svm_linear(mode = method,
                         engine = "LiblineaR",
                         cost = tune(),
                         margin = tune())
  
  lsvm_workflow <- workflow() %>%
    add_model(lsvm_mod) %>%
    add_recipe(lsvm_rec)
  
  lsvm_res <- lsvm_workflow %>%
    tune_grid(val_set,
              grid = 30,
              control = control_grid(save_pred = TRUE),
              metrics = metrics)
  
  lsvm_best <- lsvm_res %>% 
    select_best(metric = best_metric)
  c <- lsvm_best$cost
  m <- lsvm_best$margin
  
  last_lsvm_mod <- svm_linear(cost = c,
                              margin = m) %>%
    set_engine('LiblineaR',
               num.threads = parallel::detectCores(),
               importance = 'impurity') %>%
    set_mode(method)
  
  last_lsvm_workflow <- lsvm_workflow %>% 
    update_model(last_lsvm_mod)
  
  set.seed(345)
  last_lsvm_fit <- last_lsvm_workflow %>% 
    last_fit(splits)
  
  lsvm_model <- last_lsvm_fit$.workflow[[1]]

  if(method == 'regression'){
    lsvm_pred <- predict(lsvm_model, test) %>%
      dplyr::rename(preds = .pred)
    
    lsvm_results <- lm(test$target ~ lsvm_pred$preds)
    
    return(list('rSquared' = summary(lsvm_results)$adj.r.squared,
                'preds' = lsvm_pred$preds))
  } else{
    lsvm_pred <- predict(lsvm_model, test)
    cM <- confusionMatrix(as.factor(test$target), as.factor(lsvm_pred$.pred_class))
    
    return(list('accuracy' = cM$overall[[1]],
                'preds' = lsvm_pred$.pred_class))
  }
}
buildPSVMModel <- function(splits, train, test, val_set, method){
  if(method == 'regression'){
    metrics <- metric_set(rmse)
    best_metric <- 'rmse'
    psvm_rec <- recipe(target ~., data = train) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  } else{
    metrics <- metric_set(accuracy)
    best_metric <- 'accuracy'
    psvm_rec <- recipe(target ~., data = train) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  }
  
  psvm_mod <- svm_poly(mode = method,
                       engine = "kernlab",
                       cost = tune(),
                       degree = tune(),
                       scale_factor = tune(),
                       margin = tune())
  
  psvm_workflow <- workflow() %>%
    add_model(psvm_mod) %>%
    add_recipe(psvm_rec)
  
  psvm_res <- psvm_workflow %>%
    tune_grid(val_set,
              grid = 30,
              control = control_grid(save_pred = TRUE),
              metrics = metrics)
  
  psvm_best <- psvm_res %>% 
    select_best(metric = best_metric)
  c <- psvm_best$cost
  d <- psvm_best$degree
  sf <- psvm_best$scale_factor
  m <- psvm_best$margin
  
  last_psvm_mod <- svm_poly(cost = c,
                            degree = d,
                            scale_factor = sf,
                            margin = m) %>%
    set_engine('kernlab',
               num.threads = parallel::detectCores(),
               importance = 'impurity') %>%
    set_mode(method)
  
  last_psvm_workflow <- psvm_workflow %>% 
    update_model(last_psvm_mod)
  
  set.seed(345)
  last_psvm_fit <- last_psvm_workflow %>% 
    last_fit(splits)
  
  psvm_model <- last_psvm_fit$.workflow[[1]]

  if(method == 'regression'){
    psvm_pred <- predict(psvm_model, test) %>%
      dplyr::rename(preds = .pred)
    
    psvm_results <- lm(test$target ~ psvm_pred$preds)
    
    return(list('rSquared' = summary(psvm_results)$adj.r.squared,
                'preds' = psvm_pred$preds))
  } else{
    psvm_pred <- predict(psvm_model, test)
    cM <- confusionMatrix(as.factor(test$target), as.factor(psvm_pred$.pred_class))
    
    return(list('accuracy' = cM$overall[[1]],
                'preds' = psvm_pred$.pred_class))
  }
}
buildGBModel <- function(splits, train, test, val_set, method){
  if(method == 'regression'){
    metrics <- metric_set(rmse)
    best_metric <- 'rmse'
    gb_rec <- recipe(target ~., data = train) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  } else{
    metrics <- metric_set(accuracy)
    best_metric <- 'accuracy'
    gb_rec <- recipe(target ~., data = train) %>%
      step_dummy(all_nominal_predictors()) %>%
      step_zv(all_predictors()) %>%
      step_normalize(all_predictors())
  }
  
  gb_mod <- boost_tree(mode = method,
                       engine = 'xgboost',
                       mtry = tune(),
                       trees = tune(),
                       min_n = tune(),
                       tree_depth = tune(),
                       learn_rate = tune(),
                       loss_reduction = tune(),
                       sample_size = tune(),
                       stop_iter = tune())
  
  gb_workflow <- workflow() %>%
    add_model(gb_mod) %>%
    add_recipe(gb_rec)
  
  gb_res <- gb_workflow %>%
    tune_grid(val_set,
              grid = 30,
              control = control_grid(save_pred = TRUE),
              metrics = metrics)
  
  gb_best <- gb_res %>% 
    select_best(metric = best_metric)
  mt <- gb_best$mtry
  t <- gb_best$trees
  m <- gb_best$min_n
  td <- gb_best$tree_depth
  le <- gb_best$learn_rate
  lo <- gb_best$loss_reduction
  ss <- gb_best$sample_size
  si <- gb_best$stop_iter
  last_gb_mod <- boost_tree(mtry = mt,
                            trees = t,
                            min_n = m,
                            tree_depth = td,
                            learn_rate = le,
                            loss_reduction = lo,
                            sample_size = ss,
                            stop_iter = si) %>%
    set_engine('xgboost',
               num.threads = parallel::detectCores(),
               importance = 'impurity') %>%
    set_mode(method)
  
  last_gb_workflow <- gb_workflow %>% 
    update_model(last_gb_mod)
  
  set.seed(345)
  last_gb_fit <- last_gb_workflow %>% 
    last_fit(splits)
  
  gb_model <- last_gb_fit$.workflow[[1]]

  if(method == 'regression'){
    gb_pred <- predict(gb_model, test) %>%
      dplyr::rename(preds = .pred)
    
    gb_results <- lm(test$target ~ gb_pred$preds)
    
    return(list('rSquared' = summary(gb_results)$adj.r.squared,
                'preds' = gb_pred$preds))
  } else{
    gb_pred <- predict(gb_model, test)
    cM <- confusionMatrix(as.factor(test$target), as.factor(gb_pred$.pred_class))
    
    return(list('accuracy' = cM$overall[[1]],
                'preds' = gb_pred$.pred_class))
  }
}

# Build containers for model results
testCourses <- c("M_Algebra1_Stat", "M_Geometry_Stat", "M_Algebra2_Stat",
                 "S_EarthScience_Stat", "S_Biology_Stat", "S_Chemistry_Stat",
                 "E_ELA9_Stat", "E_ELA10_Stat", "E_ELA11_Stat")

reg_results <- data.frame(matrix(nrow = 5, ncol = 9))
class_results <- data.frame(matrix(nrow = 5, ncol = 9))
colnames(reg_results) <- testCourses
colnames(class_results) <- testCourses

reg_preds <- data.frame(matrix(nrow = 298, ncol = 6))
colnames(reg_preds) <- c('actual', 'knn', 'rf', 'lsvm', 'psvm', 'gb')

# Generate models
for(i in 1:length(testCourses)){
  course <- testCourses[i]
  print(course)
  processedDF <- preprocessDF(df, course, 'regression')
  
  allSplits <- createSplits(processedDF)
  splits <- allSplits$splits
  train <- allSplits$train
  test <- allSplits$test
  val_set <- allSplits$val_set
  
  print('knn')
  knn <- buildKNNModel(splits, train, test, val_set, 'regression')
  print('rf')
  rf <- buildRFModel(splits, train, test, val_set, 'regression')
  print('lsvm')
  lsvm <- buildLSVMModel(splits, train, test, val_set, 'regression')
  print('psvm')
  psvm <- buildPSVMModel(splits, train, test, val_set, 'regression')
  print('gb')
  gb <- buildGBModel(splits, train, test, val_set, 'regression')
  
  reg_results[1,i] <- knn$rSquared
  reg_results[2,i] <- rf$rSquared
  reg_results[3,i] <- lsvm$rSquared
  reg_results[4,i] <- psvm$rSquared
  reg_results[5,i] <- gb$rSquared
  
  if(course == "S_Biology_Stat"){
    reg_preds[,1] <- test$target
    reg_preds[,2] <- knn$preds
    reg_preds[,3] <- rf$preds
    reg_preds[,4] <- lsvm$preds
    reg_preds[,5] <- psvm$preds
    reg_preds[,6] <- gb$preds
  }
}
for(i in 1:length(testCourses)){
  course <- testCourses[i]
  print(course)
  processedDF <- preprocessDF(binnedDF, course, 'classification')
  
  allSplits <- createSplits(processedDF)
  splits <- allSplits$splits
  train <- allSplits$train
  test <- allSplits$test
  val_set <- allSplits$val_set
  
  print('knn')
  knn <- buildKNNModel(splits, train, test, val_set, 'classification')
  print('rf')
  rf <- buildRFModel(splits, train, test, val_set, 'classification')
  print('lsvm')
  lsvm <- buildLSVMModel(splits, train, test, val_set, 'classification')
  print('psvm')
  psvm <- buildPSVMModel(splits, train, test, val_set, 'classification')
  print('gb')
  gb <- buildGBModel(splits, train, test, val_set, 'classification')
  
  class_results[1,i] <- knn$accuracy
  class_results[2,i] <- rf$accuracy
  class_results[3,i] <- lsvm$accuracy
  class_results[4,i] <- psvm$accuracy
  class_results[5,i] <- gb$accuracy
}

combined_results <- lm(actual ~., data = reg_preds)
summary(combined_results)
plot(reg_preds$actual, combined_results$fitted.values)
