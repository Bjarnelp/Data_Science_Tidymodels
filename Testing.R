require(tidyverse)
require(knitr)
require(REdaS)
require(glue)
require(tidymodels)
require(glmnet)
require(LiblineaR)
require(kernlab)
require(kknn)
require(bestNormalize)
require(finetune)
require(moments)
require(ggcorrplot)
require(corrr)
require(workflowsets)
require(treesnip)
require(ranger)
require(baguette)
require(xgboost)
require(lightgbm)
require(lme4)
require(parallel)
require(doParallel)
require(stringr)

all_cores <- parallel::detectCores(logical = FALSE)

cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

set.seed(123)

train <- read_csv("./titanic_train.csv")
test <- read_csv("./titanic_test.csv")

train <- train %>% 
  mutate(Ticket = NULL) %>% 
  mutate(Cabin = NULL) %>% 
  mutate(Survived = as_factor(Survived)) %>% 
  mutate(Pclass = as_factor(Pclass)) %>%
  mutate(Family = case_when((SibSp == 0 & Parch == 0) ~ "Single",
                            (SibSp == 1 & Parch == 0) ~ "Couple",
                            between((SibSp + Parch),1,4) ~ "Small",
                            SibSp+Parch > 4 ~"Large")) %>% 
  mutate(Title = case_when(
    str_detect(Name,coll("Mr.")) ~ "Mr",
    str_detect(Name,coll("Mrs.")) ~ "Mrs",
    str_detect(Name,coll("Ms.")) ~ "Miss",
    str_detect(Name,coll("Miss.")) ~ "Miss",
    str_detect(Name,coll("Master.")) ~ "Master",
    str_detect(Name,coll("Dr.")) ~ "Doktor",
    str_detect(Name,coll("Rev.")) ~ "Mr",
    str_detect(Name,coll("Mlle.")) ~ "Miss",
    str_detect(Name,coll("Mme.")) ~ "Mrs",
    str_detect(Name,coll("Sir.")) ~ "Noble",
    str_detect(Name,coll("Countess.")) ~ "Noble",
    str_detect(Name,coll("Lady.")) ~ "Noble",
    str_detect(Name,coll("Jonkheer.")) ~ "Noble",
    str_detect(Name,coll("Don.")) ~ "Noble",
    str_detect(Name,coll("Dona.")) ~ "Noble",
    str_detect(Name,coll("Col.")) ~ "Military",
    str_detect(Name,coll("Capt.")) ~ "Military",
    str_detect(Name,coll("Major.")) ~ "Military",
  )) %>% 
  mutate(FamilyNameLoc = str_locate(Name,",")[,1]) %>% 
  mutate(FamilyName = str_sub(Name,1,FamilyNameLoc-1)) %>% 
  mutate(Name=NULL) %>% 
  mutate(FamilyNameLoc = NULL)

test <- test %>% 
  mutate(Ticket = NULL) %>%                                             #Remove ticket number
  mutate(Cabin = NULL) %>%                                              #Remove cabin number
  mutate(Pclass = as_factor(Pclass)) %>%
  mutate(Family = case_when((SibSp == 0 & Parch == 0) ~ "Single",
                            (SibSp == 1 & Parch == 0) ~ "Couple",
                            between((SibSp + Parch),1,4) ~ "Small",
                            SibSp+Parch > 4 ~"Large")) %>% 
  mutate(Title = case_when(
    str_detect(Name,coll("Mr.")) ~ "Mr",
    str_detect(Name,coll("Mrs.")) ~ "Mrs",
    str_detect(Name,coll("Ms.")) ~ "Miss",
    str_detect(Name,coll("Miss.")) ~ "Miss",
    str_detect(Name,coll("Master.")) ~ "Master",
    str_detect(Name,coll("Dr.")) ~ "Doktor",
    str_detect(Name,coll("Rev.")) ~ "Mr",
    str_detect(Name,coll("Mlle.")) ~ "Miss",
    str_detect(Name,coll("Mme.")) ~ "Mrs",
    str_detect(Name,coll("Sir.")) ~ "Noble",
    str_detect(Name,coll("Countess.")) ~ "Noble",
    str_detect(Name,coll("Lady.")) ~ "Noble",
    str_detect(Name,coll("Jonkheer.")) ~ "Noble",
    str_detect(Name,coll("Don.")) ~ "Noble",
    str_detect(Name,coll("Dona.")) ~ "Noble",
    str_detect(Name,coll("Col.")) ~ "Military",
    str_detect(Name,coll("Capt.")) ~ "Military",
    str_detect(Name,coll("Major.")) ~ "Military",
  )) %>% 
  mutate(FamilyNameLoc = str_locate(Name,",")[,1]) %>% 
  mutate(FamilyName = str_sub(Name,1,FamilyNameLoc-1)) %>% 
  mutate(Name=NULL) %>% 
  mutate(FamilyNameLoc = NULL)

Impute_table <- train %>% 
  group_by(Title) %>% 
  summarise(mean_age=mean(Age,na.rm=T),mean_fare=mean(Fare,na.rm=T))

train <- left_join(train,Impute_table,by=c("Title")) %>% 
  mutate(Age = if_else(is.na(Age),mean_age,Age)) %>%
  mutate(Fare = if_else(is.na(Fare),mean_fare,Fare)) %>% 
  mutate(mean_age = NULL) %>% 
  mutate(mean_fare = NULL)

test <- left_join(test,Impute_table,by=c("Title")) %>% 
  mutate(Age = if_else(is.na(Age),mean_age,Age)) %>%
  mutate(Fare = if_else(is.na(Fare),mean_fare,Fare)) %>% 
  mutate(mean_age = NULL) %>% 
  mutate(mean_fare = NULL)
  
  
  left_join(test,Agetable,by=c("Title")) %>% 
  mutate(Age = if_else(is.na(Age),mean,Age)) %>% 
  mutate(mean = NULL) 

train %>% 
  select(which(colSums(is.na(.))>0)) %>% 
  summarise_all(~ sum(is.na(.)))

test %>% 
  select(which(colSums(is.na(.))>0)) %>% 
  summarise_all(~ sum(is.na(.)))

rec <- 
  recipe(Survived ~ ., data=train) %>% 
  update_role(PassengerId,new_role="ID") %>% 
  step_impute_mode(Embarked)

folds <- vfold_cv(train,strata = Survived)

model <-
  decision_tree(min_n=tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

bagged_model <-
  bag_tree(min_n=tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

workflowset <- 
  workflow_set(
    preproc = list(age = rec),
    models = list(c50=model,bag=bagged_model))

c50_control <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

c50_results <- 
  workflowset %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    grid = 10,
    control = c50_control, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

autoplot(c50_results,rank_metric="accuracy") +
  geom_text(aes(y=0.55,label= wflow_id), angle=90) +
  lims(y=c(0.3,0.9)) +
  theme(legend.position = "none")

c50_best_parm <-
  c50_results %>% 
  extract_workflow_set_result("age_bag") %>% 
  select_best(metric="accuracy")
c50_best_parm

c50_wf <- c50_results %>% 
  extract_workflow("age_bag") %>% 
  finalize_workflow(c50_best_parm)

c50_mod_age <- fit(c50_wf,train)

c50_pred_age <- predict(c50_mod_age,new_data=test)

cbind(PassengerId = test$PassengerId,c50_pred_age) %>% 
  rename(Survived = .pred_class) %>% 
  write_csv("./c50_pred_age.csv")

# Med FamilyName og Title giver bagged c5.0 en score på 0.80143 - placering 369.

# RF

rf_model <- rand_forest(trees = tune(), min_n=tune()) %>% 
  set_engine("randomForest") %>% 
  set_mode("classification")

rf_workflowset <- 
  workflow_set(
    preproc = list(rec = imp_rec),
    models = list(rf=imp_rf))

rf_results <- 
  rf_workflowset %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    grid = 25,
    control = imp_control, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

autoplot(rf_results,rank_metric="accuracy") +
  geom_text(aes(y=0.45,label= wflow_id), angle=90) +
  lims(y=c(0.2,0.9)) +
  theme(legend.position = "none")

rf_best <-
  rf_results %>% 
  extract_workflow_set_result("age_rf") %>% 
  select_best(metric="accuracy")
rf_best

rf_wf <- rf_results %>% 
  extract_workflow("age_rf") %>% 
  finalize_workflow(rf_best)

rf_mod <- fit(rf_wf,train)

rf_pred <- predict(rf_mod,new_data=test)

cbind(PassengerId = test$PassengerId,rf_pred) %>% 
  rename(Survived = .pred_class) %>% 
  write_csv("./rf_pred_ac.csv")

lgb_mod <- boost_tree(trees=tune(),min_n=tune(),tree_depth=tune(),
                              learn_rate = tune(), loss_reduction = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("lightgbm")

boost_grid <- grid_regular(trees(),min_n(),tree_depth(),
                                   learn_rate(),loss_reduction(),levels=5) 

titanic_boost_wfs <- 
  workflow_set(
    preproc = list(rec =  imp_rec),
    models = list(lgbm= lgb_mod)
  )

titanic_boost_control <- control_grid(
  save_pred = TRUE,
  parallel_over = "nothing",
  save_workflow = TRUE
)

folds <- vfold_cv(train2,v=10,strata=Survived)

titanic_boost_results <- 
  titanic_boost_wfs %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    grid = boost_grid,
    control = titanic_boost_control,
    verbose = TRUE,
    objective = "binary:logistic",
    eval_metric = "logloss"
  )

imp_control <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

imp_results <- 
  imp_workflowset2 %>% 
  workflow_map(
    seed = 1234,
    resamples = imp_folds,
    grid = 25,
    control = imp_control, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )
all_cores <- parallel::detectCores(logical = FALSE)

cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)
require(parallel)
require(doParallel)
