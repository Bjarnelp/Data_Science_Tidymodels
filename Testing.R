require(knitr)
require(REdaS)
require(glue)
require(glmnet)
require(LiblineaR)
require(kernlab)
require(kknn)
require(bestNormalize)
require(moments)
require(ggcorrplot)
require(corrr)
require(treesnip)
require(ranger)
require(xgboost)
require(lightgbm)
require(lme4)

require(tidyverse)
require(tidymodels)
require(finetune)
require(workflowsets)
require(baguette)
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
  mutate(mean_fare = NULL) %>% 
  mutate(Embarked = if_else(is.na(Embarked),"U",Embarked))

test <- left_join(test,Impute_table,by=c("Title")) %>% 
  mutate(Age = if_else(is.na(Age),mean_age,Age)) %>%
  mutate(Fare = if_else(is.na(Fare),mean_fare,Fare)) %>% 
  mutate(mean_age = NULL) %>% 
  mutate(mean_fare = NULL)
  
train %>% 
  select(which(colSums(is.na(.))>0)) %>% 
  summarise_all(~ sum(is.na(.)))

test %>% 
  select(which(colSums(is.na(.))>0)) %>% 
  summarise_all(~ sum(is.na(.)))

rec <- 
  recipe(Survived ~ ., data=train) %>% 
  update_role(PassengerId,new_role="ID")

%>% 
  step_impute_mode(Embarked)

folds <- vfold_cv(train,strata = Survived)

bagged_model <-
  bag_tree(min_n=tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

model <-
  decision_tree(min_n=tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

bm4 <-
  bag_tree(min_n=4) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

bm6 <-
  bag_tree(min_n=6) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

bm7 <-
  bag_tree(min_n=7) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

bm8 <-
  bag_tree(min_n=8) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

bm9 <-
  bag_tree(min_n=9) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

bm10 <-
  bag_tree(min_n=10) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

workflowset <- 
  workflow_set(
    preproc = list(age = rec),
    models = list(c50=model))

wfs_4 <- 
  workflow_set(
    preproc = list(rec = rec),
    models = list(n4 = bm4))

wfs_6 <- 
  workflow_set(
    preproc = list(rec = rec),
    models = list(n6 = bm6))

wfs_7 <- 
  workflow_set(
    preproc = list(rec = rec),
    models = list(n7 = bm7))

wfs_8 <- 
  workflow_set(
    preproc = list(rec = rec),
    models = list(n8 = bm8))

wfs_9 <- 
  workflow_set(
    preproc = list(rec = rec),
    models = list(n9 = bm9))

wfs_10 <- 
  workflow_set(
    preproc = list(rec = rec),
    models = list(n10 = bm10))

c50_control <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

c50_res <- 
  workflowset %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    control = c50_control, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

bmc <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

res_4 <- 
  wfs_4 %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    control = bmc, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

res_6 <- 
  wfs_6 %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    control = bmc, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

res_7 <- 
  wfs_7 %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    control = bmc, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

res_8 <- 
  wfs_8 %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    control = bmc, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

res_9 <- 
  wfs_9 %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    control = bmc, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

res_10 <- 
  wfs_10 %>% 
  workflow_map(
    seed = 1234,
    resamples = folds,
    control = bmc, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

bm_res <- bind_rows(
  res_4,
  res_6,
  res_7,
)

autoplot(bm_res,rank_metric="accuracy") +
  geom_text(aes(y=0.55,label= wflow_id), angle=90) +
  lims(y=c(0.3,0.9)) +
  theme(legend.position = "none")

bm4_parm <-
  bm_res %>% 
  extract_workflow_set_result("rec_n4") %>% 
  select_best(metric="accuracy")

bm6_parm <-
  bm_res %>% 
  extract_workflow_set_result("rec_n6") %>% 
  select_best(metric="accuracy")

bm7_parm <-
  bm_res %>% 
  extract_workflow_set_result("rec_n7") %>% 
  select_best(metric="accuracy")

bm10_parm <-
  bm_res %>% 
  extract_workflow_set_result("rec_n10") %>% 
  select_best(metric="accuracy")

bm4_wf <- bm_res %>% 
  extract_workflow("rec_n4") %>% 
  finalize_workflow(bm4_parm)

bm6_wf <- bm_res %>% 
  extract_workflow("rec_n6") %>% 
  finalize_workflow(bm6_parm)

bm7_wf <- bm_res %>% 
  extract_workflow("rec_n7") %>% 
  finalize_workflow(bm7_parm)

bm10_wf <- bm_res %>% 
  extract_workflow("rec_n10") %>% 
  finalize_workflow(bm10_parm)

bm4_fit <- fit(bm4_wf,train)
bm6_fit <- fit(bm6_wf,train)
bm7_fit <- fit(bm7_wf,train)
bm10_fit <- fit(bm10_wf,train)

bm4_pred <- predict(bm4_fit,new_data=test)
bm6_pred <- predict(bm6_fit,new_data=test)
bm7_pred <- predict(bm7_fit,new_data=test)
bm10_pred <- predict(bm10_fit,new_data=test)

cbind(PassengerId = test$PassengerId,bm4_pred) %>% 
  rename(Survived = .pred_class) %>% 
  write_csv("./bm4_pred.csv")
cbind(PassengerId = test$PassengerId,bm6_pred) %>% 
  rename(Survived = .pred_class) %>% 
  write_csv("./bm6_pred.csv")
cbind(PassengerId = test$PassengerId,bm7_pred) %>% 
  rename(Survived = .pred_class) %>% 
  write_csv("./bm7_pred.csv")

# Med FamilyName og Title giver bagged c5.0 en score på 0.80143 - placering 369.
# Impute_mode er bedre end at sætte NA="U"
# Bedste er n=6 eller 7 - hvordan genskabes over 80%?
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

Since the random forest classifier will have a problem with dealing with the family name column, we will do the dummy encoding separately before fitting the random forest:
  ```{r}
imp_rec_rf <- imp_rec %>% 
  step_dummy(FamilyName)
```

imp_svm_lin <- svm_linear(cost=tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kernlab", scaled=TRUE)
imp_svm_pol <- svm_poly(cost=tune(),degree=tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kernlab", scaled=TRUE)
imp_svm_radial <- svm_rbf(cost=tune(),rbf_sigma=tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kernlab", scaled=TRUE)

imp_rf <- 
  rand_forest(trees = tune(), min_n=tune()) %>% 
  set_engine("randomForest") %>% 
  set_mode("classification")


imp_workflowset2 <- 
  workflow_set(
    preproc = list(rec = imp_rec),
    models = list(svm_poly = imp_svm_pol,
                  svm_rbf = imp_svm_radial))

imp_workflowset3 <-
  workflow_set(
    preproc = list(rec = imp_rec_rf),
    models = list(rf = imp_rf))

imp_workflowset4 <- bind_rows(imp_workflowset2,imp_workflowset3)

imp_results2 <- 
  imp_workflowset3 %>% 
  workflow_map(
    seed = 1234,
    resamples = imp_folds,
    grid = 25,
    control = imp_control, 
    metrics = metric_set(accuracy,roc_auc),
    verbose = TRUE
  )

imp_results <- bind_rows(imp_results1,imp_results2)
