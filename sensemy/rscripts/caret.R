# Daniela Socas Gil
# Last modified: 19/09/17

#######################################################################
#                               Modeling                              #
#######################################################################
dfm_base_tr <- dfm_base[,colnames(dfm_base)%in% features]

dfm_train <- dfm_base %>% 
  filter(day <= 23)
dfm_train <- dfm_train[,colnames(dfm_train)%in% features]

dfm_test <- dfm_base %>% 
  filter(day > 23) 
dfm_test <- dfm_test[,colnames(dfm_test)%in% features]

#Correlation matrix 
featurePlot(x=dfm_train[,colnames(dfm_train)%in% features],
            y= dfm_train$speed,
            plot = "pairs")

#######################################################################
#                                 Caret                               #
#######################################################################

#Checking NA in df. 
sum(is.na(dfm_train))

#--------------------------- Tuning -------------------------#
##############################################################

# Caret have boosting by default

# 10-fold cross validation 
control <- trainControl(method="cv", number=10 )

# 10-fold cross validation repeated twice
control <- trainControl(method="repeatedcv", number=4, repeats = 2)


#--------------------------- Models -------------------------#
##############################################################

#-----------------------------------------#
#              Random Forest              #
#-----------------------------------------#
set.seed(1234)
m_rf <- train(speed ~ ., dfm_train, trControl=control,  importance = TRUE,
              method = 'rf', prox = TRUE)
m_rf
m_rf_4 <- m_rf
varImp(m_rf)

preds_rf <- predict(m_rf_4$finalModel,dfm_test)

randerr <- mean(abs(dfm_test$speed - preds_rf))

str(m_rf, max.level = 1)

#-----------------------------------------#
#                 Tree                    #
#-----------------------------------------#
set.seed(1234)
m_tree <- train(speed ~ ., dfm_train, 
                  trControl=control, 
                  method = 'rpart')
fancyRpartPlot(m_tree$finalModel)
varImp(m_tree)
m_tree_5 <- m_tree
plot(m_tree)

preds_tree <- predict(m_tree_4$finalModel,dfm_test)
m_tree$finalModel
#------ interactive

fit_tree_ <- rpartXse(speed ~  . ,
         data=dfm_train,
         method="anova",
         control = rpart.control(minsplit=2
                                 , cp=0))

varImp(fit_tree_)

fancyRpartPlot(fit_tree_)
new.fit_tree <- prp(fit_tree,snip=TRUE)$obj
fancyRpartPlot(new.fit_tree)

#-----------------------------------------#
#                 GBM                     #
#-----------------------------------------#
set.seed(1234)
m_gbm <- train(speed ~ ., dfm_train, trControl=control, verbose = FALSE,
                method = 'gbm')
m_gbm
varImp(m_gbm)
m_gbm_5 <- m_gbm

preds_gbm <- predict(m_gbm_4$finalModel,dfm_test)

#--------------------------- Results -------------------------#
##############################################################
results <- resamples(list(cart=m_tree, rf=m_rf, gbm = m_gbm))
results_5 <- results
summary(results)
dotplot(results)

#--------------------------- PE -------------------------#
##############################################################

res_ <- performanceEstimation( PredTask(speed ~ ., dfm_train),
                                workflowVariants("standardWF",
                                                 learner=c("rpartXse","randomForest","earth")),
                                EstimationTask(dfm_test, metrics=c("mse","rmse"),method=CV(nReps=2,nFolds=5)))
rankWorkflows(res_)
?Workflow

# Monte Carlo 

res_all_tree <- performanceEstimation( PredTask(speed ~ ., dfm_base_tr),
                               c(Workflow(wf='standardWF',wfID="standRF",
                                          learner="rpartXse"),
                                 Workflow(wf='timeseriesWF',wfID="slideRF",
                                          learner="rpartXse",
                                          type="slide"),
                                 Workflow(wf='timeseriesWF',wfID="growRF",
                                          learner="rpartXse",
                                          type="grow")),
                               EstimationTask(metrics=c("mse","rmse"),
                                              method=MonteCarlo(nReps=10,szTrain=0.25,szTest=0.25)))

res_all_rf_v2<- performanceEstimation( PredTask(speed ~ ., dfm_base_tr),
                                       c(Workflow(wf='standardWF',wfID="standRF",
                                                  learner="randomForest"),
                                         Workflow(wf='timeseriesWF',wfID="slideRF",
                                                  learner="randomForest",
                                                  type="slide"),
                                         Workflow(wf='timeseriesWF',wfID="growRF",
                                                  learner="randomForest",
                                                  type="grow")),
                                       EstimationTask(metrics=c("mse","rmse"),
                                                      method=MonteCarlo(nReps=10,szTrain=0.2,szTest=0.2)))

res_all_mars <- performanceEstimation( PredTask(speed ~ ., dfm_base_tr),
                                       c(Workflow(wf='standardWF',wfID="standRF",
                                                  learner="earth"),
                                         Workflow(wf='timeseriesWF',wfID="slideRF",
                                                  learner="earth",
                                                  type="slide"),
                                         Workflow(wf='timeseriesWF',wfID="growRF",
                                                  learner="earth",
                                                  type="grow")),
                                       EstimationTask(metrics=c("mse","rmse"),
                                                      method=MonteCarlo(nReps=10,szTrain=0.25,szTest=0.25)))

rankWorkflows(res_all_tree)
rankWorkflows(res_all_RF)
rankWorkflows(res_all_mars)
plot(res_all_1)
summary(res_all_1)

#--------------------------- Plotting Rf -------------------------#
##############################################################

#tree_num <- which(m_rf_4$finalModel$forest$ndbigtree == min(m_rf_4$finalModel$forest$ndbigtree))
#min_rf <- tree_func(final_model = m_rf_4$finalModel, tree_num)
