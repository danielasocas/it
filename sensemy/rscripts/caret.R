# Daniela Socas Gil
# Last modified: 19/09/17

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

# 10-fold cross validation repeated 
control <- trainControl(method="repeatedcv", number=7, repeats = 3)


#--------------------------- Models -------------------------#
##############################################################

#-----------------------------------------#
#              Random Forest              #
#-----------------------------------------#
set.seed(128)
m_rf <- train(speed ~ ., dfm_train, trControl=control,  importance = TRUE,
              method = 'rf', prox = TRUE)
m_rf
m_rf_5 <- m_rf
varImp(m_rf)

preds_rf <- predict(m_rf_4$finalModel,dfm_test)

str(m_rf, max.level = 1)

#-----------------------------------------#
#                 Tree                    #
#-----------------------------------------#
set.seed(128)
m_tree <- train(speed ~ ., dfm_train, trControl=control, 
                method = 'rpart')
m_tree
varImp(m_tree)
m_tree_5 <- m_tree

preds_tree <- predict(m_tree_4$finalModel,dfm_test)

#-----------------------------------------#
#                 GBM                     #
#-----------------------------------------#
set.seed(128)
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

res_6 <- performanceEstimation(
  PredTask(speed ~ .,dfm_train),
  Workflow("standardWF",learner=c("rpart","rf")),
  EstimationTask(metrics="rmse",method=CV(nReps=1,nFolds=10)))
summary(res_5)
