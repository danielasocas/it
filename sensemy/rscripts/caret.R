# Daniela Socas Gil
# Last modified: 19/09/17

#######################################################################
#                               Modeling                              #
#######################################################################

dfm_train <- dfm_base %>% 
  filter(day <= 23)
dfm_train <- dfm_train[,colnames(dfm_train)%in% features]

dfm_test <- dfm_base %>% 
  filter(day > 23) 
dfm_test <- dfm_test[,colnames(dfm_test)%in% features]


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
control <- trainControl(method="repeatedcv", number=5, repeats = 2)


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

str(m_rf, max.level = 1)


#-----------------------------------------#
#                 Tree                    #
#-----------------------------------------#
set.seed(1234)
m_tree_4 <- train(speed ~ ., dfm_train, trControl=control, 
                method = 'rpart')
fancyRpartPlot(m_tree_4$finalModel)
varImp(m_tree)
m_tree_5 <- m_tree
plot(m_tree)

preds_tree <- predict(m_tree_4$finalModel,dfm_test)
m_tree$finalModel
#------ interactive

fit_tree_5 <- rpartXse(speed ~  . ,
         data=dfm_train,
         method="anova",
         control = rpart.control(minsplit=20, cp=0))

fancyRpartPlot(fit_tree_5)
new.fit_tree <- prp(fit_tree,snip=TRUE)$obj
fancyRpartPlot(new.fit_tree)
fit_tree

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

res_ <- performanceEstimation( PredTask(speed ~ ., dfm_test),
                                workflowVariants("standardWF",
                                                 learner=c("rpartXse","randomForest","earth")),
                                EstimationTask(metrics=c("mse","rmse"),method=CV(nReps=2,nFolds=5)))
