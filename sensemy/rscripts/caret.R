# Daniela Socas Gil
# Last modified: 19/09/17

#######################################################################
#                               Modeling                              #
#######################################################################
dfm_base_tr <- dfm_base[,colnames(dfm_base)%in% features]

# If doing Holdout  

dfm_train <- dfm_base %>%
  filter(day <= 23)
dfm_train <- dfm_train[,colnames(dfm_train)%in% features]

dfm_test <- dfm_base %>%
  filter(day > 23)
dfm_test <- dfm_test[,colnames(dfm_test)%in% features]

#Correlation matrix 
featurePlot(x=dfm_base[,colnames(dfm_base)%in% features],
            y= dfm_base$speed,
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

# Monte Carlo, growing window. 
control <- trainControl(method = "timeslice",
                           initialWindow = as.integer(0.25*nrow(dfm_base_tr)),
                           horizon = as.integer(0.25*nrow(dfm_base_tr)),
                           fixedWindow = FALSE,
                           allowParallel = TRUE,
                           seeds = seeds)

tuneLength.num <- 3

#### creating sampling seeds ####
set.seed(1234)
seeds <- vector(mode = "list", length = 1661)
for(i in 1:1660) seeds[[i]] <- sample.int(1000, 3)

## For the last model:
seeds[[1661]] <- sample.int(1000, 1)

# Parallel processing 
registerDoParallel(cores= 2)

#--------------------------- Models -------------------------#
##############################################################

#-----------------------------------------#
#              Random Forest              #
#-----------------------------------------#

rf.mod <- train(speed ~ ., 
                dfm_base_tr, 
                method = 'rf',
                importance = TRUE,
                trControl=control,
                tuneLength=tuneLength.num )

rf.mod <- rf.mod
varImp(rf.mod)

rf.preds <- predict(rf.mod$finalModel, dfm_test)
randerr <- mean(abs(dfm_test$speed - rf.preds))
str(rd.mod, max.level = 1)

#-----------------------------------------#
#                 Tree                    #
#-----------------------------------------#

tree.mod <- train(speed ~ ., 
                  dfm_base_tr, 
                  method = 'rpart',
                  trControl=control, 
                  tuneLength=tuneLength.num)

party.mod <- train(speed ~ .,
                   data = dfm_base_tr,
                   method = "ctree",
                   trControl =  control,
                   tuneLength=tuneLength.num)

fancyRpartPlot(tree.mod$finalModel)
varImp(tree.mod)
plot(m_tree)


tree.preds <- predict(tree.mod$finalModel,dfm_test)
tree.mod$finalModel

tree_v1.mod <- tree.mod

#------ interactive

fit_tree_ <- rpartXse(speed ~  . ,
         data=dfm_base_tr,
         method="anova",
         control = rpart.control(minsplit=2, cp=0))

varImp(fit_tree_)

fancyRpartPlot(fit_tree_)
new.fit_tree <- prp(fit_tree,snip=TRUE)$obj
fancyRpartPlot(new.fit_tree)

#-----------------------------------------#
#                 GBM                     #
#-----------------------------------------#

gbm.mod <- train(speed ~ ., 
               dfm_base_tr, 
               method = 'gbm',
               trControl=control,
               tuneLength=tuneLength.num,
               verbose = FALSE)
        
gbm.mod
varImp(gbm.mod)

gbm.preds <- predict(gbm.mod$finalModel,dfm_test)
gbm_v1.mod <- gbm.mod

#-----------------------------------------#
#                 MARS                    #
#-----------------------------------------#

mars.mod <- train(speed ~ ., 
                 dfm_base_tr, 
                 method = 'earth',
                 trControl=control,
                 tuneLength=tuneLength.num)

mars.mod
#--------------------------- Results -------------------------#
##############################################################
results_ <- resamples(list( rf=rf.mod, 
                            repart = tree.mod, 
                            gbm=gbm.mod,
                            mars=mars.mod))

results <- resamples(list(mars=mars.mod,
                          gbm=gbm.mod,
                          rf=rf.mod))
#ss <- 
summary(results)

trellis.par.set(caretTheme())
dotplot(results, metric = "RMSE")


results_tree <- resamples(list(rpart=tree.mod,
                          party=party.mod))
summary(results_tree)
dotplot(results, metric = "RMSE")


#--------------------------- Plotting Rf -------------------------#
##############################################################

#tree_num <- which(m_rf_4$finalModel$forest$ndbigtree == min(m_rf_4$finalModel$forest$ndbigtree))
#min_rf <- tree_func(final_model = m_rf_4$finalModel, tree_num)
