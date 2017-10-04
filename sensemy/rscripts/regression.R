# Daniela Socas Gil
# Last modified: 25/07/17

source("rscripts/features.R")

#######################################################################
#                               Modeling                              #
#######################################################################

dfm_train <- dfm_base %>% 
  filter(day < 23)
dfm_train <- dfm_train[,colnames(dfm_train)%in% features]

dfm_test <- dfm_base %>% 
  filter(day > 23) 
dfm_test <- dfm_test[,colnames(dfm_test)%in% features]

#-------------------------------- Regression -------------------------#
#######################################################################

#-----------------------------------------#
#                 Tree                    #
#-----------------------------------------#

# Model
regtree <- rpartXse(speed  ~ ., dfm_train, method = "anova")
preds <- predict(regtree,dfm_test)
#preds

# Error 
mae <- mean(abs(preds-dfm_test$speed))
mae

cr <- cor(preds,dfm_test$speed)
cr

#Plotting 
prp(regtree, type = 4)

new.regtree <- prp(regtree,snip=TRUE)$obj

fancyRpartPlot(regtree)
#prettyTree(regtree)

# Saving in other variable 
regtree_0 <- regtree 
regmae_0 <- mae 
cr0 <- cr

regr.eval(dfm_test$speed, regtree,train.y=dfm_train$speed)

#-----------------------------------------#
#              Random Forest              #
#-----------------------------------------#
set.seed(1234)
mrand <- randomForest(speed ~ ., dfm_train, importance = TRUE, method = "anova", ntree = 2000)

predrand <- predict(mrand,dfm_test)
describe(predrand)
table(predrand,dfm_test$speed)
randerr <- mean(abs(dfm_test$speed - predrand))

multiclass.roc(dfm_test$speed, predrand)

mrand_0 <- mrand
predrand_0 <- predrand
randerr_0 <- randerr


#------------- PARTY ---------------------#

mrand.p_0 <- randomForest(speed ~ ., dfm_train, 
                        controls=cforest_unbiased(ntree=2000, mtry=3))

#-----------------------------------------#
#                 Mars                    #
#-----------------------------------------#
set.seed(1234)
mars <- earth(speed ~ .,dfm_train)
predmars <- predict(mars,dfm_test)
mae_mars <- mean(abs(dfm_test$speed - predmars))
mae_mars
summary(predmars)
table(predmars,dfm_test$speed)

summary(mars)

mars_4 <- mars
mae_mars_0 <- mae_mars

