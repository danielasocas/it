# Daniela Socas Gil
# Created: 12/10/17

#--------------------------- PE -------------------------#
##############################################################

# Monte Carlo 

res_all_tree <- performanceEstimation( PredTask(speed ~ ., dfm_base_tr),
                                       c(Workflow(wf='standardWF',wfID="standTree",
                                                  learner="rpartXse"),
                                         Workflow(wf='timeseriesWF',wfID="slideTree",
                                                  learner="rpartXse",
                                                  type="slide"),
                                         Workflow(wf='timeseriesWF',wfID="growTree",
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
                                       c(Workflow(wf='standardWF',wfID="standarMars",
                                                  learner="earth"),
                                         Workflow(wf='timeseriesWF',wfID="slideMars",
                                                  learner="earth",
                                                  type="slide"),
                                         Workflow(wf='timeseriesWF',wfID="growMars",
                                                  learner="earth",
                                                  type="grow")),
                                       EstimationTask(metrics=c("mse","rmse"),
                                                      method=MonteCarlo(nReps=10,szTrain=0.25,szTest=0.25)))

rankWorkflows(res_all_tree)
rankWorkflows(res_all_RF)
rankWorkflows(res_all_mars)
plot(res_all_1)
summary(res_all_1)
