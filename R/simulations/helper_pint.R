#---------------------------------------------------------------------------------------------------
# HELPER FUNCTIONS TO GENERATE DATA FOR SIMULATIONS
#---------------------------------------------------------------------------------------------------


source("R/simulations/batchtools/simulation_setting_definition.R")


get_sim_results = function(data, job, instance, feature, learner, n.split, impr.par, obj.function, pint=FALSE, ...){
  
  data = instance$data
  S = instance$S
  Z = instance$Z
  X = data[, setdiff(colnames(data), "y")]
  features = colnames(X)
  testdata = create_sim_data(job, n = 100000, type = job$prob.pars$type, cor = job$prob.pars$cor, 
                             dep = job$prob.pars$dep, beta = job$prob.pars$beta, noise = job$prob.pars$noise,...)$data 
  
  if(learner == "gam"){
    
    if(job$prob.pars$type == "spur_pint"){
      model = gam(y~s(x1,x2) + 
                    s(x1)+s(x2)+s(x3)+s(x4),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      predictor = Predictor$new(model, data = X, predict.function = predict.function)
      
      model_wrapper = function(data, target){
        model = gam(y~s(x1,x2) +s(x1)+s(x2)+s(x3)+s(x4),data=data, method="REML")
        predict.function = function(model, newdata) predict.gam(model, newdata = newdata)
        return(list("model_fit" = model, "predict.function" = predict.function))
      }
    }
   
    pred = predict.function(model, testdata)
    perf.test = measureMSE(testdata$y, pred)
    
  }
  
  
  else{
    task = mlr::makeRegrTask(data = data, target = "y")
    if(learner == "regr.nnet"){
      lrn = mlr::makeLearner(learner,  maxit = 1000,  size = 10, decay = 0.001, trace = F)
    }
    else{
      lrn = mlr::makeLearner(learner)
    }
    
    model = mlr::train(lrn, task)
    predictor = Predictor$new(model, data = X, y = data$y)
    
    model_wrapper = function(data, target){
      task = makeRegrTask(data = data, target = target)
      if(learner == "regr.nnet"){
        lrn = mlr::makeLearner(learner,  maxit = 1000,  size = 10, decay = 0.001, trace = F)
      }
      else if(learner == "regr.ranger"){
        lrn = mlr::makeLearner(learner, mtry = 3)
      }
      else {
        lrn = mlr::makeLearner(learner)
      }
      model = mlr::train(lrn, task)
      predict.function = function(model, newdata) predict(model, newdata = newdata)$data$response
      return(list("model_fit" = model, "predict.function" = predict.function))
    }
    
    # performance on training data - sanity check how good ML model adjusts to underlying function
    predict.function = function(model, newdata) predict(model, newdata = newdata)$data$response
    pred = predict(object = model, newdata = testdata)
    perf.test = mlr::measureMSE(pred$data$truth, pred$data$response)
  }
  
  
  if(pint == TRUE){
    
    method = c("pd", "ale", "shap") # UPDATE
    #method = "pd"
    null_all = null_int_perm(X = X, y = data$y, model = model_wrapper, S = features, nperm = 100, effect_method = method)
    int_imp = interaction_importance(X = X, y = data$y, model_wrapper, method, features, calc_hstat = FALSE)
    
    int_test = lapply(names(null_all), function(method){
      null_dist = null_all[[method]]
      int_val = int_imp$int_imp[[method]]
      pint = PIMP(int_val, null_dist, method = "nonpar")
      return(list("pint" = pint))
    })
    names(int_test) = method

  }


  
  if(pint == TRUE){
    return(list("perf.test" = perf.test, "result.pint" = int_test))
  }
 
  
}




