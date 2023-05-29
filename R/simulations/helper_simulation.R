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
  testdata = create_sim_data(job, n = 100000, type = job$prob.pars$type, cor = job$prob.pars$cor,...)$data
  #browser()
  if(learner == "lm"){
    if(job$prob.pars$type == "xor"){
      model = lm(y~x1*x3 + x2,data=data)
    }
    if(job$prob.pars$type == "linear"){
      model = lm(y~x1*x2 + x3*x4 + x3 + x4,data=data)
    }
    if(job$prob.pars$type == "categorical_linear"){
      model = lm(y~x1*x2*x3*x4*x5,data=data)
    }
    predict.function = function(model, newdata) predict.lm(model, newdata)
    predictor = Predictor$new(model, data = X, predict.function = predict.function)
    
    pred = predict.function(model, testdata)
    perf.test = measureMSE(testdata$y, pred)
  }
  
  if(learner == "gam"){
    if(job$prob.pars$type == "xor"){
      model = gam(y~s(x1,x3)+s(x1)+s(x2)+s(x3),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata = newdata)
      predictor = Predictor$new(model, data = X, predict.function = predict.function)
    }
    if(job$prob.pars$type == "linear"){
      model = gam(y~s(x1,x2)+s(x3,x4)+
                  s(x1)+s(x2)+s(x3)+s(x4),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      predictor = Predictor$new(model, data = X, predict.function = predict.function)
    }
    if(job$prob.pars$type == "categorical_linear1"){
      model = gam(y~s(x1,x3,x4,x5)+
                    s(x1)+s(x2)+s(x3)+s(x4)+s(x5),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      predictor = Predictor$new(model, data = X, predict.function = predict.function)
    }
    if(job$prob.pars$type == "categorical_linear2"){
      model = gam(y~s(x1,x2,x3,x4,x5)+
                    s(x1)+s(x2)+s(x3)+s(x4)+s(x5),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      predictor = Predictor$new(model, data = X, predict.function = predict.function)
    }
    
    if(job$prob.pars$type == "linear_pint"){
      model = gam(y~s(x1,x2) + s(x4,x5)+
                    s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6),data=data, method="REML")
      predict.function = function(model, newdata) predict.gam(model, newdata)
      predictor = Predictor$new(model, data = X, predict.function = predict.function)
      
      model_wrapper = function(data, target){
        model = gam(y~s(x1,x2) + s(x4,x5)+s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6),data=data, method="REML")
        predict.function = function(model, newdata) predict.gam(model, newdata = newdata)
        return(list("model_fit" = model, "predict.function" = predict.function))
      }
    }
    
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
  
  else if(learner == "ranger_exact"){
   
    if(job$prob.pars$type == "xor"){
      interaction_constraint_list = "[[0,2]]"
    }
    if(job$prob.pars$type == "linear"){
      interaction_constraint_list = "[[0,1], [2,3]]"
    }
    if(job$prob.pars$type == "categorical_linear1"){
      interaction_constraint_list = "[[0,2,3,4]]"
    }
    if(job$prob.pars$type == "categorical_linear2"){
      interaction_constraint_list = "[[0,1,2,3,4]]"
    }
    #mod = ranger(formula = formula,data=data)
    library(mlr3)
    task = TaskRegr$new(id = "test", backend = data, target = "y")
    lrn.xgb = lrn("regr.xgboost", objective = "reg:squarederror",
                  eta = 0.1,  nrounds = 1000,
                  interaction_constraints = interaction_constraint_list)
    
    model = lrn.xgb
    predictor = Predictor$new(model$train(task), data = X, y = data$y)
    predict.function = function(model, newdata) model$predict_newdata(newdata = newdata[,setdiff(colnames(newdata), "y")])$response
    pred = lrn.xgb$predict_newdata(task = task,  newdata = testdata)
    perf.test = mlr3measures::mse(truth = pred$truth, response = pred$response)
    
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
    #method = c("pd", "ale")
    null_all = null_int_perm(X = X, y = data$y, model = model_wrapper, S = features, nperm = 100, effect_method = method)
    int_imp = interaction_importance(X = X, y = data$y, model_wrapper, method, features, calc_hstat = TRUE)
    
    int_test = lapply(names(null_all), function(method){
      null_dist = null_all[[method]]
      int_val = int_imp$int_imp[[method]]
      pint = PIMP(int_val, null_dist, method = NA)
      return(list("pint" = pint, "hstat" = int_imp$hstat))
    })
    names(int_test) = method

  }

  # trees with different improvement
  tree.result = lapply(obj.function, function(obj){
    if(obj == "SS_L2_shap_rc"){
      obj = "SS_L2_shap"
      shap.recalc = TRUE
      store.data = TRUE
    }
    else if(obj == "SS_L2_shap_not_rc"){
      obj = "SS_L2_shap"
      shap.recalc = FALSE
      store.data = TRUE
    }
    else{
      shap.recalc = FALSE
      store.data = TRUE
    } 
    if(pint == FALSE){
      S = list("not_pint" = S)
      Z = list(Z)
    } 
    else{
      S = Z = list("not_pint" = S, "pint" = S[which(int_test[[substr(obj, 7,nchar(obj))]]$pint<=0.05)])
    }
    
    effect = effect_function(objective = obj, model = model, data = data, S = features, predict.fun = predict.function)
    
    tree.sets = lapply(1:length(S), function(i){
      if(obj == "SS_L2_pd") effect = effect_function(objective = obj, model = model, data = data, S = S[[i]], predict.fun = predict.function)
      else effect = effect[which(names(effect) %in% S[[i]])]
      
      tree.impr = lapply(impr.par, function(impr){
        tree = compute_tree(effect = effect, testdata = data, model = model, predict.function = predict.function, objective = obj, Z = Z[[i]], n.split = n.split, impr.par = impr, min.split = 40, n.quantiles = 100, store.data = store.data, shap.recalc = shap.recalc)
        tree = list.clean(tree, function(x) length(x) == 0L, TRUE)
        
        sim.result = extract_split_criteria(tree)
        
        if(is.null(sim.result)) return()
        else if(nrow(sim.result) == 0) return()
        
        else{
          sim.result$objective.value.root = sim.result$objective.value.parent[sim.result$id==0]
          sim.result$impr.par = impr
          sim.result$objective.function = obj
          sim.result$recalc = shap.recalc
          sim.result$pint = names(S)[[i]]
          return(sim.result)
        }
        
      })
      tree.impr = list.clean(tree.impr)
      tree.impr = do.call("rbind", tree.impr)
      return(tree.impr)
    })
    tree.sets = list.clean(tree.sets)
    tree.sets = rbindlist(tree.sets, fill = TRUE)
    return(tree.sets)
  })
    
  tree.result = list.clean(tree.result)
  tree.result = rbindlist( tree.result, fill = TRUE)
  
  
  # calculate hstatistic
  #hstat = Interaction$new(model, feature = feature)$results
  #hstat$interact.feature = setdiff(colnames(X),feature)
  
  if(pint == TRUE){
    return(list("result.tree" = as.data.frame(tree.result), "perf.test" = perf.test, "result.pint" = int_test))
  }
  else{
    return(list("result.tree" = as.data.frame(tree.result), "perf.test" = perf.test))
  }
  
}




