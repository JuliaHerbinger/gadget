#---------------------------------------------------------------------------------------------------
# FUNCTIONS TO PERFORM INTERACTION TEST (PINT)
#---------------------------------------------------------------------------------------------------

helper_interaction_importance = function(method, model, data, S, predict.function, class){
  if(method == "pd"){
    mean_center_ice = function(ice){
      
      Y = tidyr::spread(ice, .borders, .value)
      Y = Y[, setdiff(colnames(Y), c(".type", ".id", ".feature"))]
      
      # center ICE curves by their mean
      Y = Y - rowMeans(Y)
      Y = setDT(Y)
    }
    
    pred = iml::Predictor$new(model, data = data)
    effects = iml::FeatureEffects$new(pred, features = S, method = "ice", grid.size = 10)
    eff_var = lapply(effects$results, function(feat){
      
      if(!is.null(feat$.class)){
        feat = feat[feat$.class==class,]
        feat = feat[,setdiff(colnames(feat), ".class")]
      } 
      if(is.factor(feat$.borders)) feat$.borders = as.numeric(feat$.borders)
      ice_centered = mean_center_ice(feat)
      var_ice = ice_centered %>% dplyr::summarise(across(where(is.numeric), var))
      sum(var_ice)
    })
  }
  if(method == "ale"){
    effects = ale_deriv(model, S = S, data, "y", h = 20, predict.function)
    eff_var = lapply(effects, function(feat){
      delta.aggr = feat[, list(dL = mean(dL, na.rm = TRUE),
                               interval.n = .N), by = c("interval.index", "x.left", "x.right")]
      
      df = merge(feat, delta.aggr, by = "interval.index")
      sum(((df$dL.x - df$dL.y)^2), na.rm = TRUE)
    })
    
  }
  if(method == "shap"){
    effects = fast_shap_values(model, S = S, data = data, target = "y", nsim = 500, pred.fun = predict.function, seed = FALSE)
    eff_var = lapply(effects, function(feat){
      if(is.numeric(feat$feat.val)){
        gam_mod = mgcv::gam(phi~s(feat.val, k = 3), data = feat)
        sum(gam_mod$residuals^2, na.rm = TRUE)
      }
      else{
        var_interval = feat %>% dplyr::group_by(feat.val) %>% dplyr::summarise(variance = var(phi, na.rm = TRUE))
        sum(var_interval$variance)
      }
    })
  }
  return(eff_var)
}

null_interaction = function(X, y, model_wrapper, S, effect_method, class){
    y_perm = y[sample(1:length(y), length(y))]
 # y_perm = sample(y)  
  data_perm = cbind(X, "y" = y_perm)
    model = model_wrapper(data_perm, target = "y")
    model_fit = model$model_fit
    predict.function = model$predict.function
    null_int = lapply(effect_method, function(method){
      effect = helper_interaction_importance(method, model_fit, data_perm, S, predict.function, class)
      #names(eff_var) = S
      return(do.call("cbind", effect))
    })
    names(null_int) = effect_method
    return(null_int)
  
}

null_int_perm = function(X, y, model, S, nperm, effect_method, class = NULL){
 
  df = data.frame(matrix(ncol = length(S), nrow = 0))
  colnames(df) = S
  null_int_dist = lapply(effect_method, function(x) x = df)
  names(null_int_dist) = effect_method
  for(i in 1:nperm){
    null_int = null_interaction(X, y, model, S, effect_method, class)
    for(method in effect_method){
      null_int_dist[[method]][i,] = null_int[[method]] 
    }
  }
  return(null_int_dist)
}

null_int_perm_par = function(X, y, model, S, nperm, effect_method, class = NULL){
  helper_interaction_importance = function(method, model, data, S, predict.function, class){
    if(method == "pd"){
      mean_center_ice = function(ice){
        
        Y = tidyr::spread(ice, .borders, .value)
        Y = Y[, setdiff(colnames(Y), c(".type", ".id", ".feature"))]
        
        # center ICE curves by their mean
        Y = Y - rowMeans(Y)
        Y = setDT(Y)
      }
      
      pred = iml::Predictor$new(model, data = data)
      effects = iml::FeatureEffects$new(pred, features = S, method = "ice", grid.size = 10)
      eff_var = lapply(effects$results, function(feat){
        
        if(!is.null(feat$.class)){
          feat = feat[feat$.class==class,]
          feat = feat[,setdiff(colnames(feat), ".class")]
        } 
        if(is.factor(feat$.borders)) feat$.borders = as.numeric(feat$.borders)
        ice_centered = mean_center_ice(feat)
        var_ice = ice_centered %>% dplyr::summarise(across(where(is.numeric), var))
        sum(var_ice)
      })
    }
    if(method == "ale"){
      effects = ale_deriv(model, S = S, data, "y", h = 20, predict.function)
      eff_var = lapply(effects, function(feat){
        delta.aggr = feat[, list(dL = mean(dL, na.rm = TRUE),
                                 interval.n = .N), by = c("interval.index", "x.left", "x.right")]
        
        df = merge(feat, delta.aggr, by = "interval.index")
        sum(((df$dL.x - df$dL.y)^2), na.rm = TRUE)
      })
      
    }
    if(method == "shap"){
      effects = fast_shap_values(model, S = S, data = data, target = "y", nsim = 500, pred.fun = predict.function, seed = FALSE)
      eff_var = lapply(effects, function(feat){
        if(is.numeric(feat$feat.val)){
          gam_mod = mgcv::gam(phi~s(feat.val, k = 3), data = feat)
          sum(gam_mod$residuals^2, na.rm = TRUE)
        }
        else{
          var_interval = feat %>% dplyr::group_by(feat.val) %>% dplyr::summarise(variance = var(phi, na.rm = TRUE))
          sum(var_interval$variance)
        }
      })
    }
    return(eff_var)
  }
  null_interaction = function(X, y, model_wrapper, S, effect_method, class){
    y_perm = y[sample(1:length(y), length(y))]
    data_perm = cbind(X, "y" = y_perm)
    model = model_wrapper(data_perm, target = "y")
    model_fit = model$model_fit
    predict.function = model$predict.function
    null_int = lapply(effect_method, function(method){
      effect = helper_interaction_importance(method, model_fit, data_perm, S, predict.function, class)
      #names(eff_var) = S
      return(do.call("cbind", effect))
    })
    names(null_int) = effect_method
    return(null_int)
    
  }
  #setup parallel backend to use many processors
  cores=parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-1) #not to overload your computer
  doParallel::registerDoParallel(cl)
  
  
  df = data.frame(matrix(ncol = length(S), nrow = 0))
  colnames(df) = S
  null_int_dist = lapply(effect_method, function(x) x = df)
  names(null_int_dist) = effect_method
  null_int_dist = foreach::foreach(i=1:nperm, .combine=rbind) %dopar% {
      null_int = null_interaction(X, y, model, S, effect_method, class)
      null_int_dist_temp = null_int[[effect_method]] 
      null_int_dist_temp
  }
  #stop cluster
  stopCluster(cl)
  return(null_int_dist)
}

interaction_importance = function(X, y, model_wrapper, effect_method, S, calc_hstat = TRUE, class = NULL){
  data = cbind(X, "y" = y)
  model = model_wrapper(data, target = "y")
  model_fit = model$model_fit
  predict.function = model$predict.function
  int_imp = lapply(effect_method, function(method){
    effect = helper_interaction_importance(method, model_fit, data, S, predict.function, class)
    #names(eff_var) = S
    return(do.call("c", effect))
  })
  names(int_imp) = effect_method
  
  if(calc_hstat == TRUE){
    pred = Predictor$new(model_fit, data = data)
    hstat = Interaction$new(pred)
    return(list("int_imp" = int_imp, "hstat" = hstat))
  }
  else return(list("int_imp" = int_imp))
    
}

