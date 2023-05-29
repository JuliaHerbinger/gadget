#---------------------------------------------------------------------------------------------------
# HELPER FUNCTIONS TO CALCULATE FEATURE EFFECTS
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------
# helper functions for ICE/PDP

mean_center_ice = function(ice, feature){
  Y = tidyr::spread(ice, feature, .value)
  Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
  
  # center ICE curves by their mean
  Y = Y - rowMeans(Y)
  setDT(Y)
}

#---------------------------------------------------------------------------------------------------
# helper functions for ALE

cumsum_na = function(values) {
  values[is.na(values)] = 0
  cumsum(values)
}

# interval marginal feature importance
ale_deriv = function(object, S, data, target, h = h, predict.fun) {
  X = data[,setdiff(colnames(data), target)]
  features = colnames(X)
  deriv = lapply(S, function(feature){
  
    if (class(data[,feature]) == "factor") {#for categorical X[,J], calculate the ALE plot
     
   
      #Calculate the model predictions with the levels of X[,J] increased and decreased by one
      data[,feature] <- droplevels(data[,feature])
      K <- nlevels(data[,feature])
      levs.orig <- levels(data[,feature])
      x.ord <- as.numeric(data[,feature])
      row.ind.plus <- (1:nrow(data))[x.ord < K]  #indices of rows for which X[,J] was not the highest level
      row.ind.neg <- (1:nrow(data))[x.ord > 1]  #indices of rows for which X[,J] was not the lowest level
      X.plus <- X
      X.neg <- X
      X.plus[row.ind.plus,feature] <- levs.orig[x.ord[row.ind.plus]+1]  
      X.neg[row.ind.neg,feature] <- levs.orig[x.ord[row.ind.neg]-1]
      #y.hat <- predict.fun(object, newdata = X)$data$response
      y.hat.plus <- predict.fun(object, newdata = X.plus)
      y.hat.neg <- predict.fun(object, newdata = X.neg)
      
      #Take the appropriate differencing and averaging for the ALE plot
      Delta <- y.hat.plus-y.hat.neg
      
      DT = data.table(
        "feat.val" = data[,feature],
        "x.left" = X.neg[,feature],
        "x.right" = X.plus[,feature],
        "dL" = Delta,
        "interval.index" = as.numeric(data[,feature]),
        "interval.width" = 1
      )
      
    } 
    
    else{
      # Interval bounds
      q = quantile(data[[feature]], 0:h/h)
      # Matching data instances to intervals
      interval.index = findInterval(data[[feature]], q, left.open = TRUE)
      # Data point in the left most interval should be in interval 1, not zero
      interval.index[interval.index == 0] = 1
      # Getting the predictions for instances of upper and lower interval limits
      data.lower = data.upper = data
      data.lower[[feature]] = q[interval.index]
      data.upper[[feature]] = q[interval.index + 1]
      loss.lower = predict.fun(object, newdata = data.lower)
      loss.upper = predict.fun(object, newdata = data.upper)
      # finite differences
      dL = (loss.upper - loss.lower)
      interval.width = q[interval.index + 1] - q[interval.index]
    
    
    
    DT = data.table(
      "feat.val" = data[,feature],
      "x.left" = q[interval.index],
      "x.right" = q[interval.index + 1],
      "dL" = dL,
      "interval.index" = interval.index,
      "interval.width" = interval.width
    )
    }
    DT
  })
  names(deriv) = S
  
  return(deriv)
}

ale_curve = function(effect) {
 
  effect_cum = lapply(effect, function(feat){
    res = ale(feat)
    list("mean_effect" = res$mean_effect, "sd_effect" = res$sd_effect)
  })
  names(effect_cum) = names(effect)
  
  return(effect_cum)
  
}

ale = function(feat){
  cumsum_na = function(values) {
    values[is.na(values)] = 0
    cumsum(values)
  }
  
  feat$dL[feat$dL == 0] = NA
  
  # average over instances within each interval
  setkeyv(feat, c("interval.index"))
  delta.aggr = feat[, list(dL = mean(dL, na.rm = TRUE),
                           interval.n = .N), by = c("interval.index", "x.left", "x.right")]
  delta.sd = feat[, list(sd = sd(dL, na.rm = TRUE),
                         interval.n = .N), by = c("interval.index", "x.left", "x.right")]
  
  if(is.numeric(feat$feat.val)){
    # accumulate over the intervals
    delta.acc = delta.aggr[, list(dL.cumsum = cumsum_na(c(0, dL)), index0 = c(0,interval.index), index1 = c(interval.index, max(interval.index)+1))]
  
    # the mean effect is the weighted mean of the interval mid point effects
    # weighted by the number of points in the interval
    fJ0 = delta.acc[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
                                           dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr$interval.n) / sum(delta.aggr$interval.n))]
    
    # centering the ALEs
    fJ = delta.acc[, list(dL = dL.cumsum - fJ0$.ale0,
                          x.grid = c(delta.aggr$x.left, delta.aggr$x.right[length(delta.aggr$x.right)]))]
    
  }
  else if(is.factor(feat$feat.val)){
    delta.acc = delta.aggr[, list(dL.cumsum = dL, index = interval.index)]
    fJ0 = delta.acc[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
                                           dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr$interval.n) / sum(delta.aggr$interval.n))]
    fJ = delta.acc[, list(dL = dL.cumsum - fJ0$.ale0,
                          x.grid = levels(feat$feat.val)[index])]
  }
  
  
  list("mean_effect" = fJ, "sd_effect" = delta.sd)
}


#---------------------------------------------------------------------------------------------------
# helper functions for Shapley

shap_values = function(predictor, S, data, target){
  X = data[, setdiff(colnames(data), target)]
  data_shap = X[FALSE,S, drop = FALSE]
  colnames(data_shap) = paste0("phi_",S)
  for(i in 1:nrow(data)){
    shap = Shapley$new(predictor = predictor, x.interest = X[i,], sample.size = 1000)
    data_shap[i,paste0("phi_",S)] = shap$results$phi[which(shap$results$feature %in% S)]
  }
  DT = lapply(S, function(feat){
    df = data.table("feat.val" = X[,feat], "phi" = data_shap[, paste0("phi_",feat)])
  })
  names(DT) = S
  return(DT)
}

fast_shap_values = function(model, S, data, target, nsim = 500, pred.fun, seed = TRUE){
  X = data[, setdiff(colnames(data), target)]
  if(seed) set.seed(101)
    # for reproducibility
  #pred.fun = function(object, newdata) predict(object, newdata = newdata)$data$response
  shap <- fastshap::explain(object = model, feature_names = S, X = X, pred_wrapper = pred.fun, nsim = nsim)
  
  
  DT = lapply(S, function(feat){
    df = data.table("feat.val" = X[,feat], "phi" = as.numeric(unlist(shap[, feat])))
  })
  names(DT) = S
  return(DT)
}



effect_function = function(objective, model, data, S, predict.fun){
  mod <- Predictor$new(model, data = data[which(names(data)!="y")], y = data$y)
  if(objective == "SS_L2_pd") effect = FeatureEffects$new(mod, method = "ice", features = S, grid.size = 20)
  else if(objective == "SS_L2_ale") effect = ale_deriv(model, S, data = data, h = 10, target = "y", predict.fun = predict.fun)
  else if(objective == "SS_L2_shap") effect = fast_shap_values(model, S, data = data, target = "y", nsim = 500, pred.fun = predict.fun)
  return(effect)
}