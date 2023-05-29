#---------------------------------------------------------------------------------------------------
# HELPER FUNCTIONS FOR PLOTTING
#---------------------------------------------------------------------------------------------------


# generate data for regional plots
regional_pd = function(effect, tree, depth){
  
  nodes = tree[[depth]]
  ice_curves = effect$results
  ice_centered = lapply(names(ice_curves), function(feat){
    ice = mean_center_ice(ice_curves[[feat]][, - which(colnames(ice_curves[[feat]])==".feature")], ".borders")
    grid_total = colnames(ice)
    ice$node = NA
    for(node_num in 1:length(nodes)){
      node = nodes[[node_num]]
      subset = node$subset.idx
      grid = node$grid[[feat]]
      ice$node[subset] = node_num
      if(length(grid) < length(grid_total)){
        ice[subset, which(!(grid_total %in% grid))] = NA
        ice[subset, grid_total] = ice[subset, ..grid_total] - rowMeans(ice[subset, ..grid_total], na.rm = TRUE)
      }
      
    }
    return(ice)
  })
  names(ice_centered) = names(ice_curves)
  return(ice_centered)
}


# generate regional pd plot
regional_pd_plot = function(effect, node_num, color_ice, color_pd, ymin, ymax){
  plot = lapply(names(effect), function(feat){
    
    data = effect[[feat]]
    data_subset = data[data$node == node_num,]
    ice = gather(data_subset, grid, value)
    ice$grid = as.numeric(ice$grid)
    ice$id = rep(1:nrow(data_subset), length(unique(ice$grid)))
    pdp_centered = ice %>% dplyr::group_by(grid) %>% dplyr::summarise(pdp = mean(value, na.rm = TRUE))
    
    p_centered_ice  = ggplot() + geom_line(data = ice, aes(x = grid, y = value, group = id), alpha = 0.4, col = color_ice) + 
      geom_line(data = pdp_centered, aes(x = grid, y = pdp), col = color_pd, lwd = 1.5) +
      theme_bw() + xlab(feat) + ylab(expression(hat(f)[j])) + ylim(ymin,ymax)
    p_centered_ice
    
  })
  return(plot)
}

# generate regional ale plot
regional_ale_plot = function(effect, color_ale, ymin, ymax, ymin2, ymax2, xmin = NULL, xmax = NULL){
  plot = lapply(names(effect), function(feat){
    
    mean_data = effect[[feat]]$mean_effect
    sd_data = effect[[feat]]$sd_effect
    
    p_ale  = ggplot() + geom_line(data = mean_data, aes(x = x.grid, y = dL), col = color_ale, lwd = 1.5) + 
      theme_bw() + xlab(feat) + ylab(expression(hat(f)[j])) + ylim(ymin,ymax)
    p_sd = ggplot() + geom_line(data = sd_data, aes(x = (x.right+x.left)/2, y = sd), col = "orange", lwd = 1.5) +
      theme_bw() + xlab("") + ylab("sd(deriv)") + ylim(ymin2, ymax2)
    if(!is.null(xmin)){
      p_ale = p_ale + xlim(xmin, xmax)
      p_sd = p_sd + xlim(xmin, xmax)
    }
    list(p_sd, p_ale)
    
  })
  names(plot) = names(effect)
  return(plot)
}

# generate regional shap plot
regional_shap_plot = function(effect, color_local, color_global, ymin, ymax, xmin = NULL, xmax = NULL){
  plot = lapply(names(effect), function(feat){
    #browser()
    data_shap = effect[[feat]]
    gam_mod = mgcv::gam(phi~s(feat.val, k = 3), data = data_shap)
    
    p_shap  = ggplot() + geom_point(data = data_shap, aes(x = feat.val, y = phi), col = color_local, alpha = 0.4) +
      geom_line(data = data_shap, aes(x = feat.val, y = gam_mod$fitted.values), col = color_global, lwd = 1.5) +
      theme_bw() + xlab(feat) + ylab(expression(hat(f)[j])) + ylim(ymin,ymax)
    
    if(!is.null(xmin)){
      p_shap = p_shap + xlim(xmin, xmax)
    }
    p_shap
    
  })
  names(plot) = names(effect)
  return(plot)
}



# create explanation plots: pdp
plot_pdp_split = function(sp_L2, eff, data, feature_num){
  
  feature = names(data)[feature_num]
  ice_right = eff$results[[feature]][which(eff$results[[feature]]$.id %in% which(data[,sp_L2$feature[sp_L2$best.split]]>sp_L2$split.points[[which(sp_L2$best.split)]])),]
  ice_left = eff$results[[feature]][which(eff$results[[feature]]$.id %in% which(data[,sp_L2$feature[sp_L2$best.split]]<=sp_L2$split.points[[which(sp_L2$best.split)]])),]
  pdp = aggregate(.value~.borders, data = eff$results[[feature]], FUN = mean)
  pdp_right = aggregate(.value~.borders, data = ice_right, FUN = mean)
  pdp_left = aggregate(.value~.borders, data = ice_left, FUN = mean)
  colnames(pdp)[1] = colnames(pdp_right)[1] = colnames(pdp_left)[1] = feature
  
  p_pdp = ggplot() +
    geom_line(data = ice_right, aes_string(x = ".borders", y = ".value", group = ".id"), color = brewer.pal(11, "RdBu")[4], alpha = 0.6) +
    geom_line(data = ice_left, aes_string(x = ".borders", y = ".value", group = ".id"), color = brewer.pal(11, "RdBu")[8], alpha = 0.6) +
    geom_line(data = pdp_right, aes_string(x = feature, ".value"), lwd = 2, color = brewer.pal(11, "RdBu")[3]) + 
    geom_line(data = pdp_left, aes_string(x = feature, ".value"), lwd = 2, color = brewer.pal(11, "RdBu")[9]) + 
    geom_line(data = pdp, aes_string(x = feature, ".value"), color = "darkgrey", lwd = 2) +
    geom_point(data = data, aes_string(x = feature, y = "y"), alpha = 0.4) +
    theme_bw() + ylab(bquote(hat(f)[ .(feature_num)])) + xlab(feature) +
    geom_rug(data = data, aes_string(x = feature), color = ifelse(data[,sp_L2$feature[sp_L2$best.split]] > sp_L2$split.points[[which(sp_L2$best.split)]], brewer.pal(11, "RdBu")[4], brewer.pal(11, "RdBu")[8]))
  p_pdp
  
}

# create explanation plots: ale
plot_ale_split = function(ale_preds){
 
  # average over instances within each interval
  setkeyv(ale_preds, c("interval.index"))
  delta.aggr = ale_preds[,list(dL = mean(dL, na.rm = TRUE), interval.n = .N), by = c("interval.index", "x.left", "x.right")]
  delta.aggr1 = ale_preds[ale_preds$node==1, list(dL = mean(dL, na.rm = TRUE), interval.n = .N), by = c("interval.index", "x.left", "x.right")]
  delta.aggr2 = ale_preds[ale_preds$node==2, list(dL = mean(dL, na.rm = TRUE), interval.n = .N), by = c("interval.index", "x.left", "x.right")]
  
  # accumulate over the intervals
  delta.acc = delta.aggr[, list(dL.cumsum = cumsum_na(c(0, dL)), index0 = c(0,interval.index), index1 = c(interval.index, max(interval.index)+1))]
  delta.acc1 = delta.aggr1[, list(dL.cumsum = cumsum_na(c(0, dL)), index0 = c(0,interval.index), index1 = c(interval.index, max(interval.index)+1))]
  delta.acc2 = delta.aggr2[, list(dL.cumsum = cumsum_na(c(0, dL)), index0 = c(0,interval.index), index1 = c(interval.index, max(interval.index)+1))]
  
  # the mean effect is the weighted mean of the interval mid point effects
  # weighted by the number of points in the interval
  fJ0 = delta.acc[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
                                         dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr$interval.n) / sum(delta.aggr$interval.n))]
  fJ01 = delta.acc1[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
                                           dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr1$interval.n) / sum(delta.aggr1$interval.n))]
  fJ02 = delta.acc2[, list(.ale0 = sum(((dL.cumsum[1:(nrow(.SD) - 1)] +
                                           dL.cumsum[2:nrow(.SD)]) / 2) * delta.aggr2$interval.n) / sum(delta.aggr2$interval.n))]
 
  # centering the ALEs
  fJ = delta.acc[, list(dL = dL.cumsum - fJ0$.ale0, x.grid = c(delta.aggr$x.left, delta.aggr$x.right[length(delta.aggr$x.right)]))]
  fJ1 = delta.acc1[, list(dL = dL.cumsum - fJ01$.ale0, x.grid = c(delta.aggr1$x.left, delta.aggr1$x.right[length(delta.aggr1$x.right)]))]
  fJ2 = delta.acc2[, list(dL = dL.cumsum - fJ02$.ale0, x.grid = c(delta.aggr2$x.left, delta.aggr2$x.right[length(delta.aggr2$x.right)]))]
  p_ale = ggplot()  + geom_point(aes(x = feat.val, y = y-mean(y), color = node ),data = ale_preds, alpha = 0.5) + 
    geom_line(data = fJ, aes(x = x.grid, y = dL), color = "darkgrey", lwd = 2) + 
    geom_line(data = fJ1, aes(x = x.grid, y = dL), color = brewer.pal(11, "RdBu")[9], lwd = 2) + 
    geom_line(data = fJ2, aes(x = x.grid, y = dL), color = brewer.pal(11, "RdBu")[3], lwd = 2) +
    geom_vline(data = data.frame(interval = c(min(ale_preds$x.left),unique(ale_preds$x.right))), aes(xintercept = interval), lty = 2) + theme_classic()
  p_ale
  
}


