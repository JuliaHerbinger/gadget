####################################################################################################
# HELPER FUNCTIONS FOR APPLICATIONS
####################################################################################################

regional_plot = function(tree, effect, type){
  
  if(type == "pd"){
    
    effect = effect$results
    eff = lapply(effect, function(feat){
      feature_name = unique(feat$.feature)
      pdps = lapply(2:length(tree), function(depth) {
        
        
        pdp = lapply(1:length(tree[[depth]]), function(node){

          subspace = tree[[depth]][[node]]
          split.value = subspace$split.value
          
          if(is.null(subspace$split.feature) & !is.null(subspace$subset.idx)){
            
            
            grid = subspace$grid[[feature_name]]
            subset.idx = subspace$subset.idx
            data = feat[feat$.id %in% subset.idx,]
             
              pdp = cbind(aggregate(x = .value~.borders, data = data, FUN = mean), depth = subspace$depth, id = node) 
              data_centered = gather(subspace$local[[feature_name]], .borders, .value)
              sd = aggregate(x = .value~.borders, data = data_centered, FUN = sd)
              pdp$lower = pdp$.value-sd$.value*1.96
              pdp$upper = pdp$.value+sd$.value*1.96
              pdp$lower[-which(as.character(pdp$.border) %in% grid)] = pdp$upper[-which(as.character(pdp$.border) %in% grid)] = NA
            #} 
            pdp$.value[-which(as.character(pdp$.border) %in% grid)] = NA
            pdp
          }
        })
        do.call("rbind", pdp)
      })
      
      pdps = list.clean(pdps)
      pdps = do.call("rbind",pdps)
      pdps$group = paste0(pdps$depth, pdps$id)
      pdps
    })
  }
  
  else if(type == "ale"){
    
    eff = lapply(names(effect), function(feature_name){
      feat = effect[[feature_name]]
      ale_plots = lapply(2:length(tree), function(depth) {
        
        
        
        ale = lapply(1:length(tree[[depth]]), function(node){
          
          subspace = tree[[depth]][[node]]
          split.value = subspace$split.value
          
          if(is.null(subspace$split.feature) & !is.null(subspace$subset.idx)){
            
            subset.idx = subspace$subset.idx
            data = feat[subset.idx,]
            
            ale_eff_all = ale(data)
            ale_eff = ale_eff_all$mean_effect
            ale_eff$depth = depth
            ale_eff$id = node
            ale_eff
          }
        })
        do.call("rbind", ale)
      })
      
      ales = list.clean(ale_plots)
      ales = do.call("rbind",ales)
      ales$group = paste0(ales$depth, ales$id)
      ales
    })
  }
  
  else if(type == "shap"){
    
    eff = lapply(names(effect), function(feature_name){
      feat = effect[[feature_name]]
      shap_plots = lapply(2:length(tree), function(depth) {
        
        
        
        shap = lapply(1:length(tree[[depth]]), function(node){
          
          subspace = tree[[depth]][[node]]
          split.value = subspace$split.value
          
          
          if(is.null(subspace$split.feature) & !is.null(subspace$subset.idx)){
            
            subset.idx = subspace$subset.idx
            data = subspace$local[[feature_name]]
            
            if(is.numeric(data$feat.val)){
              gam_mod = mgcv::gam(phi~s(feat.val, k = 3, bs="cs"), data = data)
              pred = predict(gam_mod, data, se.fit = TRUE)
              data$phi_pred = pred$fit
          
            }
            else{
              data$phi_pred = data$phi
              data_aggr = cbind(aggregate(x = phi~feat.val, data = data, FUN = mean)) 
              sd = aggregate(x = phi~feat.val, data = data, FUN = sd)
              data_aggr$lower = data_aggr$phi-sd$phi*1.96
              data_aggr$upper = data_aggr$phi+sd$phi*1.96
              data = data_aggr
            }
            
            data$depth = depth
            data$id = node
            data
          }
        })
        do.call("rbind", shap)
      })
      
      shaps = list.clean(shap_plots)
      shaps = do.call("rbind",shaps)
      shaps$group = paste0(shaps$depth, shaps$id)
      shaps
    })
  }
  
  names(eff) = names(effect)
  return(eff)
  
}


helper_regional_labeler = function(tree){
  
  e <- new.env()
  e$region_label = list()
 
  lapply(2:length(tree), function(depth) {
    
    lapply(1:length(tree[[depth]]), function(node){
      
      subspace = tree[[depth]][[node]]
      split.value = subspace$split.value
      
      if(length(e$region_label) < depth-1) e$region_label[[depth-1]] = character(length(tree[[depth]]))
      e$region_label[[depth-1]][node] = paste(subspace$split.feature.parent, subspace$child.type, subspace$split.value.parent)
      
    })
  })
}


region_labeler = function(tree, effect_region){
  labels_all = helper_regional_labeler(tree)
  groups = unique(effect_region$id)
  
  labels = lapply(groups, function(group){
    
    for( depth in max(effect_region$depth):2){
      
      if(depth == max(effect_region$depth)){
        label = labels_all[[depth-1]][group]
        parent_node = ceiling(group/2)
      } 
      else {
        label = c(label, labels_all[[depth-1]][parent_node])
        parent_node = ceiling(parent_node/2)
      }
      
    }
    paste0(paste(rev(label), collapse = " & "), ", n = ", length(tree[[max(effect_region$depth)]][[group]]$subset.idx))
  })
  labels
}

plot_regional = function(effect, label, type, color_pal, ymin, ymax){
  if(type == "pd"){
    
    plots = lapply(names(effect), function(feat_name){
      feat = effect[[feat_name]]
      if(is.numeric(feat$.borders)){
        ggplot(feat, aes(x = .borders, y = .value, group = group)) + geom_line(aes(col = group), lwd = 1.5) + theme_bw() +
          ylab(expression(hat(f)[j])) + xlab(feat_name) +  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +ylim(ymin, ymax) + 
          scale_color_manual(labels = unlist(label), values = color_pal[1:length(label)]) 
      }
      else{
        ggplot(feat, aes(x = .borders, y = .value)) + 
          geom_pointrange(aes(ymin = lower, ymax = upper, group = factor(group), col = factor(group)), lwd = 1.5, size = 0.9, position=position_dodge(0.05)) + 
          theme_bw() +
          ylab(expression(hat(f)[j]))+ xlab(feat_name) + ylim(ymin, ymax) + 
          scale_color_manual(labels = unlist(label), values = color_pal[1:length(label)]) 
      }
    })
  }
  else if(type == "ale"){
    
    plots = lapply(names(effect), function(feat_name){
      feat = effect[[feat_name]]
      if(is.numeric(feat$x.grid)){
        ggplot(feat, aes(x = x.grid, y = dL, group = group)) + geom_line(aes(col = group), lwd = 1.5) + theme_bw() +
          ylab("") + xlab(feat_name) +
          scale_color_manual(labels = unlist(label), values = color_pal[1:length(label)]) 
      }
      else{
        ggplot(feat, aes(x = x.grid, y = dL)) + geom_boxplot( aes(col = factor(group)), lwd = 1.5) + theme_bw() +
          ylab("")+ xlab(feat_name) + 
          scale_color_manual(labels = unlist(label)[unique(feat$id[!is.na(feat$dL)])], values = color_pal[unique(feat$id[!is.na(feat$dL)])]) 
      }
    })
  }
  else if(type == "shap"){
    
    plots = lapply(names(effect), function(feat_name){
      feat = effect[[feat_name]]
      if(is.numeric(feat$feat.val)){
        ggplot(feat, aes(x = feat.val, y = phi_pred, group = group)) +  
          geom_point(aes(x = feat.val, y = phi, col = group), alpha = 0.25, size = 0.8) + geom_line(aes(col = group), lwd = 1.2) + 
          theme_bw() +
          ylab(expression(hat(f)[j])) + xlab(feat_name) +  
          ylim(ymin, ymax) + 
          scale_color_manual(labels = unlist(label), values = color_pal[1:length(label)]) 
      }
      else{
        ggplot(feat, aes(x = feat.val, y = phi)) + 
          geom_pointrange(aes(ymin = lower, ymax = upper, group = factor(group), col = factor(group)), lwd = 1.1, size = 0.6, position=position_dodge(0.05)) + 
          theme_bw() +
          ylab(expression(hat(f)[j]))+ xlab(feat_name) + ylim(ymin, ymax) + 
          scale_color_manual(labels = unlist(label)[unique(feat$id[!is.na(feat$phi)])], values = color_pal[unique(feat$id[!is.na(feat$phi)])]) 
      }
    })
  }
}
