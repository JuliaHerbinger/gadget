#---------------------------------------------------------------------------------------------------
# FUNCTIONS TO APPROXIMATE NULL DISTRIBUTION AND CALCULATE P VALUE
# The code is for the approximation and p-value caluclation is taken from Altmann et al:
# https://github.com/andrealtmann/PIMP
#---------------------------------------------------------------------------------------------------


## fitting a gamma distribution to a population
## return a the shape and scale parameter
gamma.param <- function(x){
  s = log(mean(x))-mean(log(x))
  k=(3-s+sqrt((s-3)^2+24*s))/(12*s)
  scale = mean(x)/k
  shape = k
  return(c(scale, shape))
}



PIMP <- function(imp, rnd, two.sided=F, log.p=F, method = "nonpar", minsize = 10, ...){
  if(!(method %in% c(NA, "nonpar", "normal", "lognormal", "gamma")))
  {print("Incorrect or not supported method."); return(-1)}
  
  ## parameters if distribution is normal
  if(method %in% c(NA, "normal")){
    m.norm = apply(rnd, 2, mean)
    sd.norm = apply(rnd, 2, sd)
  }
  
  ## parameters if distribution is lognormal
  if(method %in% c(NA, "lognormal")){
    m.lnorm = apply(rnd, 2, function(x){
      if(sum(x>0)>=minsize) {return(mean(log(x[x>0])))} 
      else {print("Not enough positive observations for estimation of mean.");return(NA)}
    })  # mean of the positive observations
    sd.lnorm = apply(rnd, 2, function(x){
      if(sum(x>0)>=minsize) {return(sd(log(x[x>0])))} 
      else {print("Not enough positive observations for estimation of stdev.");return(NA)}
    }) # sd of the positive observations
  }
  
  ## parameters if the distribution is gamma
  if(method %in% c(NA, "gamma")){
    s = apply(rnd, 2, function(x) {
      if(sum(x>0)>=minsize) {return(log(mean(x[x>0]))-mean(log(x[x>0])))}
      else{print("Not enough positive observations for parameters estimation.");return(NA)}
    })
    k = sapply(s, function(x) {(3-x+sqrt((x-3)^2+24*x))/(12*x)} )
    scale.gamma = 1/k*apply(rnd, 2, function(x){
      if(sum(x>0)>minsize){return(mean(x[x>0]))}
      else{return(NA)}
    })
    shape.gamma = k
  }
  
  ## find the best fitting distribution
  if(is.na(method)){
    test.norm <- test.lognorm <- test.gamma <- NULL	
    for(i in 1:length(imp)){
      test.norm = c(test.norm, ks.test(rnd[,i], "pnorm", mean = m.norm[i], sd = sd.norm[i])$p.value)
      
      if(is.na(m.lnorm[i]+sd.lnorm[i])){test.lognorm <- c(test.lognorm, NA)}
      else{test.lognorm = c(test.lognorm, ks.test(rnd[rnd[,i]>0,i], "plnorm", meanlog = m.lnorm[i], sdlog = sd.lnorm[i])$p.value)}
      
      if(is.na(scale.gamma[i]+shape.gamma[i])){test.gamma <- c(test.gamma, NA)}
      else{test.gamma = c(test.gamma, ks.test(rnd[rnd[,i]>0,i], "pgamma", scale = scale.gamma[i], shape = shape.gamma[i])$p.value)}
    }
    
    # the 0.05 quantile p-value for each variable is computed. Greater p-values show evidence that a certain distribution is appropriated
    min.pvals = c(quantile(test.norm, 0.05, na.rm = T), quantile(test.lognorm, 0.05, na.rm = T), quantile(test.gamma, 0.05, na.rm = T))	
    names(min.pvals) <- c("normal", "lognormal", "gamma")
    print(min.pvals)
    maxpval = max(min.pvals)  # the best fitting distribution
    if(maxpval < 0.01) {
      method <- "nonpar"
      print("No parametric distribution fits well. Non-parametric estimation is used.")}
    else{
      method <- names(min.pvals)[which(min.pvals == maxpval)]
      print(paste(method, " distribution is fitted to the nulls."))
    }
  } 
  
  ## non-parametric estimation
  if(method == "nonpar"){
    res <- c()
    for(i in 1:length(imp)){
      #compute 'upper' p-value
      pv <-sum(rnd[,i]>=imp[i])/length(rnd[,i])
      
      #if two.sided compute also the 'lower' p-value
      if (two.sided==T) {pv <- min(pv, sum(rnd[,i]<=imp[i])/length(rnd[,i]))}
      
      #transfrom to -log10(p-value)
      if (log.p==T) {pv <- pv / -log(10)}
      
      res <- c(res, pv)
    }
  }
  
  ## Fitting a normal distribution
  if(method == "normal"){
    res <- c()
    for(i in 1:length(imp)){
      #compute 'upper' p-value
      pv <- pnorm(imp[i], m.norm[i], sd.norm[i], lower.tail=F, log.p=log.p)
      
      #if two.sided compute also the 'lower' p-value
      if (two.sided==T)
        pv <- min(pv, pnorm(imp[i], m.norm[i], sd.norm[i], lower.tail=T, log.p=log.p))
      
      #transfrom to -log10(p-value)
      if (log.p==T) {pv <- pv / -log(10)}
      
      res <- c(res, pv)
    }
  }
  
  ## Fitting a lognormal distribution 
  if(method == "lognormal"){
    res <- c()
    for(i in 1:length(imp)){
      if(is.na(m.lnorm[i]+sd.lnorm[i])){   # parameters could not be estimated => nonparametric estimation
        pv <-sum(rnd[,i]>=imp[i])/length(rnd[,i])
        if (two.sided==T) {pv <- min(pv, sum(rnd[,i]<=imp[i])/length(rnd[,i]))}
      }
      else{  # lognormal fitting
        #compute 'upper' p-value
        pv <- plnorm(imp[i], m.lnorm[i], sd.lnorm[i], lower.tail=F, log.p=log.p)
        
        #if two.sided compute also the 'lower' p-value
        if (two.sided==T)
          pv <- min(pv, plnorm(imp[i], m.lnorm[i], sd.lnorm[i], lower.tail=T, log.p=log.p))
      }
      #transfrom to -log10(p-value)
      if (log.p==T) {pv <- pv / -log(10)}
      
      res <- c(res, pv)
    }
  }
  
  ## Fitting a gamma distribution
  if(method == "gamma"){
    res <- c()
    for(i in 1:length(imp)){
      if(is.na(scale.gamma[i]+shape.gamma[i])){   # parameters could not be estimated => nonparametric estimation
        pv <-sum(rnd[,i]>=imp[i])/length(rnd[,i])
        if (two.sided==T) {pv <- min(pv, sum(rnd[,i]<=imp[i])/length(rnd[,i]))}
      }
      else{  # gamma fitting
        #compute 'upper' p-value
        pv <- pgamma(imp[i], scale = scale.gamma[i], shape = shape.gamma[i], lower.tail=F, log.p=log.p)
        
        #if two.sided compute also the 'lower' p-value
        if (two.sided==T)
          pv <- min(pv, pgamma(imp[i], scale = scale.gamma[i], shape = shape.gamma[i], lower.tail=T, log.p=log.p))
      }
      #transfrom to -log10(p-value)
      if (log.p==T) {pv <- pv / -log(10)}
      
      res <- c(res, pv)
    }
  }
  
  return(res)
}