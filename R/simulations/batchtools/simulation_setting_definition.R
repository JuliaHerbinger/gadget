#---------------------------------------------------------------------------------------------------
# DEFINE SIMULATION SETTINGS
#---------------------------------------------------------------------------------------------------

create_sim_data = function(job, n, type, cor, dep, beta, noise, ...){
  
  if(type == "xor"){
    
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x1 = (cor)*x3 + (1-cor)*runif(n, -1, 1) 
    y = ifelse(x3>0, 3*x1, -3*x1) + x3 + rnorm(n, sd = 0.3)
    data = data.frame(x1,x2,x3, y)
    
    X = data[,setdiff(names(data),"y")]
    S = Z = features = colnames(X)
    
  }

  if(type == "categorical_linear1"){
    
    x1 = runif(n, 0, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x4 = runif(n, -1, 1)
    x5 = runif(n, -1, 1)
    
    formula = ifelse(x3 <= 0, ifelse(x4 > 0, I(x1),I(3*(x1))),ifelse(x5 <= 0, I(-x1),I(-3*(x1))))  
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    
    data = data.frame(mget(paste0("x",1:5)), y)
    X = data[,setdiff(names(data),"y")]
    S = "x1" 
    features = colnames(X)
    Z = setdiff(features, S)
  }
  
  if(type == "categorical_linear2"){
    
    x1 = runif(n, 0, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x4 = runif(n, -1, 1)
    x5 = runif(n, -1, 1)
    
    formula = ifelse(x3 <= 0, ifelse(x4 > 0, I(x1),I(4*(x1))),ifelse(x5 <= 0, ifelse(x2 > 0, I(-x1), I(-3*x1)), I(-5*(x1))))  
   
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    
    data = data.frame(mget(paste0("x",1:5)), y)
    X = data[,setdiff(names(data),"y")]
    S = "x1" 
    features = colnames(X)
    Z = setdiff(features, S)
  }
  
  
  if(type == "spur_pint"){
    x1 = runif(n, -1,1)
    x2 = runif(n, -1,1)
    x3 = x2 + rnorm(n, 0, 0.3)
    x4 = runif(n, -1,1)
    X = data.frame(x1,x2,x3,x4)
    
    y = x1 + x2 + x3 - 2*x1*x2
    data = data.frame(mget(paste0("x",1:4)), y)
    X = data[,setdiff(names(data),"y")]
    S = Z = features = colnames(X)
  }
  
  if(type == "spur_lin"){
    x1 = runif(n, -1,1)
    x2 = runif(n, -1,1)
    
    if(dep == "high") x3 = x2 + rnorm(n, 0, 0.3)
    else if(dep == "medium")  x3 = 0.37*x2 + (0.63)*runif(n, -1, 1) 
    else if(dep == "no") x3 = runif(n, -1, 1)
    x4 = runif(n, -1,1)
    X = data.frame(x1,x2,x3,x4)
    formula = x1 + x2 + x3 - beta*x1*x2
    
    if(noise == "no") y = formula
    else if(noise == "yes") {
      eps = rnorm(n, 0, sd(formula)*0.1)
      y = formula + eps
    } 
    data = data.frame(mget(paste0("x",1:4)), y)
    X = data[,setdiff(names(data),"y")]
    S = Z = features = colnames(X)
  }
  
  if(type == "spur_nonlin"){
    x1 = runif(n, -1,1)
    x2 = runif(n, -1,1)
    
    if(dep == "high") x3 = x2 + rnorm(n, 0, 0.3)
    else if(dep == "medium") x3 = 0.37*x2 + (0.63)*runif(n, -1, 1) 
    else if(dep == "no") x3 = runif(n, -1, 1)
    x4 = runif(n, -1,1)
    X = data.frame(x1,x2,x3,x4)
    formula = (x1)^2 + (x2)^3 + exp(x3) - beta*x1*x2
    
    if(noise == "no") y = formula
    else if(noise == "yes") {
      eps = rnorm(n, 0, sd(formula)*0.1)
      y = formula + eps
    } 
    data = data.frame(mget(paste0("x",1:4)), y)
    X = data[,setdiff(names(data),"y")]
    S = Z = features = colnames(X)
  }
  
  if(type == "spur_int_non"){
    x1 = runif(n, -1,1)
    x2 = runif(n, -1,1)
    
    if(dep == "high") x3 = exp(x2) + rnorm(n, 0, 0.1)
    else if(dep == "medium") x3 = 0.37*x2 + (0.63)*runif(n, -1, 1) 
    else if(dep == "no") x3 = runif(n, -1, 1)
    x4 = runif(n, -1,1)
    X = data.frame(x1,x2,x3,x4)
    formula = beta*x1*exp(x2)
    
    if(noise == "no") y = formula
    else if(noise == "yes") {
      eps = rnorm(n, 0, 1)
      y = formula + eps
    } 
    data = data.frame(mget(paste0("x",1:4)), y)
    X = data[,setdiff(names(data),"y")]
    S = Z = features = colnames(X)
  }
  
  if(type == "spur_int"){
    x1 = runif(n, -1,1)
    x2 = runif(n, -1,1)
    
    if(dep == "high") x3 = x2 + rnorm(n, 0, 0.3)
    else if(dep == "medium") x3 = 0.37*x2 + (0.63)*runif(n, -1, 1) 
    else if(dep == "no") x3 = runif(n, -1, 1)
    x4 = runif(n, -1,1)
    X = data.frame(x1,x2,x3,x4)
    formula = beta*x1*x2
    
    if(noise == "no") y = formula
    else if(noise == "yes") {
      eps = rnorm(n, 0, 1)
      y = formula + eps
    } 
    data = data.frame(mget(paste0("x",1:4)), y)
    X = data[,setdiff(names(data),"y")]
    S = Z = features = colnames(X)
  }

  
  return(list("data" = data, "S" = S, "Z" = Z))
}
