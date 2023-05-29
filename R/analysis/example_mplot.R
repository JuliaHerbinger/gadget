#---------------------------------------------------------------------------------------------------
# APPENDIX A: POSITIVE LINEAR CORRELATION + NEGATIVE LINEAR RELATIONSHIP (MPLOT FAILS)
#---------------------------------------------------------------------------------------------------

source("R/load_packages.R")

# generate data
create_lin_corr = function(n, seed){
  set.seed(seed)
  x = rmvnorm(n, mean = c(0,0), sigma = matrix(c(1,0.9,0.9,1), ncol=2))
  X = data.frame("x1" = x[,1], "x2"=x[,2])
  y = -X$x1 + 2*X$x2 + rnorm(n, sd = 0.2)
  data.frame(X, y)
}
data = create_lin_corr(n = 500, seed=123)
X = data[,setdiff(names(data),"y")]


# Modelling
task = makeRegrTask(data = data, target = "y")


set.seed(123)
lrn = makeLearner("regr.lm")
model = mlr::train(task = task, learner = lrn)
testset = create_lin_corr(n = 10000, seed=234)
pred = predict(model, newdata = testset)$data
measureRSQ(pred$truth, pred$response)


# Interpretation
mod <- Predictor$new(model, data = data[which(names(data)!="y")], y = data$y)


# ICE and PDP
eff <- FeatureEffect$new(mod, feature = "x1", grid.size = 20, method = "pdp+ice")

p_ice = ggplot() + geom_line(data = eff$results[eff$results$.type == "ice",], aes(x = x1, y = .value, group = .id), alpha = 0.2) + 
  geom_line(data = eff$results[eff$results$.type == "pdp",], aes(x = x1, y= .value), color = "yellow", size = 2)+ theme_bw() +
  ylab(expression(hat(f)[1]))


# ICE RESTRICTED
ice = eff$results[eff$results$.type=="ice",]

# function to define grid for restricted ice curves based on correlation with other features
ice_restricted = function(ice, X, p = 0.05){
  feat = names(ice)[1]
  features = X[, setdiff(names(X), feat), drop = FALSE]
  for(g in unique(ice[,feat])){
    
    for(f in features){
      range_f = range(f[X[,feat]>(g-p*(max(X[,feat])-min(X[,feat]))) & X[,feat]<=(g+p*(max(X[,feat])-min(X[,feat])))])
      ice[ice[,feat]==g,".value"][which(f<range_f[1] | f>range_f[2])] = NA
    }
  }
  ice
}

ice_rest = ice_restricted(ice, data[,setdiff(names(data), "y")])

# create restricted ICE plot (including conditional PD plot (meaning MPlot))
pdp_rest <- ice_rest %>% 
  dplyr::group_by(x1) %>% 
  dplyr::summarise(pdp = mean(.value, na.rm = TRUE))

p_rice = ggplot(data = ice_rest) + geom_line(aes(x = x1, y = .value, group = .id), alpha = 0.4) + 
  geom_line(data = pdp_rest, aes(x = x1, y= pdp), color = "yellow", size = 2)+ theme_bw() +
  ylab(expression(hat(f)[1]))
p_rice

ggsave(filename = "figures/sim2_pdp_price.pdf", plot = ggarrange(p_ice,p_rice), width = 8, height = 4)
