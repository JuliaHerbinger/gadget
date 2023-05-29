####################################################################################################
# APPENDIX EXAMPLE FOR PINT/SPURIOUS INTERACTION (APPENDIX D)
####################################################################################################

source("R/load_packages.R")

# create data (of spurious interaction example Section 6.3)
n = 500
set.seed(123)
x1 = runif(n, -1,1)
x2 = runif(n, -1,1)
x3 = x2 + rnorm(n, 0, 0.3)
x4 = runif(n, -1,1)
X = data.frame(x1,x2,x3,x4)

y = x1 + x2 + x3 - 2*x1*x2
data = data.frame(mget(paste0("x",1:4)), y)


# Fit SVM (as in Section 6.3)
task = makeRegrTask(data = data, target = "y")
learner = makeLearner("regr.ksvm")
model = mlr::train(learner, task)
pred = Predictor$new(model, data = X, y = data$y)

# mean-centering function for ice curves
mean_center_ice = function(ice, feature){
  Y = tidyr::spread(ice, feature, .value)
  Y = Y[, setdiff(colnames(Y), c(".type", ".id"))]
  
  # center ICE curves by their mean
  Y = Y - rowMeans(Y)
  setDT(Y)
}

# calculate mean-centered ice curves
effect = FeatureEffects$new(pred, method = "ice")
ice_curves = effect$results
ice_centered = lapply(ice_curves, function(feat){
  ice = mean_center_ice(feat[, - which(colnames(feat)==".feature")], ".borders")
  ice = gather(ice, grid, value)
  ice$grid = as.numeric(ice$grid)
  ice$id = rep(1:n, length(unique(ice$grid)))
  ice$feature = feat$.feature[1]
  return(ice)
})
ice_centered = rbindlist(ice_centered)
pdp_centered = ice_centered %>% dplyr::group_by(feature, grid) %>% dplyr::summarise(pdp = mean(value))

# create plot for mean-centered ice curves of all features
p_centered_ice = ggplot(ice_centered) + geom_line(aes(x = grid, y = value, group = id), alpha = 0.4) + 
  geom_line(data = pdp_centered, aes(x = grid, y = pdp), col = "lightgrey", lwd = 1.5) +
  facet_grid(.~feature) + theme_bw() + xlab("") + ylab(expression(hat(f)[j]))
ggsave("figures/test_centered_ice.pdf", p_centered_ice, width = 6, height = 2.1)

