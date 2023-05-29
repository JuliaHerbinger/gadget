####################################################################################################
# ANALYSIS OF BIKESHARING DATA
####################################################################################################

# source required functions 
source("R/load_packages.R")
source("R/tree_splitting_multiple.R")
source("R/helper_effects.R")
source("R/applications/helper.R")
source("R/helper_tree_splitting.R")
source("R/interaction_test_all.R")
source("R/pimp.R")

#---------------------------------------------------------------------------------------------------
# 1. load dataset from ISLR2 and convert features

library(ISLR2)
data(Bikeshare)
bike <- data.table(Bikeshare)
bike[, hr := as.numeric(as.character(hr))]
bike[, workingday := as.factor((workingday))]
bike[, season := as.factor(season)]

# feature space
X <- bike[, .(day, hr, temp, windspeed, workingday, hum, season, weathersit, atemp, casual)]

# target
y <- bike$bikers

# analyzed dataset
train1 = cbind(X, "cnt" = y)
# remove data point with weathersit = heavy rain/snow (only one occurence) to use lm within benchmark
data = as.data.frame(train1)[-which(train1$weathersit=="heavy rain/snow"),]
data$weathersit = droplevels(data$weathersit)


#---------------------------------------------------------------------------------------------------
# 2. Model selection via (nested) cross-validation

# create Task
set.seed(123)
task = makeRegrTask(id = "bike", data = data, target = "cnt")
X = data[,setdiff(colnames(data), "cnt")]


# Tuning Settings for svm in the inner resampling loop (RBF kernel, we tune parameters C and sigma)
ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
ctrl = makeTuneControlRandom(maxit = 30L)
inner = makeResampleDesc("CV", iters = 3)
lrn_svm_tuned = makeTuneWrapper("regr.ksvm",
                       resampling = inner, par.set = ps, control = ctrl,
                       show.info = TRUE)


# Learners: compare tuned svm, random forest (ranger), featureless and linear model on outer folds
lrns = list(makeLearner("regr.ranger"), makeLearner("regr.featureless"), makeLearner("regr.lm"), lrn_svm_tuned)

# Outer resampling loop
outer = list(makeResampleDesc("CV", iters = 5))


# Benchmark learners on bikesharing data
res = benchmark(lrns, task, outer,
                measures = list(mlr::mse, mlr::rsq), show.info = TRUE,
                keep.extract = TRUE)
res

# task.id       learner.id mse.test.mean rsq.test.mean
# 1    bike      regr.ranger      1077.441  0.9397499401
# 2    bike regr.featureless     17902.172 -0.0002480515
# 3    bike          regr.lm      6641.436  0.6288811687
# 5    bike  regr.ksvm.tuned      3365.372  0.8113827891


# Random Forest (ranger) performs best --> chosen for analysis 


#---------------------------------------------------------------------------------------------------
# 3. Train random forest on entire dataset and create predictor for analysis of effects

set.seed(123)
model = mlr::train(task = task, learner = makeLearner("regr.ranger"))
predictor = Predictor$new(model, data = X, y = task$env$data$cnt)
predict.function = function(model, newdata) predict(model, newdata = newdata)$data$response

# define model wrapper for PINT
model_wrapper_rf = function(data, target){
  task = makeRegrTask(id = "bike", data = data, target = target)
  learner = makeLearner("regr.ranger")
  model = mlr::train(learner, task)
  predict.function = function(model, newdata) predict(model, newdata = newdata)$data$response
  return(list("model_fit" = model, "predict.function" = predict.function))
}

#---------------------------------------------------------------------------------------------------
# 4. apply PINT to find interacting features -- here with ALE

# calculate null distribution
null_importance_bike = null_int_perm(X = X, y = data$cnt, model = model_wrapper_rf, S = colnames(X), nperm = 500, effect_method = "ale")

# calculate interaction related heterogeneity for each feature on original dataset
int_imp_bike = interaction_importance(X = X, y = data$cnt, model_wrapper_rf, "ale", colnames(X), calc_hstat = FALSE)

# calculate p-value
null_dist = null_importance_bike[["ale"]]
int_val = int_imp_bike$int_imp[["ale"]]
pint = PIMP(int_val, null_dist, method = NA)
# > pint
# day           hr         temp    windspeed   workingday          hum       season   weathersit        atemp       casual 
# 1.000000e+00 9.802503e-31 1.000000e+00 1.000000e+00 1.376545e-16 1.000000e+00 9.910121e-01 1.000000e+00 1.000000e+00 1.000000e+00 

# only hr and workingday are significant (included in S)


#---------------------------------------------------------------------------------------------------
# 5. calculate GADGET for S based on ALE
effect_ale = ale_deriv(object = model, S =  c("hr", "workingday"), data = data, h = 10, target = "cnt", predict.fun = predict.function)
tree = compute_tree(effect_ale, data, model = model,predict.function = predict.function, objective = "SS_L2_ale", 
                    Z = c("hr", "workingday"), n.split = 2, impr.par = 0.1, n.quantiles = 100, min.split = 50, 
                    shap.recalc = TRUE, store.data = TRUE)


# Create plots for Figures in Section 7

# Global effect plots for hr
eff_root = ale(effect_ale$hr)
mean_root = eff_root$mean_effect
sd_root = eff_root$sd_effect

p_ale  = ggplot() + geom_line(data = mean_root, aes(x = x.grid, y = dL), col = "black", lwd = 1.5) + 
  theme_bw() + xlab("hr") + ylab(expression(hat(f)["hr"])) + ylim(-80,110)
p_sd = ggplot() + geom_line(data = sd_root, aes(x = (x.right+x.left)/2, y = sd), col = "orange", lwd = 1.5) +
  theme_bw() + xlab("") + ylab("sd(deriv)") + ylim(0, 80)


# Regional effect plots for hr
eff =regional_plot(tree, effect_ale, "ale")
plot = plot_regional(eff, c("No workingday", "Workingday"), "ale", brewer.pal(11, "RdYlBu")[c(8,10)])

# calculate sd for each region
sd_left = ale(tree[[2]][[1]]$local$hr)$sd_effect
sd_left$id = 1
sd_right = ale(tree[[2]][[2]]$local$hr)$sd_effect
sd_right$id = 2
sd_child = rbind(sd_left, sd_right)

# create regional plots 
p_mean_child1 = plot[[1]] + ylim(-80,110)
p_sd_child1 = ggplot() + 
  geom_line(data = sd_child, aes(x = (x.right+x.left)/2, y = sd, group = factor(id), col = factor(id)), lwd = 1.5) +
  theme_bw() + xlab("") + ylab("") + ylim(0, 80) + 
  scale_color_manual(labels = c("workingday = No", "workingday = Yes"), values = brewer.pal(11, "RdYlBu")[c(5,4)])  
  

#---------------------------------------------------------------------------------------------------
# 6.) Use also temp in S (might be meaningful from a domain perspective)
effect_ale = ale_deriv(object = model, S =  c("hr", "workingday", "temp"), data = data, h = 10, target = "cnt", predict.fun = predict.function)
tree = compute_tree(effect_ale, data, model = model,predict.function = predict.function, objective = "SS_L2_ale", 
                    Z = c("hr", "workingday", "temp"), n.split = 2, impr.par = 0.01, n.quantiles = 100, min.split = 50, 
                    shap.recalc = TRUE, store.data = TRUE)

# Regional effect plots for hr
eff =regional_plot(tree, effect_ale, "ale")
plot = plot_regional(eff, c("No workingday", "Workingday & temp <= 0.49", "Workingday & temp > 0.49"), "ale", brewer.pal(11, "RdYlBu")[c(8,9,11)])

# calculate sd for each region
sd_left = ale(tree[[2]][[1]]$local$hr)$sd_effect
sd_left$id = 1
sd_right1 = ale(tree[[3]][[3]]$local$hr)$sd_effect
sd_right1$id = 2
sd_right2 = ale(tree[[3]][[4]]$local$hr)$sd_effect
sd_right2$id = 3
sd_child = rbind(sd_left, sd_right1, sd_right2)

# create regional plots 
p_mean_child = plot[[1]] + ylim(-80,110)
p_sd_child = ggplot() + 
  geom_line(data = sd_child, aes(x = (x.right+x.left)/2, y = sd, group = factor(id), col = factor(id)), lwd = 1.5) +
  theme_bw() + xlab("") + ylab("") + ylim(0, 80)
  scale_color_manual(labels = c("No workingday", "Workingday & temp <= 0.49", "Workingday & temp > 0.49"), 
                     values = brewer.pal(11, "RdYlBu")[c(5,4,3)])   
  


#---------------------------------------------------------------------------------------------------
# create and save final plots (Figure in Section 7)
p = ((p_sd/p_ale) + 
       plot_layout(heights = c(1,3))) | ((p_sd_child1/p_mean_child1) + plot_layout(heights = c(1,3)))  | ((p_sd_child/p_mean_child) + plot_layout(heights = c(1,3))) 
p = p & theme(legend.position = "right", legend.title = element_blank())
ggsave(p, filename = "figures/bike_ale.pdf", width = 11, height = 4)



####################################################################################################
# EXAMPLE FOR INTRO

# Calculate GADGET for S based on PD
set.seed(123)
ids = sample(1:nrow(data), 500) # sample ICE curves for illustration purposes
predictor = Predictor$new(model, data = data)
effect_pd = FeatureEffects$new(predictor, method = "ice", features = c("hr"), grid.size = 10)
tree = compute_tree(effect_pd, data, model = model,predict.function = predict.function, objective = "SS_L2_pd", 
                    Z = c("workingday"), n.split = 1, impr.par = 0.1, n.quantiles = 100, min.split = 50, 
                    shap.recalc = TRUE, store.data = TRUE)

# Global and regional effects for PD
reg_pd =regional_plot(tree, effect_pd, "pd")
glo_pd = aggregate(.value~.borders, data = effect_pd$results$hr[effect_pd$results$hr$.id %in% ids,], FUN = mean)
ice = effect_pd$results$hr[effect_pd$results$hr$.id %in% ids,]
ice$group = ice$.id %in% which(data$workingday=="1")

# create global and regional effect plots
glo_pd_bike = ggplot() +
  geom_line(data = ice, aes_string(x = ".borders", y = ".value", group = ".id"), alpha = 0.4) +
  geom_line(data = glo_pd, aes_string(x = ".borders", y = ".value"), col = "lightgrey", lwd = 2) + 
  theme_bw() + ylab(expression(hat(f)[hr])) + xlab("hr")
reg_pd_bike = ggplot() +
  geom_line(data = ice, aes_string(x = ".borders", y = ".value", group = ".id", color = "group"), alpha = 0.6) +
  geom_line(data = reg_pd$hr[reg_pd$hr$group=="21",], aes_string(x = ".borders", y = ".value"), col = brewer.pal(11, "RdBu")[3], lwd = 2) + 
  geom_line(data = reg_pd$hr[reg_pd$hr$group=="22",], aes_string(x = ".borders", y = ".value"), col = brewer.pal(11, "RdBu")[9], lwd = 2) + 
  scale_color_manual(labels = c("No workingday", "Workingday"), values = brewer.pal(11, "RdBu")[c(4,8)], name = "") +
  theme_bw() + ylab(expression(hat(f)[hr])) + xlab("hr")


# save final plots (Figure in Section 1)
ggsave(glo_pd_bike + reg_pd_bike, filename = "figures/bike_pd.pdf", width = 8, height = 3.5)
