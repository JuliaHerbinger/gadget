#------------------------------------------------------------------------------------------------------------------------------------
#
# PLOTS FOR EXAMPLES IN SECTION 3 + 4 AND RESPECTIVE APPENDICES
#
#------------------------------------------------------------------------------------------------------------------------------------

# Load packages and functions
source("R/load_packages.R")
source("R/helper_tree_splitting.R")
source("R/tree_splitting.R")
source("R/helper_plots.R")
source("R/helper_general.R")

#------------------------------------------------------------------------------------------------------------------------------------
# Simulation example described in Section 3 with independent features

# data generation
n = 500
set.seed(123)
create_xor = function(n, seed){
  x2 = runif(n, -1, 1)
  x3 = runif(n, -1, 1)
  x1 = runif(n, -1, 1)
  y = ifelse(x3>0, 3*x1, -3*x1) + x3 + rnorm(n, sd = 0.3)
  data.frame(x1,x2,x3, y)
}
data = create_xor(n, seed)
X = data[,setdiff(names(data),"y")]
features = colnames(X)



# Modelling
task = makeRegrTask(data = data, target = "y")

# tune Neural Network
set.seed(123)
ps = makeParamSet(
  makeDiscreteParam("decay", values = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5)),
  makeDiscreteParam("size", values = c(3, 5, 10, 20, 30))
)
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 5L)
res = tuneParams(makeLearner("regr.nnet",  maxit = 1000), task = task, resampling = rdesc,
                 par.set = ps, control = ctrl,  measures = list(mlr::mse, mlr::mae, mlr::rsq))

# fit Neural Network with best found HP on all training data
set.seed(123)
lrn = makeLearner("regr.nnet",  maxit = 1000,  size = res$x$size, decay = res$x$decay, trace = F)
#lrn = makeLearner("regr.nnet",  maxit = 1000,  size = 10, decay = 0.001, trace = F)
model = mlr::train(task = task, learner = lrn)

testset = create_xor(n = 10000, seed=234)
pred = predict(model, newdata = testset)$data
measureRSQ(pred$truth, pred$response)
predict.function = function(model, newdata) predict(model, newdata = newdata)$data$response

#------------------------------------------------------------------------------------------------------------------------------------
# Plots for uncorrelated case with ICE (Section 3 - Explaining REPID and Section 4.3 - GADGET-PD)


# Feature effects and Splitting
feature = "x1"
mod <- Predictor$new(model, data = data[which(names(data)!="y")], y = data$y)

# Interpretation with PDP
# ICE curves
eff = FeatureEffects$new(mod, grid.size = 20, feature = feature, method = "ice")

# get mean-centered ice curves
ice = compute_data_for_ice_splitting(eff, data, Z = setdiff(features, feature))
Y = ice$Y
X = ice$X
grid = ice$grid

# first split 
sp_L2 = split_parent_node(Y = Y, X = X, objective = SS_L2, n.quantiles = NULL, min.node.size = 1,
                          n.splits = 1, optimizer = find_best_binary_split, grid = grid)
SS_L2(Y,X) # before split
sp_L2 # after split

p_pdp = plot_pdp_split(sp_L2, eff, data, 1)


# Plot with all features for tree structure
effect = FeatureEffects$new(mod, grid.size = 20, method = "ice")
tree = compute_tree(effect, data, model = model, objective = "SS_L2_pd", Z = colnames(X), n.split = 1, n.quantiles = NULL, min.split = 1)

# plot root node
ice_region = regional_pd(effect, tree, 1)
plot_root = regional_pd_plot(ice_region, 1, "black", "lightgrey", -4, 4)
plot_root[[1]] + plot_root[[2]] + plot_root[[3]]

# save root node plot for all features
ggsave(plot_root[[1]] + plot_root[[2]] + plot_root[[3]], filename = "figures/full_tree_root.pdf", width = 8, height = 3.5)


# plot child nodes
ice_region_child = regional_pd(effect, tree, 2)

# create plots for left and right child nodes
plot_left = regional_pd_plot(ice_region_child, 1, brewer.pal(11, "RdBu")[8], brewer.pal(11, "RdBu")[9], -4, 4)
plot_right = regional_pd_plot(ice_region_child, 2, brewer.pal(11, "RdBu")[4], brewer.pal(11, "RdBu")[3], -4, 4)

# x1: (-0.998, 2.87), (0.0532, -0.168); x3: (-0.998, -0.5734), (-0.0562, 0.413)
p_left_1 = plot_left[[1]] + 
  geom_segment(aes(x = c(-0.998, 0.0532), y = c(2.87, 2.87), xend = c(0.0532, 0.0532), yend = c(2.87, -0.168)), lty = 2) +
  annotate(geom="text", x= c((-0.998 + 0.0532)/2, 0.5), y= c(3.3, 1.42), label=c("Delta~x==1.05", "Delta~hat(f)[j]==3.04"), parse = TRUE)
p_left_3 = plot_left[[3]] + 
  geom_segment(aes(x = c(-0.998, -0.0562), y = c(-0.5734, -0.5734), xend = c(-0.0562, -0.0562), yend = c(-0.5734, 0.413)), lty = 2) +
  annotate(geom="text", x= c((-0.998 -0.0562)/2, 0.4), y= c(-0.9, -0), label=c("Delta~x==0.94", "Delta~hat(f)[j]==0.99"), parse = TRUE)

ggsave(p_left_1 + plot_left[[2]] + p_left_3, filename = "figures/full_tree_left.pdf", width = 8, height = 3.5)

# x1: (-0.998, -2.93), (0.0532, 0.149); x3: (0.0484, -0.541), (0.885, 0.348)
p_right_1 = plot_right[[1]] + 
  geom_segment(aes(x = c(-0.998, 0.0532), y = c(-2.93, -2.93), xend = c(0.0532, 0.0532), yend = c(-2.93, 0.149)), lty = 2) +
  annotate(geom="text", x= c((-0.998 + 0.0532)/2, 0.5), y= c(-3.3, -1.42), label=c("Delta~x==1.05", "Delta~hat(f)[j]==3.08"), parse = TRUE)
p_right_3 = plot_right[[3]] + 
  geom_segment(aes(x = c(0.0484, 0.885), y = c(-0.541, 0.348), xend = c(0.0484, 0.0484), yend = c(0.348, 0.348)), lty = 2) +
  annotate(geom="text", x= c(0.45, -0.45), y= c(0.9, -0.1), label=c("Delta~x==0.83", "Delta~hat(f)[j]==0.89"), parse = TRUE)

ggsave(p_right_1 + plot_right[[2]] + p_right_3, filename = "figures/full_tree_right.pdf", width = 8, height = 3.5)



#------------------------------------------------------------------------------------------------------------------------------------
# Plots for uncorrelated case with ALE (Section 4.4 and Appendix C.1)


# ALE splitting
ale_preds = ale_deriv(model, feature, data = data, h = 10, target = "y", predict.fun = predict.function)
grid = lapply(ale_preds, function(feat){
  feat$feat.val
})

# first split 
sp_ale = split_parent_node(Y = ale_preds, X = X, objective = SS_ALE, n.quantiles = NULL, min.node.size = 1,
                           n.splits = 1, optimizer = find_best_binary_split, grid = grid)
SS_ALE(y= ale_preds,X) # before split
sp_ale # after split

ale_preds[[feature]]$node = as.factor(ifelse(data$x3<=sp_ale$split.points[[which(sp_ale$best.split)]], 1, 2))
ale_preds[[feature]]$dL[ale_preds[[feature]]$dL == 0] = NA


# Explanation figure (Section 4.4)
ale_preds = ale_preds[[feature]]
p_expl_ale = ggplot(data = ale_preds) + 
  geom_vline(data = data.frame(interval = c(min(ale_preds$x.left),unique(ale_preds$x.right))), aes(xintercept = interval), lty = 2) + 
  theme_classic() +
  geom_point(data = ale_preds, aes(x = feat.val, y = dL, col = node), alpha = 0.5) +
  geom_boxplot(aes(x = (x.left+x.right)/2-0.04, y= dL, group = interval.index), fill = "lightgrey") +
  geom_boxplot(aes(x = (x.left+x.right)/2+0.04, y= dL, group = interaction((x.left+x.right)/2+0.04, node), fill = node)) +
  xlab(expression(x[1])) + ylab(expression(paste(partialdiff,hat(f),"/", partialdiff,x[1]))) +
  scale_color_manual(name = "first split", values = c("1" = brewer.pal(11, "RdBu")[8], "2"=brewer.pal(11, "RdBu")[4]), 
                     labels = c(as.expression(bquote(paste(x[3]~"<="~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))),as.expression(bquote(paste(x[3]~">"~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))))) +
  scale_fill_manual(name = "first split", values = c("1" = brewer.pal(11, "RdBu")[8],"2"=brewer.pal(11, "RdBu")[4]), 
                    labels = c(as.expression(bquote(paste(x[3]~"<="~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))),as.expression(bquote(paste(x[3]~">"~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))))) +
  theme(legend.position="top")


p_ale = plot_ale_split(cbind(ale_preds, data)) +
  scale_color_manual(name = "first split", values = c("1" = brewer.pal(11, "RdBu")[8], "2"=brewer.pal(11, "RdBu")[4]), 
                     labels = c(as.expression(bquote(paste(x[3]~"<="~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))),as.expression(bquote(paste(x[3]~">"~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))))) +
  xlab(expression(x[1])) + 
  ylab(expression(paste(hat(f)[1])))  +
  theme(legend.position="top")



# Plots for Appendix C.1 with all features for tree structure
effect = ale_deriv(object = model, S =  c("x1","x2","x3"), data = data, h = 10, target = "y", predict.fun = predict.function)
tree = compute_tree(effect, data, objective = "SS_L2_ale", model = model, Z = c("x1","x2","x3"), n.split = 1, n.quantiles = NULL, min.split = 1, store.data = TRUE)

# root node plot
ale_root = ale_curve(effect)
plot_root = regional_ale_plot(ale_root, "grey", -4, 4, 0, 3)
p_root_ale = (((plot_root$x1[[1]]/plot_root$x1[[2]]) + plot_layout(heights = c(1,3))) | ((plot_root$x2[[1]]/plot_root$x2[[2]]) + plot_layout(heights = c(1,3))) | ((plot_root$x3[[1]]/plot_root$x3[[2]]) + plot_layout(heights = c(1,3))) )

ggsave(p_root_ale, filename = "figures/full_tree_root_ale.pdf", width = 8, height = 3.5)

# left node plot
ale_left = ale_curve(tree[[2]][[1]]$local)
plot_left = regional_ale_plot(ale_left, brewer.pal(11, "RdBu")[9], -4, 4, 0, 3, -1, 1)
p_left_1 = plot_left$x1[[2]] + 
  geom_segment(aes(x = c(-0.998, -0.005), y = c(2.84, 2.84), xend = c(-0.005, -0.005), yend = c(2.84, -0.009)), lty = 2) +
  annotate(geom="text", x= c((-0.998 -0.005)/2, 0.5), y= c(3.3, 1.42), label=c("Delta~x==0.99", "Delta~hat(f)[j]==2.85"), parse = TRUE)
p_left_3 = plot_left$x3[[2]]  + 
  geom_segment(aes(x = c(-0.998, 0.03), y = c(-0.607, -0.607), xend = c(0.03, 0.03), yend = c(-0.607, 0.7)), lty = 2) +
  annotate(geom="text", x= c((-0.998 + 0.03)/2, 0.5), y= c(-1.1, -0), label=c("Delta~x==1.03", "Delta~hat(f)[j]==1.3"), parse = TRUE)

p_left_ale = (((plot_left$x1[[1]]/p_left_1) + plot_layout(heights = c(1,3))) | ((plot_left$x2[[1]]/plot_left$x2[[2]]) + plot_layout(heights = c(1,3))) | ((plot_left$x3[[1]]/p_left_3) + plot_layout(heights = c(1,3))) )

ggsave(p_left_ale, filename = "figures/full_tree_left_ale.pdf", width = 8, height = 3.5)

# right node plot
ale_right = ale_curve(tree[[2]][[2]]$local)
plot_right = regional_ale_plot(ale_right, brewer.pal(11, "RdBu")[3], -4, 4, 0, 3, -1, 1)
p_right_1 = plot_right$x1[[2]]+ 
  geom_segment(aes(x = c(-0.998, -0.005), y = c(-2.88, -2.88), xend = c(-0.005, -0.005), yend = c(-2.88, 0.009)), lty = 2) +
  annotate(geom="text", x= c((-0.998 -0.005)/2, 0.5), y= c(-3.3, -1.42), label=c("Delta~x==0.99", "Delta~hat(f)[j]==2.89"), parse = TRUE)
p_right_3 = plot_right$x3[[2]]   + 
  geom_segment(aes(x = c(-0.18, 0.78), y = c(-0.56, 0.38), xend = c(-0.18, -0.18), yend = c(0.38, 0.38)), lty = 2) +
  annotate(geom="text", x= c(0.35, -0.6), y= c(0.9, -0.1), label=c("Delta~x==0.96", "Delta~hat(f)[j]==0.94"), parse = TRUE)

p_right_ale = (((plot_right$x1[[1]]/p_right_1) + plot_layout(heights = c(1,3))) | ((plot_right$x2[[1]]/plot_right$x2[[2]]) + plot_layout(heights = c(1,3))) | ((plot_right$x3[[1]]/p_right_3) + plot_layout(heights = c(1,3))) )

ggsave(p_right_ale, filename = "figures/full_tree_right_ale.pdf", width = 8, height = 3.5)




#------------------------------------------------------------------------------------------------------------------------------------
# Plots for uncorrelated case with Shapley (Section 4.5 and Appendix C.2)


# caluclate Shapley values
shap = shap_values(mod, features, data = data, target = "y")
#save(shap, file = "data/rdata_examples/shap_values_tree.RData")
#load("data/rdata_examples/shap_values_tree.RData")

# first split
sp_shap = split_parent_node(Y = shap, X = X, objective = SS_SHAP, grid = grid,n.quantiles = NULL, min.node.size = 2,
                             n.splits = 1, optimizer = find_best_binary_split)

# mean estimate with spline 
data_shap = cbind(shap[[feature]], data)
gam_mod = mgcv::gam(phi~s(feat.val, k = 3), data = data_shap)
gam_mod_l = mgcv::gam(phi~s(feat.val, k = 3), data = data_shap[data_shap[[sp_shap$feature[which(sp_shap$best.split)]]]<= sp_shap$split.points[[which(sp_shap$best.split)]],])
gam_mod_r = mgcv::gam(phi~s(feat.val, k = 3), data = data_shap[data_shap[[sp_shap$feature[which(sp_shap$best.split)]]]> sp_shap$split.points[[which(sp_shap$best.split)]],])
data_shap$node = ifelse(data_shap[[sp_shap$feature[which(sp_shap$best.split)]]]<= sp_shap$split.points[[which(sp_shap$best.split)]], 1, 2)


# Explanation Figure (Section 4.5)
p_shap = ggplot() + geom_point(data = data_shap, aes(x = feat.val, y = phi, color = factor(node)), alpha = 0.4) +
  geom_line(data = data_shap, aes(x = feat.val, y = gam_mod$fitted.values), col = "lightgrey", lwd = 2) +
  geom_line(data = data_shap[data_shap$node==1,], aes(x = feat.val, y = gam_mod_l$fitted.values), col = brewer.pal(11, "RdBu")[9], lwd = 2) +
  geom_line(data = data_shap[data_shap$node==2,], aes(x = feat.val, y = gam_mod_r$fitted.values), col = brewer.pal(11, "RdBu")[3], lwd = 2) +
  geom_segment(data =data.frame(feat.val = rep(data_shap$feat.val[195],2), phi = c(data_shap$phi[195]-0.02,data_shap$phi[195]+0.02), pred =  c(gam_mod$fitted.values[195],predict(gam_mod_l, newdata = data_shap[195,]))), aes(x = feat.val, y = phi, xend = feat.val, yend = pred), arrow = arrow(length = unit(0.1,"cm"))) +
  annotate(geom="text", x=rep(data_shap$feat.val[195],2)-0.05, y= c(data_shap$phi[195]-0.6, data_shap$phi[195]+0), label=c(expression(Delta[0]^2), expression(Delta[1]^2))) +
  scale_color_manual(name = "first split", values = c("1" = brewer.pal(11, "RdBu")[8], "2"=brewer.pal(11, "RdBu")[4]), labels = c(as.expression(bquote(paste(x[3]~"<="~ .(round(sp_shap$split.points[[which(sp_shap$best.split)]],2))))),as.expression(bquote(paste(x[3]~">"~ .(round(sp_shap$split.points[[which(sp_shap$best.split)]],2))))))) +
  theme_bw() + xlab(expression(x[1])) + ylab(expression(phi[1])) + theme(legend.position = "top") 
  

# Plots for Appendix C.1 - plots for tree structure
#load("data/rdata_examples/shap_values_tree.RData")
tree_shap = compute_tree(shap, data, model = model, predict.function = predict.function, objective = "SS_L2_shap", Z = c("x1","x2","x3"), n.split = 1, n.quantiles = NULL, min.split = 2, store.data = TRUE, shap.recalc = TRUE)

# root node plot
shap_root = shap
plot_root = regional_shap_plot(shap_root, "black", "lightgrey", -4, 4)
p_root_shap = plot_root$x1 + plot_root$x2 + plot_root$x3

ggsave(p_root_shap, filename = "figures/full_tree_root_shap.pdf", width = 8, height = 3.5)

# left node plot
shap_left = tree_shap[[2]][[1]]$local
plot_left = regional_shap_plot(shap_left, brewer.pal(11, "RdBu")[8], brewer.pal(11, "RdBu")[9], -4, 4, -1, 1)
p_left_1 = plot_left$x1 + geom_segment(aes(x = c(-0.995, 0), y = c(2.92, 2.92), xend = c(0, 0), yend = c(2.92, -0.1)), lty = 2) +
  annotate(geom="text", x= c(-0.5, 0.5), y= c(3.5, 1.5), label=c("Delta~x==1", "Delta~hat(f)[j]==3.02"), parse = TRUE)
p_left_3 = plot_left$x3  + geom_segment(aes(x = c(-1, 0), y = c(-0.59, 0.53), xend = c(-0, -0), yend = c(-0.59, -0.59)), lty = 2) +
  annotate(geom="text", x= c((-0.998 - 0.02)/2, 0.5), y= c(-1, 0), label=c("Delta~x==1", "Delta~hat(f)[j]==1.12"), parse = TRUE)

p_left_shap = p_left_1 + plot_left$x2 + p_left_3

ggsave(p_left_shap, filename = "figures/full_tree_left_shap.pdf", width = 8, height = 3.5)

# right node plot
shap_right = tree_shap[[2]][[2]]$local
plot_right = regional_shap_plot(shap_right, brewer.pal(11, "RdBu")[4], brewer.pal(11, "RdBu")[3], -4, 4, -1, 1)
p_right_1 = plot_right$x1+ geom_segment(aes(x = c(-0.99, -0), y = c(-2.9, -2.9), xend = c(-0, -0), yend = c(-2.9, 0.05)), lty = 2) +
  annotate(geom="text", x= c((-0.99 -0.005)/2, 0.5), y= c(-3.3, -1.42), label=c("Delta~x==0.99", "Delta~hat(f)[j]==2.95"), parse = TRUE)
p_right_3 = plot_right$x3   + geom_segment(aes(x = c(0.01, 0.99), y = c(-0.47, 0.48), xend = c(0.01, 0.01), yend = c(0.48, 0.48)), lty = 2) +
  annotate(geom="text", x= c(0.5, -0.5), y= c(0.9, -0), label=c("Delta~x==98", "Delta~hat(f)[j]==0.95"), parse = TRUE)

p_right_shap = p_right_1 + plot_right$x2 + p_right_3

ggsave(p_right_shap, filename = "figures/full_tree_right_shap.pdf", width = 8, height = 3.5)



#------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------
# Simulation example described in Section 3 with dependent features

# simulation example
create_xor_corr = function(n, seed){
  x3 = runif(n, -1, 1)
  x2 = runif(n, -1, 1)
  #x2 = -0.8*x3 + rnorm(n, sd = 0.1)
  x1 = x3 + rnorm(n, sd = 0.25)
  y = ifelse(x3>0, 3*x1, -3*x1)  + x3 + rnorm(n, sd = 0.3)
  data.frame(x1,x2,x3, y)
}

# generate data
set.seed(123)
data = create_xor_corr(n, seed)
X = data[,setdiff(names(data),"y")]
features = names(X)


# Modelling
task = makeRegrTask(data = data, target = "y")

set.seed(123)
lrn = makeLearner("regr.nnet",  maxit = 1000,  size = res$x$size, decay = res$x$decay, trace = F)
model = mlr::train(task = task, learner = lrn)

testset = create_xor_corr(n = 10000, seed=234)
pred = predict(model, newdata = testset)$data
measureRSQ(pred$truth, pred$response)


#------------------------------------------------------------------------------------------------------------------------------------
# Plots for correlated case with ICE (Section 3 - Limitations of REPID)

# Interpretation
mod <- Predictor$new(model, data = data[which(names(data)!="y")], y = data$y)
eff = FeatureEffects$new(mod, grid.size = 20, feature = feature, method = "ice")

# get mean-centered ice curves
ice = compute_data_for_ice_splitting(eff, data, Z = setdiff(features, feature))
Y = ice$Y
X = ice$X
grid = ice$grid

# first split 
sp_L2_corr = split_parent_node(Y = Y, X = X, objective = SS_L2, n.quantiles = NULL, min.node.size = 1,
                          n.splits = 1, optimizer = find_best_binary_split, grid = grid)

SS_L2(Y,X) # before split
sp_L2_corr # after split

p_pdp_corr = plot_pdp_split(sp_L2_corr, eff, data, 1)

ggsave("figures/example_repid_extrapol.pdf", p_pdp + p_pdp_corr, width = 9, height = 4)



#------------------------------------------------------------------------------------------------------------------------------------
# Plots for correlated case with ALE (Section 4.4)

# ALE splitting
set.seed(123)
h_quant = 10
ale_preds = ale_deriv(model, feature, data = data, h = 10, target = "y", predict.fun = predict.function)
grid = lapply(ale_preds, function(feat){
  feat$feat.val
})

# first split 
sp_ale = split_parent_node(Y = ale_preds, X = X, objective = SS_ALE, n.quantiles = NULL, min.node.size = 1,
                           n.splits = 1, optimizer = find_best_binary_split, grid = grid)
SS_ALE(y= ale_preds,X) # before split
sp_ale # after split


ale_preds[[feature]]$node = as.factor(ifelse(data$x3<=sp_ale$split.points[[which(sp_ale$best.split)]], 1, 2))
ale_preds[[feature]]$dL[ale_preds[[feature]]$dL == 0] = NA


# Explanation figure
ale_preds = ale_preds[[feature]]
p_expl_ale_corr = ggplot(data = ale_preds) + 
  geom_vline(data = data.frame(interval = c(min(ale_preds$x.left),unique(ale_preds$x.right))), aes(xintercept = interval), lty = 2) + 
  theme_classic() +
  geom_point(data = ale_preds, aes(x = feat.val, y = dL, col = node), alpha = 0.5) +
  geom_boxplot(aes(x = (x.left+x.right)/2-0.04, y= dL, group = interval.index), fill = "lightgrey") +
  geom_boxplot(aes(x = (x.left+x.right)/2+0.04, y= dL, group = interaction((x.left+x.right)/2+0.04, node), fill = node)) +
  xlab(expression(x[1])) + ylab(expression(paste(partialdiff,hat(f),"/", partialdiff,x[1]))) +
  scale_color_manual(name = "first split", values = c("1" = brewer.pal(11, "RdBu")[8], "2"=brewer.pal(11, "RdBu")[4]), labels = c(as.expression(bquote(paste(x[3]~"<="~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))),as.expression(bquote(paste(x[3]~">"~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))))) +
  scale_fill_manual(name = "first split", values = c("1" = brewer.pal(11, "RdBu")[8],"2"=brewer.pal(11, "RdBu")[4]), labels = c(as.expression(bquote(paste(x[3]~"<="~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))),as.expression(bquote(paste(x[3]~">"~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))))) +
  theme(legend.position="top")


p_ale_corr = plot_ale_split(cbind(ale_preds, data)) +
  scale_color_manual(name = "first split", values = c("1" = brewer.pal(11, "RdBu")[8], "2"=brewer.pal(11, "RdBu")[4]), 
                     labels = c(as.expression(bquote(paste(x[3]~"<="~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))),as.expression(bquote(paste(x[3]~">"~ .(round(sp_ale$split.points[[which(sp_ale$best.split)]],2))))))) +
  xlab(expression(x[1])) + ylab(expression(paste(hat(f)[1])))  +
  theme(legend.position="top")


ggsave("figures/example_ale_xor.pdf", (p_expl_ale + p_expl_ale_corr)/(p_ale + p_ale_corr), width = 9, height = 5)


#------------------------------------------------------------------------------------------------------------------------------------
# Plots for uncorrelated case with Shapley (Section 4.5)


# caluclate Shapley values
shap_cor = shap_values(mod, features, data = data, target = "y")
#save(shap_cor, file = "data/rdata_examples/data_shap_dep.RData")
#load("data/rdata_examples/data_shap_dep.RData")

# first split
sp_shap_cor = split_parent_node(Y = shap_cor, X = X, objective = SS_SHAP, grid = grid,n.quantiles = NULL, min.node.size = 2,
                            n.splits = 1, optimizer = find_best_binary_split)

# mean estimate with spline (degree 3)
data_shap_cor = cbind(shap_cor[[feature]], data)
gam_mod_cor = mgcv::gam(phi~s(feat.val, k = 3), data = data_shap_cor)
gam_mod_l_cor = mgcv::gam(phi~s(feat.val, k = 3), data = data_shap_cor[data_shap_cor[[sp_shap_cor$feature[which(sp_shap_cor$best.split)]]]<= sp_shap_cor$split.points[[which(sp_shap_cor$best.split)]],])
gam_mod_r_cor = mgcv::gam(phi~s(feat.val, k = 3), data = data_shap_cor[data_shap_cor[[sp_shap_cor$feature[which(sp_shap_cor$best.split)]]]> sp_shap_cor$split.points[[which(sp_shap_cor$best.split)]],])
data_shap_cor$node = ifelse(data_shap_cor[[sp_shap_cor$feature[which(sp_shap_cor$best.split)]]]<= sp_shap_cor$split.points[[which(sp_shap_cor$best.split)]], 1, 2)


# create explanation graphic
obs = 233
p_shap_cor = ggplot() + geom_point(data = data_shap_cor, aes(x = feat.val, y = phi, color = factor(node)), alpha = 0.4) +
  geom_line(data = data_shap_cor, aes(x = feat.val, y = gam_mod_cor$fitted.values), col = "lightgrey", lwd = 2) +
  geom_line(data = data_shap_cor[data_shap_cor$node==1,], aes(x = feat.val, y = gam_mod_l_cor$fitted.values), col = brewer.pal(11, "RdBu")[9], lwd = 2) +
  geom_line(data = data_shap_cor[data_shap_cor$node==2,], aes(x = feat.val, y = gam_mod_r_cor$fitted.values), col = brewer.pal(11, "RdBu")[3], lwd = 2) +
  geom_segment(data =data.frame(feat.val = rep(data_shap_cor$feat.val[obs],2), phi = c(data_shap_cor$phi[obs]+0.02,data_shap_cor$phi[obs]-0.02), pred =  c(gam_mod_cor$fitted.values[obs],predict(gam_mod_r_cor, newdata = data_shap_cor[obs,]))), aes(x = feat.val, y = phi, xend = feat.val, yend = pred), arrow = arrow(length = unit(0.1,"cm"))) +
  annotate(geom="text", x=rep(data_shap_cor$feat.val[obs],2)-0.09, y= c(data_shap_cor$phi[obs]+0.5, data_shap_cor$phi[obs]-0.05), label=c(expression(Delta[0]^2), expression(Delta[1]^2))) +
  scale_color_manual(name = "first split", values = c("1" = brewer.pal(11, "RdBu")[8], "2"=brewer.pal(11, "RdBu")[4]), labels = c(as.expression(bquote(paste(x[3]~"<="~ .(round(sp_shap_cor$split.points[[which(sp_shap_cor$best.split)]],2))))),as.expression(bquote(paste(x[3]~">"~ .(round(sp_shap_cor$split.points[[which(sp_shap_cor$best.split)]],2))))))) +
  theme_bw() + 
  xlab(expression(x[1])) + 
  ylab(expression(phi[1])) + 
  theme(legend.position = "top") 


ggsave("figures/example_shap_xor.pdf", p_shap + p_shap_cor, width = 10.5, height = 4)



