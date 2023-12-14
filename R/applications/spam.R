library(mlr3verse)
library(mlr3oml)



# # list OpenML datasets
# odatasets = list_oml_data(
#   number_features = c(30, 100),
#   number_instances = c(1000, 20000),
#   number_classes = 2
# )
# 
# odatasets[NumberOfFeatures > 30,
#           c("data_id", "name", "NumberOfFeatures", "NumberOfInstances")]
# 
# 
# # choose Spam dataset
# odata = odt(id = 44)
# odata
# 
# # convert to mlr3 backend and task
# backend = as_data_backend(odata)
# tsk_spam = as_task_classif(backend, target = "class", positive = "1")
tsk_spam = tsk("spam")


# evaluate performance of random forest
set.seed(123)
rf = lrn("classif.ranger", predict_type = "prob")
cv = rsmp("cv", folds = 5)
rr = resample(tsk_spam, rf, cv)

# performance
rr$score(msrs(c("classif.ce", "classif.bacc", "classif.acc", "classif.bbrier")))
rr$aggregate(msrs(c("classif.ce", "classif.bacc", "classif.acc")))

# train a random forest on all the data for analysis
# set.seed(123)
# rf$train(tsk_spam)
X = tsk_spam$data(,cols = tsk_spam$feature_names)
y = tsk_spam$data(, cols = tsk_spam$target_names)


# source required functions 
source("R/load_packages.R")
source("R/tree_splitting.R")
source("R/helper_effects.R")
source("R/applications/helper.R")
source("R/helper_tree_splitting.R")
source("R/helper_interaction_test.R")
source("R/pint.R")
source("R/helper_general.R")



data = as.data.frame(cbind(X, "y" = y$type))
task = mlr::makeClassifTask(id = "spam", data = data, target = "y")
lrn = mlr::makeLearner("classif.ranger", predict.type = "prob", importance = "permutation")
set.seed(123)
model = mlr::train(lrn, task)
predictor = Predictor$new(model, data = X, y = task$env$data$y, class = "spam")

# Feature importance
imp = sort(model$learner.model$variable.importance)
df_imp = data.frame(feature = names(imp), pfi = imp)
df_imp$feature = factor(df_imp$feature, levels = names(imp))
p_imp = ggplot(df_imp, aes(x = pfi, y = feature)) + geom_bar(stat = "identity") + 
  theme_bw()
ggsave(filename = "figures/spam_pfi.pdf", p_imp, width = 4, height = 6.5)


# Apply GADGET without prefiltering
corrplot::corrplot(cor(X)) # just a few columns show higher correlation, others almost uncorrelated


# calculate GADGET based on PDP 
#effect_pd = FeatureEffects$new(predictor, grid.size = 10, method = "ice", features = c(features[1:2]))
features = tsk_spam$feature_names
#features = S
effects <- future.apply::future_lapply(features,
                                       function(x) {
                                         feature_effect <- function(x, predictor) {
                                           grid = quantile(data[,x], probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
                                           FeatureEffect$new(
                                             feature = x, predictor = predictor, method = "ice",
                                             grid.points = grid
                                           )
                                         }
                                         feature_effect(x,
                                                        predictor = predictor
                                         )
                                       },
                                       future.seed = TRUE,
                                       future.packages = loadedNamespaces()
)
names(effects) <- features
results <- future.apply::future_lapply(effects, function(x) {
  res <- x$results
  fname.index <- which(colnames(res) %in% names(effects))
  res$.feature <- colnames(res)[fname.index]
  colnames(res)[fname.index] <- ".borders"
  res
},
future.seed = TRUE
)
effect_pd = list("results" = results)
#effect_pd_red = list("results" = results)
                                       

predict.function = function(model, newdata) predict(model, newdata = newdata)$data$prob.spam
data = task$env$data
tree_pd = compute_tree(effect_pd, data, model = model,predict.function = predict.function, objective = "SS_L2_pd",
                       Z = colnames(X), n.split = 4, impr.par = 0.25, n.quantiles = 100, min.split = 100, shap.recalc = TRUE, store.data = TRUE)
summary_split = extract_split_criteria(tree_pd)

save(effect_pd, tree_pd, file = "data/application/spam_results.RData")

# calculate contribution for each feature (in percentage of own heterogeneity and of total heterogeneity) --> features contributing to overall highest recution are visualized regionally
library(tidyselect)
obj_root = sum(unlist(summary_split[1,] %>% select(starts_with("objective.value.parent."))))


library(purrr)
df <- dapply(summary_split, as.numeric)
risk_leaf = colSums(df[df$node.final==TRUE,3:59] )

df_overview = data.frame(features = substring(colnames(summary_split[1,] %>% select(starts_with("objective.value.parent."))),first = 24), risk.root = unlist(summary_split[1,] %>% select(starts_with("objective.value.parent."))), risk.root.total = obj_root, risk.leaf = risk_leaf)
df_overview$risk.red = df_overview$risk.root-df_overview$risk.leaf
df_overview$risk.red.rel = df_overview$risk.red/df_overview$risk.root
df_overview$risk.red.rel.tot = df_overview$risk.red/df_overview$risk.root.total

df_highest_red = head(df_overview[order(df_overview$risk.red.rel.tot, decreasing = TRUE),], 10)

# Plot for PD
eff =regional_plot(tree_pd, effect_pd, "pd")
region_labels = region_labeler(tree_pd, eff$remove)
region_labels = unique(region_labels)
region_labels = list(region_labels[[1]], region_labels[[4]], region_labels[[7]], region_labels[[2]], region_labels[[3]], region_labels[[5]], region_labels[[6]])
# plot: manually adjust color and label names
plot = plot_regional(eff, region_labels, "pd", brewer.pal(11, "Paired")[c(11,1:6)], -.08, 1.01)


# create plots for paper
library(gridExtra)
library(lemon)
#p1 = plot[[1]] 
#p2 = plot[[9]] + theme( legend.position = "none" ) 
#p3 = plot[[15]] + theme( legend.position = "none" ) 
p1 = plot[[19]] + theme( legend.position = "none" ) 
p2 = plot[[32]] + theme( legend.position = "none" ) 
#p3 = plot[[49]] + theme( legend.position = "none" ) 
p4 = plot[[50]] + theme( legend.position = "none" ) 
#p5 = plot[[55]] + theme( legend.position = "none" ) 
legend <- g_legend(p1 + theme(legend.position='bottom', legend.title = element_blank()) + guides(colour = guide_legend(nrow = 7)))
plot_region_all = grid.arrange(p1+theme(legend.position='hidden'), p2+theme(legend.position='hidden'),
                               p4+theme(legend.position='hidden'), ncol = 3,
                               bottom=legend$grobs[[1]])
ggsave(filename = "figures/spam_unfiltered.pdf", plot_region_all, width = 6.5, height = 5)

# stepwise: pre-filter features with almost no heterogeneity--> small risk value --> visualize for appendix mean-centered ICE curves
df_sorted = df_overview[order(df_overview$risk.root, decreasing = TRUE),]

# plot with highest heterogeneity
data_center_plot = function(effect, feature){
  effect_remove = effect$results[[feature]]
  effect_rem_center = tidyr::spread(effect_remove, .borders, .value)
  effect_rem_center = effect_rem_center[, setdiff(colnames(effect_rem_center), c(".type",".feature"))]
  effect_rem_center[,-which(colnames(effect_rem_center)==".id")] = effect_rem_center[,-which(colnames(effect_rem_center)==".id")] - rowMeans(effect_rem_center[,-which(colnames(effect_rem_center)==".id")])
  effect_rem_center = tidyr::gather(effect_rem_center, key = ".grid", value = .value, -.id)
  effect_rem_center$.grid = as.numeric(effect_rem_center$.grid)
  return(effect_rem_center)
}

effect_rem_center = data_center_plot(effect_pd, "remove")
p_rem = ggplot(effect_rem_center, aes(x = .grid, y = .value, group = .id)) + 
  geom_line(alpha = 0.4) + ylim(-0.5, 0.4) + theme_bw() + xlab("remove") +ylab(expression(hat(f)[j]))

effect_round_center = data_center_plot(effect_pd, "charRoundbracket")
p_round = ggplot(effect_round_center, aes(x = .grid, y = .value, group = .id)) + 
  geom_line(alpha = 0.4) + ylim(-0.5, 0.4) + theme_bw() + xlab("charRoundbracket") + ylab(expression(hat(f)[j]))

effect_labs_center = data_center_plot(effect_pd, "labs")
p_labs = ggplot(effect_labs_center, aes(x = .grid, y = .value, group = .id)) + 
  geom_line(alpha = 0.4) + ylim(-0.5, 0.4) + theme_bw() + xlab("labs") + ylab(expression(hat(f)[j]))

effect_tab_center = data_center_plot(effect_pd, "table")
p_tab = ggplot(effect_tab_center, aes(x = .grid, y = .value, group = .id)) + 
  geom_line(alpha = 0.4) + ylim(-0.5, 0.4) + theme_bw() + xlab("table") + ylab(expression(hat(f)[j]))

ggsave(filename = "figures/spam_heterogeneity.pdf", p_rem + p_round + p_labs + p_tab + plot_layout(nrow=1), width = 10, height = 2)

# cutoff point at labs (risk value > 5) --> keep 30 features for PINT
S = df_sorted$features[which(df_sorted$risk.root > 5)]

p_risk = ggplot(df_sorted, aes(x = factor(features, levels = features), y = risk.root)) + geom_point() +
  geom_path(group=1) + theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5)) +
  xlab("features") + ylab("heterogeneity-related risk") +
  geom_vline(xintercept = 30.5, lty = 2, col = "red")
  #geom_segment(aes(x = 30.5, xend = 30.5, y = 0, yend = 6), col = "red", lwd = 1)
ggsave(filename = "figures/spam_elbow.pdf", p_risk, width = 7, height = 4)


# use PINT for pre-filtered features
# Model wrapper for PINT
model_wrapper_rf = function(data, target){
  task = mlr::makeClassifTask(data = data, target = target)
  learner = mlr::makeLearner("classif.ranger", predict.type = "prob")
  model = mlr::train(learner, task)
  predict.function = function(model, newdata) predict(model, newdata = newdata)$data$prob.spam
  return(list("model_fit" = model, "predict.function" = predict.function))
}


#predict.function = function(model, newdata) predict(model, newdata = newdata)$data$prob.HighScore
#data = task$env$data

#---------------------------------------------------------------------------------------------------
# 4.) apply PINT to find interacting features -- here with PDP
print(Sys.time())
set.seed(123)
null_importance = null_int_perm_par(X = X, y = data$y, model = model_wrapper_rf, S = S, nperm = 200, effect_method = "pd", class = "spam")
int_imp = interaction_importance(X = X, y = data$y, model_wrapper_rf, "pd", S, calc_hstat = FALSE, class = "spam")
save(null_importance, int_imp, file = "pint_spam_200.RData")
print(Sys.time())
null_dist = null_importance
int_val = int_imp$int_imp[["pd"]]
pint = PIMP(int_val, null_dist, method = NA)


# 10 of the 30 features are significant w.r.t. alpha = 0.05
pint[pint<0.05]
S = names(pint[pint<0.05])
#> S
#[1] "remove"          "hp"              "charExclamation" "capitalLong"     "charDollar"      "free"            "george"         
#[8] "money"           "credit"          "receive"  

# use GADGET on final feature subset S chosen by PINT
tree_pd_red = compute_tree(effect_pd_red, data, model = model,predict.function = predict.function, objective = "SS_L2_pd",
                       Z = S, n.split = 4, impr.par = 0.25, n.quantiles = 100, min.split = 100, shap.recalc = TRUE, store.data = TRUE)
summary_split = extract_split_criteria(tree_pd_red)

# Plot for PD
eff_red =regional_plot(tree_pd_red, effect_pd_red, "pd")
region_labels = region_labeler(tree_pd_red, eff_red$remove)
region_labels = unique(region_labels)
region_labels = list(region_labels[[1]], region_labels[[5]], region_labels[[4]], region_labels[[2]], region_labels[[3]])
# plot: manually adjust color and label names
plot = plot_regional(eff_red, region_labels, "pd", brewer.pal(11, "Paired")[c(11,3,5,7:8)], -.08, 1.01)


# create plots for paper
library(gridExtra)
library(lemon)
#p1 = plot[[1]] 
#p2 = plot[[9]] + theme( legend.position = "none" ) 
#p3 = plot[[15]] + theme( legend.position = "none" ) 
p1 = plot[[3]] + theme( legend.position = "none" ) 
p2 = plot[[8]] + theme( legend.position = "none" ) 
#p3 = plot[[49]] + theme( legend.position = "none" ) 
p4 = plot[[1]] + theme( legend.position = "none" ) 
#p5 = plot[[55]] + theme( legend.position = "none" ) 
legend <- g_legend(p1 + theme(legend.position='bottom', legend.title = element_blank()) + guides(colour = guide_legend(nrow = 7)))
plot_region_all = grid.arrange(p1+theme(legend.position='hidden'), p2+theme(legend.position='hidden'),
                               p4+theme(legend.position='hidden'), ncol = 3,
                               bottom=legend$grobs[[1]])
ggsave(filename = "figures/spam_filtered.pdf", plot_region_all, width = 6.5, height = 5)
                   