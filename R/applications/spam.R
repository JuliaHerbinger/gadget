####################################################################################################
# ANALYSIS OF SPAM DATA
####################################################################################################

library(mlr3verse)
library(mlr3oml)
library(tidyselect)
library(purrr)

# source required functions 
source("R/load_packages.R")
source("R/tree_splitting.R")
source("R/helper_effects.R")
source("R/applications/helper.R")
source("R/helper_tree_splitting.R")
source("R/helper_interaction_test.R")
source("R/pint.R")
source("R/helper_general.R")

#---------------------------------------------------------------------------------------------------
# define task
tsk_spam = tsk("spam")

# evaluate performance of random forest
set.seed(123)
rf = lrn("classif.ranger", predict_type = "prob")
cv = rsmp("cv", folds = 5)
rr = mlr3::resample(tsk_spam, rf, cv)

# performance
rr$score(msrs(c("classif.ce", "classif.bacc", "classif.acc", "classif.bbrier")))
rr$aggregate(msrs(c("classif.ce", "classif.bacc", "classif.acc")))

# train a random forest on all the data for analysis
X = tsk_spam$data(,cols = tsk_spam$feature_names)
y = tsk_spam$data(, cols = tsk_spam$target_names)


# Define data and model
data = as.data.frame(cbind(X, "y" = y$type))
task = mlr::makeClassifTask(id = "spam", data = data, target = "y")
lrn = mlr::makeLearner("classif.ranger", predict.type = "prob", importance = "permutation")
set.seed(123)
model = mlr::train(lrn, task)
predictor = Predictor$new(model, data = X, y = task$env$data$y, class = "spam")


# correlation of features
corrplot::corrplot(cor(X)) # just a few columns show higher correlation, others almost uncorrelated

#---------------------------------------------------------------------------------------------------
# calculate GADGET based on PDP with quantile based grid

features = tsk_spam$feature_names
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
                                       
# compute GADGET
predict.function = function(model, newdata) predict(model, newdata = newdata)$data$prob.spam
data = task$env$data
tree_pd = compute_tree(effect_pd, data, model = model,predict.function = predict.function, objective = "SS_L2_pd",
                       Z = colnames(X), n.split = 4, impr.par = 0.25, n.quantiles = 100, min.split = 100, shap.recalc = TRUE, store.data = TRUE)
summary_split = extract_split_criteria(tree_pd)

#---------------------------------------------------------------------------------------------------
# first filtering step

obj_root = sum(unlist(summary_split[1,] %>% dplyr::select(tidyselect::starts_with("objective.value.parent."))))
df <- dapply(summary_split, as.numeric)
risk_leaf = colSums(df[df$node.final==TRUE,3:59] )

df_overview = data.frame(features = substring(colnames(summary_split[1,] %>% dplyr::select(tidyselect::starts_with("objective.value.parent."))),first = 24), 
                         risk.root = unlist(summary_split[1,] %>% dplyr::select(starts_with("objective.value.parent."))), 
                         risk.root.total = obj_root, 
                         risk.leaf = risk_leaf)
df_overview$risk.red = df_overview$risk.root-df_overview$risk.leaf
df_overview$risk.red.rel = df_overview$risk.red/df_overview$risk.root
df_overview$risk.red.rel.tot = df_overview$risk.red/df_overview$risk.root.total

df_highest_red = head(df_overview[order(df_overview$risk.red.rel.tot, decreasing = TRUE),], 10)


df_sorted = df_overview[order(df_overview$risk.root, decreasing = TRUE),]

# plot mean centered ICE curves
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

effect_round_center = data_center_plot(effect_pd, "font")
p_round = ggplot(effect_round_center, aes(x = .grid, y = .value, group = .id)) + 
  geom_line(alpha = 0.4) + ylim(-0.5, 0.4) + theme_bw() + xlab("font") + ylab(expression(hat(f)[j]))

effect_labs_center = data_center_plot(effect_pd, "technology")
p_labs = ggplot(effect_labs_center, aes(x = .grid, y = .value, group = .id)) + 
  geom_line(alpha = 0.4) + ylim(-0.5, 0.4) + theme_bw() + xlab("technology") + ylab(expression(hat(f)[j]))

effect_tab_center = data_center_plot(effect_pd, "table")
p_tab = ggplot(effect_tab_center, aes(x = .grid, y = .value, group = .id)) + 
  geom_line(alpha = 0.4) + ylim(-0.5, 0.4) + theme_bw() + xlab("table") + ylab(expression(hat(f)[j]))

ggsave(filename = "figures/spam_heterogeneity.pdf", p_rem + p_round + p_labs + p_tab + plot_layout(nrow=1), width = 10, height = 2)

# cutoff point at labs (risk value > 5) --> keep 30 features for PINT
S = df_sorted$features[which(df_sorted$risk.root > 5)]

# Elbow graph of interaction-related risk
p_risk = ggplot(df_sorted, aes(x = factor(features, levels = features), y = risk.root)) + geom_point() +
  geom_path(group=1) + theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5)) +
  xlab("features") + ylab("heterogeneity-related risk") +
  geom_vline(xintercept = 30.5, lty = 2, col = "red")
  #geom_segment(aes(x = 30.5, xend = 30.5, y = 0, yend = 6), col = "red", lwd = 1)
ggsave(filename = "figures/spam_elbow.pdf", p_risk, width = 7, height = 4)


#---------------------------------------------------------------------------------------------------
# use PINT for pre-filtered features
# Model wrapper for PINT
model_wrapper_rf = function(data, target){
  task = mlr::makeClassifTask(data = data, target = target)
  learner = mlr::makeLearner("classif.ranger", predict.type = "prob")
  model = mlr::train(learner, task)
  predict.function = function(model, newdata) predict(model, newdata = newdata)$data$prob.spam
  return(list("model_fit" = model, "predict.function" = predict.function))
}


print(Sys.time())
set.seed(123)
null_importance = null_int_perm_par(X = X, y = data$y, model = model_wrapper_rf, S = S, nperm = 200, effect_method = "pd", class = "spam")
int_imp = interaction_importance(X = X, y = data$y, model_wrapper_rf, "pd", S, calc_hstat = FALSE, class = "spam")
save(null_importance, int_imp, file = "pint_spam_200.RData")
print(Sys.time())
null_dist = null_importance
int_val = int_imp$int_imp[["pd"]]
pint = PIMP(int_val, null_dist, method = NA)


# 12 of the 30 features are significant w.r.t. alpha = 0.05
pint[pint<0.05]
S = names(pint[pint<0.05])
#> S
#[1] "remove"          "charExclamation" "hp"              "capitalLong"     "charDollar"      "george"          "free"            "edu"            
#[9] "money"           "credit"          "num000"          "receive"  

# use GADGET on final feature subset S chosen by PINT
effect_pd_red = list(effect_pd$results[which(names(effect_pd$results) %in% S)])
tree_pd_red = compute_tree(effect_pd_red, data, model = model,predict.function = predict.function, objective = "SS_L2_pd",
                       Z = S, n.split = 4, impr.par = 0.25, n.quantiles = 100, min.split = 100, shap.recalc = TRUE, store.data = TRUE)
summary_split = extract_split_criteria(tree_pd_red)

# Plot for PD

names(effect_pd_red) = "results"
eff_red =regional_plot(tree_pd_red, effect_pd_red, "pd")
region_labels = region_labeler(tree_pd_red, eff_red$remove)
region_labels = unique(region_labels)
region_labels = list(region_labels[[5]], region_labels[[1]], region_labels[[4]], region_labels[[2]], region_labels[[3]])
# plot: manually adjust color and label names
plot = plot_regional(eff_red, region_labels, "pd", brewer.pal(11, "Paired")[c(11,3,5,7:8)], -.12, 1)


# create plots for paper
library(gridExtra)
library(lemon)

p1 = plot[[4]] + theme( legend.position = "none" ) 
p2 = plot[[5]] + theme( legend.position = "none" ) 
p3 = plot[[12]] + theme( legend.position = "none" ) 
legend <- g_legend(p1 + theme(legend.position='bottom', legend.title = element_blank()) + guides(colour = guide_legend(nrow = 5)))
plot_region_all = grid.arrange(p3+theme(legend.position='hidden'), p1+theme(legend.position='hidden'),
                               p2+theme(legend.position='hidden'), ncol = 3,
                               bottom=legend$grobs[[1]])
ggsave(filename = "figures/spam_filtered.pdf", plot_region_all, width = 6.5, height = 5)
                   