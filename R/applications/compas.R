####################################################################################################
# ANALYSIS OF COMPAS DATA
####################################################################################################

# source required functions 
source("R/load_packages.R")
source("R/tree_splitting.R")
source("R/helper_effects.R")
source("R/applications/helper.R")
source("R/helper_tree_splitting.R")
source("R/helper_interaction_test.R")
source("R/pint.R")

#---------------------------------------------------------------------------------------------------
# 1. load dataset (from ProPublica repo) and process data (as done by ProPublica)

# Process data
raw_data <- read.csv("data/application/compas-scores-two-years-violent.csv")
nrow(raw_data)


df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')
dim(df)


df$length_of_stay <- as.numeric(as.Date(df$c_jail_out) - as.Date(df$c_jail_in))
cor(df$length_of_stay, df$decile_score)

# select only race %in% c("African-American", "Caucasian")
sparse_category_flag <- which((df$race %in% c("African-American", "Caucasian")))
df = df[sparse_category_flag,]

# factorize categorical features and target (we use here a binary target vor High / Low risk)
# the binary target is created in the same way as in some analysis for ProPublica
df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race)) %>%
  mutate(two_year_recid_factor = factor(two_year_recid)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))


# define feature and target space
features = c("age", "crime_factor", "race_factor", "gender_factor",  "priors_count")
target = "score_factor"
X = df[,features]
y = df[,target]
data = cbind(X, y)


#---------------------------------------------------------------------------------------------------
# 2. Model selection via (nested) cross-validation
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3tuning)
task = TaskClassif$new("compas", data, target = "y")

# setting search space for svm
learner = lrn("classif.ksvm",
              C  = to_tune(p_dbl(-12, 12, trafo = function(x) 2^x)),
              sigma = to_tune(p_dbl(-12, 12, trafo = function(x) 2^x)),
              kernel = "rbfdot",
              type = "C-svc",
              predict_type = "prob"
)
learner$encapsulate = c(train = "callr", predict = "callr") 
learner$param_set$values = list(type = "C-svc",kernel = "rbfdot", C = 1, sigma = as.numeric(sigest(y~.,data = data)[2])) 

# 3 inner folds and random search
at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.mcc"),
  term_evals = 30,
)

# evaluation on 5 outer folds
outer_resampling = rsmp("cv", folds = 5)


# benchmark algorithms
set.seed(123)
lrns = c(at, lrn("classif.featureless", predict_type = "prob"), lrn("classif.ranger", predict_type = "prob"),
         lrn("classif.log_reg", predict_type = "prob"), lrn("classif.ksvm", predict_type = "prob"))
d = benchmark_grid(task = task, learner = lrns, resampling = outer_resampling)
bmr = benchmark(design = d)

bmr$aggregate(list(msr("classif.mcc"), msr("classif.fbeta"),msr("classif.bbrier")))


# 
# nr task_id          learner_id resampling_id iters classif.mcc classif.fbeta classif.bbrier
# 1:  1  compas  classif.ksvm.tuned            cv     5   0.4752263     0.8125753      0.1699458
# 2:  2  compas classif.featureless            cv     5   0.0000000     0.7573156      0.3904601
# 3:  3  compas      classif.ranger            cv     5   0.4710182     0.8125245      0.1679199
# 4:  4  compas     classif.log_reg            cv     5   0.4675127     0.8056969      0.1679249


# best model is tuned svm --> use it for analysis
set.seed(123)
at$train(task)


#---------------------------------------------------------------------------------------------------
# 3. Train random forest on entire dataset and create predictor for analysis of effects
# Modelling 

set.seed(123)
task = mlr::makeClassifTask(id = "compas", data = cbind(X, "y"=y), target = "y")
lrn = mlr::makeLearner("classif.ksvm", sigma = at$tuning_result$learner_param_vals[[1]]$sigma, C = at$tuning_result$learner_param_vals[[1]]$C, predict.type = "prob")
model = mlr::train(lrn, task)
predictor = Predictor$new(model, data = X, y = task$env$data$y, class = "HighScore")

# Model wrapper for PINT
model_wrapper_svm = function(data, target){
  task = mlr::makeClassifTask(data = data, target = target)
  learner = mlr::makeLearner("classif.ksvm", sigma = at$tuning_result$learner_param_vals[[1]]$sigma, C = at$tuning_result$learner_param_vals[[1]]$C, predict.type = "prob")
  model = mlr::train(learner, task)
  predict.function = function(model, newdata) predict(model, newdata = newdata)$data$prob.HighScore
  return(list("model_fit" = model, "predict.function" = predict.function))
}


predict.function = function(model, newdata) predict(model, newdata = newdata)$data$prob.HighScore
data = task$env$data

#---------------------------------------------------------------------------------------------------
# 4.) apply PINT to find interacting features -- here with PDP
set.seed(123)
null_importance = null_int_perm(X = X, y = data$y, model = model_wrapper_svm, S = colnames(X), nperm = 200, effect_method = "pd", class = "HighScore")
int_imp = interaction_importance(X = X, y = data$y, model_wrapper_svm, "pd", colnames(X), calc_hstat = FALSE, class = "HighScore")

null_dist = null_importance[["pd"]]
int_val = int_imp$int_imp[["pd"]]
pint = PIMP(int_val, null_dist, method = NA)
# all features are significant w.r.t. alpha = 0.05 

#---------------------------------------------------------------------------------------------------
# 5.) calculate GADGET for S based on PDP (SD for comparison)
effect_pd = FeatureEffects$new(predictor, grid.size = 10, method = "ice")
effect_shap = fast_shap_values(model, colnames(X), data, "y", nsim = 500, predict.function)


tree_pd = compute_tree(effect_pd, data, model = model,predict.function = predict.function, objective = "SS_L2_pd",
                       Z = colnames(X), n.split = 2, impr.par = 0.15, n.quantiles = 100, min.split = 50, shap.recalc = TRUE, store.data = TRUE)
tree_shap = compute_tree(effect_shap, data, model = model,predict.function = predict.function, objective = "SS_L2_shap", 
                         Z = colnames(X), n.split = 2, impr.par = 0.1, n.quantiles = 10, min.split = 50, shap.recalc = TRUE, store.data = TRUE)
#save(effect_shap, tree, file = "data/application/compas_shap.RData")




# Plot for PD
eff =regional_plot(tree_pd, effect_pd, "pd")
region_labels = region_labeler(tree_pd, eff$age)
# plot: manually adjust color and label names
plot = plot_regional(eff, region_labels, "pd", brewer.pal(11, "RdYlBu")[c(1,3,10,8)], 0, 1.1)


# create plots for paper
library(gridExtra)
library(lemon)
p1 = plot[[1]] 
p2 = plot[[2]] + theme( legend.position = "none" ) +xlab("crime")
p3 = plot[[3]] + theme( legend.position = "none" ) +xlab("ethnicity")
p4 = plot[[4]] + theme( legend.position = "none" ) +xlab("gender")
p5 = plot[[5]] + theme( legend.position = "none" ) 
legend <- g_legend(p1 + theme(legend.position='bottom', legend.title = element_blank()))
plot_region_all = grid.arrange(p1+theme(legend.position='hidden'), p2+theme(legend.position='hidden'),
             p3+theme(legend.position='hidden'),p4+theme(legend.position='hidden'), p5+theme(legend.position='hidden'), ncol = 5,
             bottom=legend$grobs[[1]])
ggsave(plot_region_all, filename = "figures/compas_pdp_region.pdf", width = 12, height = 3.7)


# root node plot
names(effect_pd$results) = c("age", "crime", "ethnicity","gender", "priors_count" )
p_root = lapply(names(effect_pd$results), function(feature_name){
  feat = effect_pd$results[[feature_name]]
  pdp = cbind(aggregate(x = .value~.borders, data = feat, FUN = mean)) 
  
  if(is.numeric(pdp$.borders)){
    ggplot() + geom_line(data = feat, mapping = aes(x = .borders, y = .value, group = .id), alpha = 0.4, col = "black" ) + theme_bw() +
      ylab(expression(hat(f)[j])) + xlab(feature_name) +  
      geom_line(data = pdp, mapping = aes(x = .borders, y = .value), lwd = 1.5, col = "lightgrey") +ylim(0, 1.1) 
  }
  else{
    ggplot(feat, aes(x = .borders, y = .value)) + geom_boxplot(aes(middle = mean(.value))) + theme_bw() +
      ylab(expression(hat(f)[j]))+ xlab(feature_name) + ylim(0, 1.1) 
  }
})
plot_root_all = grid.arrange(p_root[[1]], p_root[[2]], p_root[[3]],p_root[[4]],p_root[[5]], ncol = 5)
ggsave(plot_root_all, filename = "figures/compas_pdp_root.pdf", width = 12, height = 3.5)



# Plot for SD
eff =regional_plot(tree, effect_shap, "shap")
region_labels = region_labeler(tree, eff$age)
# plot: manually adjust color and label names
plot = plot_regional(eff, region_labels, "shap", brewer.pal(11, "RdYlBu")[c(1,3,10,8)], -0.7, 0.9)


# create plots for paper
p1 = plot[[1]] 
p2 = plot[[2]] + theme( legend.position = "none" ) +xlab("crime")
p3 = plot[[3]] + theme( legend.position = "none" ) +xlab("ethnicity")
p4 = plot[[4]] + theme( legend.position = "none" ) +xlab("gender")
p5 = plot[[5]] + theme( legend.position = "none" ) 
legend <- g_legend(p1 + theme(legend.position='bottom', legend.title = element_blank()))
plot_region_all = grid.arrange(p1+theme(legend.position='hidden'), p2+theme(legend.position='hidden'),
                               p3+theme(legend.position='hidden'),p4+theme(legend.position='hidden'), 
                               p5+theme(legend.position='hidden'), ncol = 5,
                               bottom=legend$grobs[[1]])
ggsave(plot_region_all, filename = "figures/compas_shap_region.pdf", width = 12.7, height = 4)


# root node plot
names(effect_shap) = c("age", "crime", "ethnicity","gender", "priors_count" )
p_root = lapply(names(effect_shap), function(feature_name){
  feat = effect_shap[[feature_name]]
  if(is.numeric(feat$feat.val)){
    gam_mod = mgcv::gam(phi~s(feat.val), data = feat)
    pred = predict(gam_mod, feat, se.fit = TRUE)
    feat$phi_pred = pred$fit
  }
  
  if(is.numeric(feat$feat.val)){
    ggplot() +  geom_point(data = feat, mapping = aes(x = feat.val, y = phi), size = 0.8, col = "black", alpha = 0.25) + 
      geom_line(data = feat, mapping = aes(x = feat.val, y = phi_pred), lwd = 1.2, col = "lightgrey" ) + theme_bw() +
      ylab(expression(hat(f)[j])) + xlab(feature_name) +ylim(-0.7, 0.9) 
  }
  else{
    ggplot(feat, aes(x = feat.val, y = phi)) + geom_boxplot(aes(middle = mean(phi))) + theme_bw() +
      ylab(expression(hat(f)[j]))+ xlab(feature_name) + ylim(-0.7, 0.9) 
  }
})
plot_root_all = grid.arrange(p_root[[1]], p_root[[2]], p_root[[3]],p_root[[4]],p_root[[5]], ncol = 5)
ggsave(plot_root_all, filename = "figures/compas_shap_root.pdf", width = 12, height = 3.5)




