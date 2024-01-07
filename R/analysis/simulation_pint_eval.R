library(xtable)
source("R/helper_general.R")
source("R/load_packages.R")
source("R/simulations/helper_pint.R")

#---------------------------------------------------------------------------------------------------------------

# SIMULATION PINT EVALUATION (SECTION 5 AND APPENDIX F)

#---------------------------------------------------------------------------------------------------------------

# Before this file can be run, the data needs to be generated (R/simulations/batchtools/generate_data_pint_eval.R)
# and results need to be reduced (R/simulations/batchtools/reduce_experiments_pint_eval.R)

#---------------------------------------------------------------------------------------------------------------

# path to folder, with folders for dataset results
path = "data/batchtools/result_tables_pint"
datasets = list.files(path)


# Analysis for PINT 
data_pint = function(type, path, cor){
    data = lapply(type, function(typ){
   
    method = "pint"
    
    res.tree = readRDS(file.path(path, paste0("eval_", typ, "_", method, ".rds")))
    res.tree = res.tree[res.tree$dep == cor,]
  
    for(i in 1:nrow(res.tree)){
      df_sub = as.data.frame(res.tree[i,])
      df_sub_res = data.frame(rbind(rbind(df_sub$result[[1]]$pd$pint,df_sub$result[[1]]$ale$pint),
                                    df_sub$result[[1]]$shap$pint ))
      colnames(df_sub_res) = paste0("x",1:ncol(df_sub_res))
      df_sub_res = gather(df_sub_res, feature, pint, colnames(df_sub_res), factor_key=TRUE)
      df_sub_res$method = rep(c("pd","ale","sd"), nlevels(df_sub_res$feature))
      #df_sub_res$method = rep(c("pd"), nlevels(df_sub_res$feature))
      
      df_sub = cbind(df_sub[,which(colnames(df_sub) != "result")], df_sub_res)
      
      
      if(i == 1) df = df_sub
      else df = rbind(df, df_sub)
    }
    df
    
  })
  
  data
}

#----------------------------------------------------------------------------------------------------
# CREATE FIGURES FOR SECTION 5

type = "spur_int"

# define data sets
data_pint_spur = data_pint(type = type, path = path, cor = "no")
data_pint_spur = data_pint_spur[[1]]

# define rejection rate an summary over repetitions
data_pint_spur$rejection = ifelse(data_pint_spur$pint <= 0.05, 1, 0)
lin_overview = data_pint_spur %>% dplyr::group_by(dep, beta, noise, feature, method) %>% dplyr::summarise(rejection_proportion = mean(rejection), pvalue = round(mean(pint),3))
lin_overview$beta = as.numeric(lin_overview$beta)

# create plots
p_pint_lin_no = ggplot(lin_overview[lin_overview$dep=="no",]) + 
  geom_point(aes(x = beta, y = rejection_proportion, color = feature, shape = feature)) + 
  facet_grid(.~method ) + 
  geom_line(aes(x = beta, y = rejection_proportion, color = feature, group = feature)) + 
  geom_hline(yintercept = 0.05, col = "red", lty = 2) + 
  labs(x = "Interaction effect size", y = "Rejection rate") + 
  theme_bw() + theme(legend.title=element_blank()) + 
  scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])

ggsave("figures/pint_eval_error.pdf", p_pint_lin_no, width = 8, height = 3)



p_pint_pval = ggplot(data_pint_spur_lin[data_pint_spur_lin$dep=="no" & data_pint_spur_lin$feature %in% c("x1", "x4"),]) + 
  geom_boxplot(aes(x = as.numeric(beta), y = pint, group = beta), fill = "lightgrey") + 
  facet_grid(.~feature + method) +
  geom_hline(yintercept = 0.05, col = "red", lty = 2) + 
  labs(x = "Interaction effect size", y = "P-value") + 
  theme_bw() + theme(legend.title=element_blank()) + 
  scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])

ggsave("figures/pint_eval_pval.pdf", p_pint_pval, width = 10, height = 3.5)



# CREATE FIGURES FOR SECTION 5 COUNTEREXAMPLE

type = "spur_int_non"

# define data sets
data_pint_spur = data_pint(type = type, path = path, cor = "high")
data_pint_spur_high = data_pint_spur[[1]]

# define rejection rate an summary over repetitions
data_pint_spur_high$rejection = ifelse(data_pint_spur_high$pint <= 0.05, 1, 0)
lin_overview = data_pint_spur_high %>% dplyr::group_by(dep, beta, noise, feature, method) %>% dplyr::summarise(rejection_proportion = mean(rejection), pvalue = round(mean(pint),3))
lin_overview$beta = as.numeric(lin_overview$beta)

# create plot
p_pint_lin = ggplot(lin_overview) + geom_point(aes(x = beta, y = rejection_proportion, color = feature)) + 
  facet_grid(.~method ) + geom_line(aes(x = beta, y = rejection_proportion, color = feature, group = feature)) + 
  geom_hline(yintercept = 0.05, col = "red", lty = 2) + 
  labs(x = "Interaction effect size", y = "Rejection rate") + 
  theme_bw() + theme(legend.title=element_blank()) + 
  scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])

ggsave("figures/pint_eval_counterexample.pdf", p_pint_lin, width = 8, height = 3)




#----------------------------------------------------------------------------------------------------
# CREATE FIGURES FOR APPENDIX F

type = c("spur_lin", "spur_nonlin")
#type = "spur_int"
# type = "spur_int_non"

# define data sets depending on correlation
data_pint_spur_no = data_pint(type = type, path = path, cor = "no")
data_pint_spur_med = data_pint(type = type, path = path, cor = "medium")
data_pint_spur_high = data_pint(type = type, path = path, cor = "high")

# define data sets based on functional relationship
data_pint_spur_lin = rbind(data_pint_spur_no[[1]], data_pint_spur_med[[1]], data_pint_spur_high[[1]])
data_pint_spur_nonlin = rbind(data_pint_spur_no[[2]], data_pint_spur_med[[2]], data_pint_spur_high[[2]])

# define rejection rates
data_pint_spur_lin$rejection = ifelse(data_pint_spur_lin$pint <= 0.05, 1, 0)
data_pint_spur_nonlin$rejection = ifelse(data_pint_spur_nonlin$pint <= 0.05, 1, 0)

# summarize rejection rates over repetitions
lin_overview = data_pint_spur_lin %>% dplyr::group_by(dep, beta, noise, feature, method) %>% dplyr::summarise(rejection_proportion = mean(rejection), pvalue = round(mean(pint),3))
nonlin_overview = data_pint_spur_nonlin %>% dplyr::group_by(dep, beta, noise, feature, method) %>% dplyr::summarise(rejection_proportion = mean(rejection), pvalue = round(mean(pint),3))

# convert effect size beta 
lin_overview$beta = as.numeric(lin_overview$beta)
nonlin_overview$beta = as.numeric(nonlin_overview$beta)


# create plots 
p_pint_lin = ggplot(lin_overview) + geom_point(aes(x = beta, y = rejection_proportion, color = feature),  position=position_dodge(width = 0.12)) + 
  facet_grid(method~dep  + noise, labeller = labeller(dep = as_labeller(c(`high` = "high correlation", `medium` = "medium correlation", `no` = "no correlation")),
                                                      noise = as_labeller(c(`no` = "no noise", `yes` = "with noise")))) + 
  geom_line(aes(x = beta, y = rejection_proportion, color = feature, group = feature),  position=position_dodge(width = 0.12)) +
  geom_hline(yintercept = 0.05, col = "red", lty = 2) + 
  labs(x = "Interaction effect size", y = "Rejection rate") + 
  theme_bw() + theme(legend.title=element_blank()) + 
  scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])

p_pint_nonlin = ggplot(nonlin_overview) + geom_point(aes(x = beta, y = rejection_proportion, color = feature),  position=position_dodge(width = 0.12)) + 
  facet_grid(method~dep  + noise, labeller = labeller(dep = as_labeller(c(`high` = "high correlation", `medium` = "medium correlation", `no` = "no correlation")),
                                                      noise = as_labeller(c(`no` = "no noise", `yes` = "with noise")))) +
  geom_line(aes(x = beta, y = rejection_proportion, color = feature, group = feature),  position=position_dodge(width = 0.12)) +
  geom_hline(yintercept = 0.05, col = "red", lty = 2) + 
  labs(x = "Interaction effect size", y = "Rejection rate") + 
  theme_bw() + theme(legend.title=element_blank()) + 
  scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])

ggsave("figures/sim_lin_rej.pdf", p_pint_lin, height = 6, width = 10)
ggsave("figures/sim_nonlin_rej.pdf", p_pint_nonlin, height = 6, width = 10)





