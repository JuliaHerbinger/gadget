library(xtable)
source("R/helper_general.R")
source("R/load_packages.R")
source("R/simulations/helper_simulation.R")

#---------------------------------------------------------------------------------------------------------------

# SIMULATION STUDY SECTION 6.1 (EXTRAPOLATION) 

#---------------------------------------------------------------------------------------------------------------

# Before this file can be run, the data needs to be generated (R/simulations/batchtools/generate_data_cor.R)
# and results need to be reduced (R/simulations/batchtools/reduce_experiments_cor.R)

#---------------------------------------------------------------------------------------------------------------

# ANALYSIS OF XOR SETTING


# path to folder, with folders for dataset results
path = "data/batchtools/2_result_tables/"
datasets = list.files(path)
type = c( "xor")

# create list with one list containing one dataframe per dataset
data_all = lapply(type, function(typ){
  
  df.final = NULL
  methods = c("tree", "perf")
  
  lapply(methods, function(method){
    res.tree = readRDS(file.path(path, paste0("eval_", typ, "_", method, ".rds")))
    for(i in 1:nrow(res.tree)){
      df_sub = as.data.frame(res.tree[i,])
      
      if(method == "perf") df_sub_res = data.frame(perf = df_sub$result[[1]])
      else df_sub_res = df_sub$result[[1]]
      if((is.null(df_sub_res)) ) next
      else if( (nrow(df_sub_res) == 0)) next
      df_sub = cbind(df_sub[,which(colnames(df_sub) != "result")], df_sub_res)
      
      if(i == 1) df = df_sub
      else df = rbind(df, df_sub)
    }
    df
  })
})
names(data_all) = type


# Data
data_to_plot = data_all$xor[[1]]
impr.par = 0.2

# choose one impr. parameter and depth = 6, models: gam, neural net
data_to_plot = data_to_plot[which(data_to_plot$impr.par==impr.par),]
data_to_plot = data_to_plot[which(data_to_plot$depth!=6),]
data_to_plot = data_to_plot[which(data_to_plot$learner %in% c("gam", "regr.nnet")),]

# include only recalculated SHAP values
data_to_plot = data_to_plot[which(!(data_to_plot$objective.function == "SS_L2_shap" & data_to_plot$recalc == FALSE)),]
data_to_plot$objective.function = substr(data_to_plot$objective.function, 7, nchar(data_to_plot$objective.function))

# calculate improvements and create plot for paper
data_to_plot$intImp = as.numeric(data_to_plot$intImp)
data_to_plot2 = data_to_plot[which(data_to_plot$split.feature != "final"),] %>% 
  dplyr::group_by(job.id, learner, split.feature, objective.function, cor) %>% 
  dplyr::summarise(mean_intImp = sum(intImp))
data_to_plot2$split.feature = as.character(data_to_plot2$split.feature)

learner_names = c(`gam` = "GAM", `regr.nnet` = "Neural net")
cor_names = c(`0` = "0", `0.3` = "0.4", `0.5` = "0.7", `0.7` = "0.9")
p_box_xor = ggplot(data_to_plot2) + geom_boxplot(aes(x = objective.function, y = mean_intImp, color = split.feature)) + 
  facet_grid(learner~cor, labeller = labeller(learner = as_labeller(learner_names), cor = as_labeller(cor_names)))
p_box_xor = p_box_xor + theme_bw() + xlab("Method") + ylab(expression(I[z])) + guides(color=guide_legend(title="Split feature"))
p_box_xor = p_box_xor + scale_color_brewer(palette = "Accent")

ggsave("figures/boxplot_xor.pdf", p_box_xor, width = 7, height = 3)

# how often was x3 chosen to be first split feature? (create table)
tab = data_to_plot[which(data_to_plot$depth==1),]  %>% 
  dplyr::group_by(learner, objective.function, cor, split.feature) %>% 
  dplyr::summarise(count = n(), min = min(unlist(split.value)), max = max(unlist(split.value)), range = max-min)
tab = tab[tab$split.feature=="x3",c("learner","objective.function", "cor","count", "range")]
tab$count = tab$count/30

tab = tab %>%
  tidyr::pivot_wider(
    names_from  = c(objective.function), # Can accommodate more variables, if needed.
    values_from = c(count, range)
  )
library(xtable)
print(xtable(tab), include.rownames=FALSE)

# how often do we split in second stage?
tab = data_to_plot[which(data_to_plot$depth==2 & data_to_plot$split.feature != "final"),]  %>% 
  dplyr::group_by(learner, objective.function, cor, split.feature) %>% 
  dplyr::summarise(count = n(), min = min(unlist(split.value)), max = max(unlist(split.value)), range = max-min)

# total number of nodes
tab = data_to_plot[which(data_to_plot$split.feature == "final"),]  %>% 
  dplyr::group_by(job.id, learner, objective.function, cor, split.feature) %>% 
  dplyr::summarise(count = n())
tab = tab %>% 
  dplyr::group_by(learner, objective.function, cor, split.feature) %>% 
  dplyr::summarise(min = min(count), max = max(count))

# calculate Rsquared
tab_rsq_all = data_to_plot[which(data_to_plot$split.feature != "final"),]  %>% 
  dplyr::group_by(job.id, learner, objective.function, cor) %>% 
  dplyr::summarise(Rsq = sum(intImp))
tab_rsq = tab_rsq_all  %>% dplyr::group_by(learner, objective.function, cor) %>% dplyr::summarise(rsq = mean(Rsq), rsq.sd = sd(Rsq))

# calculate Rsquared per feature in S
data_rsq_j = data_to_plot 

data_rsq_j = data_rsq_j[!is.na(data_rsq_j$split.feature.parent),]

tab_rsq_j = data_rsq_j %>% dplyr::group_by(job.id, learner, objective.function, cor, objective.value.parent.x1) %>%
  dplyr::summarise(obj.x1 = sum(unlist(objective.value.x1)), obj.x2 = sum(unlist(objective.value.x2)), 
                   obj.x3 = sum(unlist(objective.value.x3)), obj.parent.x1 = mean(unlist(objective.value.parent.x1)), 
                   obj.parent.x2 = mean(unlist(objective.value.parent.x2)), obj.parent.x3 = mean(unlist(objective.value.parent.x3)))
tab_rsq_j$loss_red.x1 = tab_rsq_j$obj.parent.x1-tab_rsq_j$obj.x1
tab_rsq_j$loss_red.x2 = tab_rsq_j$obj.parent.x2-tab_rsq_j$obj.x2
tab_rsq_j$loss_red.x3 = tab_rsq_j$obj.parent.x3-tab_rsq_j$obj.x3

tab_rsq_j_all = tab_rsq_j  %>% dplyr::group_by(job.id, learner, objective.function, cor) %>% 
  dplyr::summarise(loss_red.x1 = sum(unlist(loss_red.x1)), loss_red.x2 = sum(unlist(loss_red.x2)), 
                   loss_red.x3 = sum(unlist(loss_red.x3)), obj.root.x1 = max(unlist(objective.value.parent.x1)), 
                   obj.root.x2 = max(unlist(obj.parent.x2)), obj.root.x3 = max(unlist(obj.parent.x3)), 
                   rsq.x1 = loss_red.x1/obj.root.x1, rsq.x2 = loss_red.x2/obj.root.x2, rsq.x3 = loss_red.x3/obj.root.x3)
tab_rsq_j_aggr = tab_rsq_j_all  %>% dplyr::group_by(learner, objective.function, cor) %>% 
  dplyr::summarise(rsq.mean.x1 = mean(rsq.x1), rsq.sd.x1 = sd(rsq.x1),rsq.mean.x2 = mean(rsq.x2), 
                   rsq.sd.x2 = sd(rsq.x2),rsq.mean.x3 = mean(rsq.x3), rsq.sd.x3 = sd(rsq.x3) )


# performance comparison
data_perf = perf_xor %>% dplyr::group_by(job.id, learner, cor) %>% dplyr::summarise(mean = mean(perf))
data_perf %>% group_by(learner, cor) %>% dplyr::summarise(mu = mean(mean),sd= sd(mean) )




