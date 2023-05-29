library(xtable)
source("R/helper_general.R")
source("R/load_packages.R")
source("R/simulations/helper_simulation.R")

#---------------------------------------------------------------------------------------------------------------

# SIMULATION STUDY SECTION 6.2 (HIGHER ORDER) 

#---------------------------------------------------------------------------------------------------------------

# Before this file can be run, the data needs to be generated (R/simulations/batchtools/generate_data_higher_order.R)
# and results need to be reduced (R/simulations/batchtools/reduce_experiments_higher_order.R)

#---------------------------------------------------------------------------------------------------------------


# path to folder, with folders for dataset results
path = "data/batchtools/1_result_tables/"
datasets = list.files(path)
type = c( "categorical_linear1", "categorical_linear2")

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


# data
data_cat2 = data_all$categorical_linear2
impr.par = 0.1
data_to_plot = data_cat2[[1]]
data_to_plot = data_to_plot[which(data_to_plot$impr.par==impr.par),]
data_to_plot = data_to_plot
data_to_plot = data_to_plot[which(data_to_plot$learner %in% c("ranger_exact", "regr.ranger")),]

# create plot for paper
data_to_plot$objective.function[which(data_to_plot$objective.function == "SS_L2_shap" & data_to_plot$recalc == TRUE)] = "SS_L2_shap_rc"
data_to_plot$objective.function[which(data_to_plot$objective.function == "SS_L2_shap" & data_to_plot$recalc == FALSE)] = "SS_L2_shap_not_rc"
data_to_plot$objective.function = substr(data_to_plot$objective.function, 7, nchar(data_to_plot$objective.function))
data_to_plot$intImp = as.numeric(data_to_plot$intImp)
data_to_plot2 = data_to_plot %>%
  dplyr::group_by(job.id, learner, split.feature, objective.function) %>% 
  dplyr::summarise(mean_intImp = sum(intImp))
data_to_plot2$split.feature = as.character(data_to_plot2$split.feature)


# create figure
learner_names = c(`ranger_exact` = "XGB", `regr.ranger` = "RF")
p_box_cat = ggplot(data_to_plot2) + geom_boxplot(aes(x = objective.function, y = mean_intImp, color = split.feature)) + 
  facet_grid(.~learner, labeller = labeller(learner = as_labeller(learner_names)))
p_box_cat = p_box_cat + theme_bw() + xlab("Method") + ylab(expression(I[z])) + guides(color=guide_legend(title="Split feature"))
p_box_cat = p_box_cat + scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])
ggsave("figures/sim_higher_order.pdf", p_box_cat, width = 7, height = 3.2)


# how often do we split in second stage?
tab_depth_2 = data_to_plot[which(data_to_plot$depth==2 & data_to_plot$split.feature != "final"),]  %>% 
  dplyr::group_by(learner, objective.function, id, split.feature) %>% dplyr::summarise(count = n()/30)
tab_depth_2 = tab_depth_2 %>%
  tidyr::pivot_wider(
    names_from  = c(id), # Can accommodate more variables, if needed.
    values_from = c(split.feature, count)
  )

tab_depth_2 = tab_depth_2[,c(1,2,3,5,4,6)]

print(xtable(tab_depth_2), include.rownames=FALSE)

# how often do we split in third stage?
tab_depth_3 = data_to_plot[which(data_to_plot$depth==3 & data_to_plot$split.feature.parent == "x5" & data_to_plot$id ==1 & data_to_plot$split.feature != "final"),]  %>% 
  dplyr::group_by(learner, objective.function, split.feature) %>% dplyr::summarise(count = n()/30)
print(xtable(tab_depth_3), include.rownames=FALSE)

# total number of nodes
tab = data_to_plot[which(data_to_plot$split.feature == "final"),]  %>% 
  dplyr::group_by(job.id, learner, objective.function) %>% 
  dplyr::summarise(count = n())
tab = tab %>% dplyr::group_by(learner, objective.function) %>% 
  dplyr::summarise(min = min(count), max = max(count), med = median(count))
print(xtable(tab), include.rownames=FALSE)

# calculate Rsquared
tab_rsq_all = data_to_plot[which(data_to_plot$split.feature != "final"),]  %>% 
  dplyr::group_by(job.id, learner, objective.function) %>% dplyr::summarise(Rsq = sum(intImp))
tab_rsq = tab_rsq_all  %>% dplyr::group_by(learner, objective.function) %>% 
  dplyr::summarise(rsq = mean(Rsq), rsq.sd = sd(Rsq))

# average performance
data_perf = data_cat2[[2]] %>% dplyr::group_by(job.id, learner) %>% dplyr::summarise(mean = mean(perf))
data_perf %>% group_by(learner) %>% dplyr::summarise(mu = mean(mean),sd= sd(mean) )



