library(xtable)
source("R/helper_general.R")
source("R/load_packages.R")
source("R/simulations/helper_simulation.R")

#---------------------------------------------------------------------------------------------------------------

# SIMULATION STUDY SECTION 6.3 (SPURIOUS INTERACTIONS AND PINT) 

#---------------------------------------------------------------------------------------------------------------

# Before this file can be run, the data needs to be generated (R/simulations/batchtools/generate_data_pint.R)
# and results need to be reduced (R/simulations/batchtools/reduce_experiments_pint.R)

#---------------------------------------------------------------------------------------------------------------

# path to folder, with folders for dataset results
path = "data/batchtools/5_result_tables/"
datasets = list.files(path)
type = c("spur_pint")


# Analysis for PINT compared to HStatistics
data_pint = lapply(type, function(typ){
  
  method = "pint"
  
  res.tree = readRDS(file.path(path, paste0("eval_", typ, "_", method, ".rds")))
  
  for(i in 1:nrow(res.tree)){
    df_sub = as.data.frame(res.tree[i,])
    df_sub_res = data.frame(rbind(rbind(df_sub$result[[1]]$pd$pint,df_sub$result[[1]]$ale$pint),df_sub$result[[1]]$shap$pint ))
    colnames(df_sub_res) = paste0("x",1:ncol(df_sub_res))
    df_sub_res = gather(df_sub_res, feature, pint, colnames(df_sub_res), factor_key=TRUE)
    hstatistics = rbind(rbind(df_sub$result[[1]]$pd$hstat$results,df_sub$result[[1]]$ale$hstat$results),df_sub$result[[1]]$shap$hstat$results )
    df_sub_res$hstatistics = hstatistics[order(.feature),]$.interaction
    df_sub_res$method = rep(c("pd","ale","sd"), ncol(df_sub_res)/3)
    
    df_sub = cbind(df_sub[,which(colnames(df_sub) != "result")], df_sub_res)
    
    
    if(i == 1) df = df_sub
    else df = rbind(df, df_sub)
  }
  df
  
})

data_pint_spur = data_pint[[1]]


# Plot for PINT and HSTAT
label_names = c(`300` = "n = 300", `500` = "n = 500")

p_pint = ggplot(data_pint_spur) + geom_boxplot(aes(x = method, y = pint, color = feature)) + 
  facet_grid(.~n, labeller = labeller(n = as_labeller(label_names))) +
  geom_hline(yintercept = 0.05, col = "red", lty = 2) + 
  labs(x = "Method", y = "PINT p-value") + 
  theme_bw() + theme(legend.title=element_blank()) + 
  scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])

p_hstat = ggplot(data_pint_spur) + geom_boxplot(aes(x = feature, y = hstatistics, color = feature)) + 
  facet_grid(.~n, labeller = labeller(n = as_labeller(label_names))) +
  labs(x = "Features", y = "H-Statistics") + 
  theme_bw() + theme(legend.title=element_blank()) + 
  scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])


#library(patchwork)
p_pint_hstat = p_pint + p_hstat + plot_layout(guides = 'collect', widths = c(2:1))
ggsave("figures/sim_pint_hstat.pdf", p_pint_hstat, width = 10, height = 3.9)




# Analysis: splitting with and without pint

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
      else {
        if(ncol(df_sub) > ncol(df)) df_sub = df_sub[,colnames(df)]
        df = rbind(df, df_sub)
      }
    }
    df
  })
})
names(data_all) = type


# Create plot for paper
data_spur = data_all$spur_pint
data_to_plot = data_spur[[1]]
data_to_plot = data_to_plot
data_to_plot$objective.function = substr(data_to_plot$objective.function, 7, nchar(data_to_plot$objective.function))
data_to_plot$objective.function[data_to_plot$objective.function=="shap"] = "sd"
data_to_plot$intImp = as.numeric(data_to_plot$intImp)
data_to_plot2 = data_to_plot %>% 
  dplyr::group_by(job.id, n, split.feature, objective.function, pint) %>% 
  dplyr::summarise(mean_intImp = sum(intImp))
data_to_plot2$split.feature = as.character(data_to_plot2$split.feature)

label_names_n = c(`300` = "n = 300", `500` = "n = 500")
label_names_pint = c(`not_pint` = "without PINT", `pint` = "with PINT")

p_box_pint = ggplot(data_to_plot2) + geom_boxplot(aes(x = objective.function, y = mean_intImp, color = split.feature)) + 
  facet_grid(n~pint, labeller = labeller(n = as_labeller(label_names_n), pint = as_labeller(label_names_pint)))
p_box_pint = p_box_pint + theme_bw() + xlab("Method") + ylab(expression(I[z])) + guides(color=guide_legend(title="Split feature"))
p_box_pint = p_box_pint + scale_color_manual(values = brewer.pal(5,"Accent")[c(1,2,3,5)])
ggsave("figures/sim_pint.pdf", p_box_pint, width = 7, height = 4.2)


# total number of nodes
tab = data_to_plot[which(data_to_plot$split.feature == "final"),]  %>% 
  dplyr::group_by(job.id, n, objective.function, pint) %>% 
  dplyr::summarise(count = n())
tab = tab %>% dplyr::group_by(n, objective.function, pint) %>% 
  dplyr::summarise(min = min(count), max = max(count), med = median(count))
tab$pint = factor(tab$pint, levels = c("pint", "not_pint"), labels = c("with PINT", "without PINT"))
tab$n = as.integer(tab$n)
print(xtable(tab), include.rownames=FALSE)

# calculate Rsquared
tab_rsq_all = data_to_plot[which(data_to_plot$split.feature != "final"),]  %>% 
  dplyr::group_by(job.id, n, pint, objective.function) %>% 
  dplyr::summarise(Rsq = sum(intImp))
tab_rsq = tab_rsq_all  %>% dplyr::group_by(n, pint, objective.function) %>% dplyr::summarise(rsq = mean(Rsq))

# calculate Rsquared per feature in S
data_rsq_j = data_to_plot 
data_rsq_j = data_rsq_j[!(is.na(data_rsq_j$split.feature.parent)),]

tab_rsq_j = data_rsq_j %>% dplyr::group_by(job.id, n, objective.function, pint, objective.value.parent.x1) %>% 
  dplyr::summarise(obj.x1 = sum(unlist(objective.value.x1)), obj.x2 = sum(unlist(objective.value.x2)), 
                   obj.parent.x1 = mean(unlist(objective.value.parent.x1)), obj.parent.x2 = mean(unlist(objective.value.parent.x2)))
tab_rsq_j = tab_rsq_j[-which(is.na(tab_rsq_j$obj.parent.x1)),]
tab_rsq_j$loss_red.x1 = tab_rsq_j$obj.parent.x1-tab_rsq_j$obj.x1
tab_rsq_j$loss_red.x2 = tab_rsq_j$obj.parent.x2-tab_rsq_j$obj.x2

tab_rsq_j_all = tab_rsq_j  %>% dplyr::group_by(job.id, n, objective.function, pint) %>% 
  dplyr::summarise(loss_red.x1 = sum(unlist(loss_red.x1), na.rm = TRUE), 
                   loss_red.x2 = sum(unlist(loss_red.x2), na.rm = TRUE), 
                   obj.root.x1 = max(unlist(objective.value.parent.x1), na.rm = TRUE), 
                   obj.root.x2 = max(unlist(obj.parent.x2), na.rm = TRUE),  
                   rsq.x1 = loss_red.x1/obj.root.x1, rsq.x2 = loss_red.x2/obj.root.x2)

tab_rsq_j_aggr = tab_rsq_j_all  %>% dplyr::group_by(n, objective.function, pint) %>% 
  dplyr::summarise(rsq.mean.x1 = mean(rsq.x1), rsq.sd.x1 = sd(rsq.x1),rsq.mean.x2 = mean(rsq.x2), rsq.sd.x2 = sd(rsq.x2) )
tab_rsq_j_aggr$pint = factor(tab_rsq_j_aggr$pint, levels = c("pint", "not_pint"), labels = c("with PINT", "without PINT"))
tab_rsq_j_aggr$n = as.integer(tab_rsq_j_aggr$n)
print(xtable(tab_rsq_j_aggr), include.rownames=FALSE)


# average performance
data_perf = data_spur[[2]] %>% dplyr::group_by(job.id, n) %>% dplyr::summarise(mean = mean(perf))
data_perf %>% group_by(n) %>% dplyr::summarise(mu = mean(mean),sd= sd(mean) )





