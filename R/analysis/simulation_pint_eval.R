library(xtable)
source("R/helper_general.R")
source("R/load_packages.R")
source("R/simulations/helper_pint.R")

#---------------------------------------------------------------------------------------------------------------

# SIMULATION STUDY SECTION 6.3 (SPURIOUS INTERACTIONS AND PINT) 

#---------------------------------------------------------------------------------------------------------------

# Before this file can be run, the data needs to be generated (R/simulations/batchtools/generate_data_pint.R)
# and results need to be reduced (R/simulations/batchtools/reduce_experiments_pint.R)

#---------------------------------------------------------------------------------------------------------------

# path to folder, with folders for dataset results
path = "data/batchtools/result_tables_pint/"
datasets = list.files(path)
type = c("spur_pint")


# Analysis for PINT compared to HStatistics
data_pint = lapply(type, function(typ){
  
  method = "pint"
  
  res.tree = readRDS(file.path(path, paste0("eval_", typ, "_", method, ".rds")))
  #browser()
  for(i in 1:nrow(res.tree)){
    df_sub = as.data.frame(res.tree[i,])
    df_sub_res = data.frame(rbind(rbind(df_sub$result[[1]]$pd$pint,df_sub$result[[1]]$ale$pint),df_sub$result[[1]]$shap$pint ))
    colnames(df_sub_res) = paste0("x",1:ncol(df_sub_res))
    df_sub_res = gather(df_sub_res, feature, pint, colnames(df_sub_res), factor_key=TRUE)
    df_sub_res$method = rep(c("pd","ale","sd"), nlevels(df_sub_res$feature))
    
    df_sub = cbind(df_sub[,which(colnames(df_sub) != "result")], df_sub_res)
    
    
    if(i == 1) df = df_sub
    else df = rbind(df, df_sub)
  }
  df
  
})

data_pint_spur = data_pint[[1]]
data_pint_spur$rejection = ifelse(data_pint_spur$pint <= 0.05, 1, 0)
data_pint_spur %>% dplyr::group_by(feature, method) %>% dplyr::summarise(rejection_proportion = mean(rejection))


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





# average performance
data_perf = data_spur[[2]] %>% dplyr::group_by(job.id, n) %>% dplyr::summarise(mean = mean(perf))
data_perf %>% group_by(n) %>% dplyr::summarise(mu = mean(mean),sd= sd(mean) )





