# --- 0. SETUP ---

source("R/simulations/helper_simulation.R")
source("R/simulations/batchtools/simulation_setting_definition.R")
source("R/tree_splitting.R")
source("R/helper_effects.R")
source("R/helper_tree_splitting.R")
source("R/helper_general.R")

# - packages - 
packages = c(
  "batchtools",  
  "mlr", 
  "reshape2", 
  "stringr", 
  "iml", 
  "mgcv",
  "ranger",
  "BBmisc",
  #"tidyverse",
  "kernlab",
  "data.table",
  "dplyr",
  "checkmate",
  "rlist",
  "mlr3",
  "mlr3learners",
  "Rmalschains",
  #"kmlShape",
  "dtw",
  "egg",
  "rlist",
  "vip",
  "dplyr",
  "tidyr",
  "fastshap",
  "xgboost"
  
) 

lapply(packages, library, character.only = TRUE)

