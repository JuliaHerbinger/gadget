#---------------------------------------------------------------------------------------------------
# GENERATE DATA FOR SIMULATION EXTRAPOLATION (SECTION 6.1)
#---------------------------------------------------------------------------------------------------


library(batchtools)
source("R/simulations/batchtools/config.R")
lapply(packages, require, character.only = TRUE)



# --- 1. SETUP REGISTRY ---
if (!dir.exists("data/batchtools")) dir.create("data/batchtools", recursive = TRUE)

#unlink("data/batchtools/gadget_sim_cor", recursive = TRUE)
reg = makeExperimentRegistry(file.dir = "data/batchtools/gadget_sim_cor", packages = packages,
                             source = c("R/simulations/batchtools/simulation_setting_definition.R",
                                        "R/tree_splitting.R","R/simulations/batchtools/config.R"), 
                             seed = 123)




# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "gadget_sim_cor", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = c(1000), type = c("xor"), cor = c(0,0.3,0.5,0.7))
pdes = list("gadget_sim_cor" = pdes)

# add aglorithms

# define configurations
ades = expand.grid(impr.par = list(list(0.2)), obj.function = list(list("SS_L2_pd","SS_L2_ale","SS_L2_shap_rc")), 
                   n.split = 5, learner = c("gam", "regr.nnet"), stringsAsFactors = FALSE)

ALGORITHMS = list(
  get_sim_results = list(fun = get_sim_results, ades = ades
  ))

ades = lapply(ALGORITHMS, function(x) x$ades)

# add all algorithms
for (i in 1:length(ALGORITHMS)) {
  addAlgorithm(name = names(ALGORITHMS)[i], reg = reg, fun = ALGORITHMS[[i]]$fun)  
}


# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  repls = 30L)



# --- 3. SUBMIT JOBS
# Sys.time()
# submitJobs(ids = reg$defs$def.id)
# Sys.time()


