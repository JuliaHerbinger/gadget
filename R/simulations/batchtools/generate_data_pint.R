#---------------------------------------------------------------------------------------------------
# GENERATE DATA FOR SIMULATION PINT (SECTION 6.3)
#---------------------------------------------------------------------------------------------------


library(batchtools)

source("R/simulations/batchtools/config.R")
lapply(packages, require, character.only = TRUE)



# --- 1. SETUP REGISTRY ---
if (!dir.exists("data/batchtools")) dir.create("data/batchtools", recursive = TRUE)

#unlink("data/batchtools/gadget_sim_pint2", recursive = TRUE)
reg = makeExperimentRegistry(file.dir = "data/batchtools/gadget_sim_pint", packages = packages,
                             source = c("R/simulations/batchtools/simulation_setting_definition.R","R/tree_splitting.R","R/helper_interaction_test.R","R/pint.R", "R/simulations/batchtools/config.R"), seed = 123)




# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "gadget_sim_pint", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = c(300, 500), type = c("spur_pint"))
pdes = list("gadget_sim_pint" = pdes)

# add aglorithms

# define configurations
ades = expand.grid(impr.par = list(list(0.15)), obj.function = list(list("SS_L2_pd","SS_L2_ale", "SS_L2_shap_rc")), n.split = 5, learner = c("regr.ksvm"), pint = TRUE, stringsAsFactors = FALSE)

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


