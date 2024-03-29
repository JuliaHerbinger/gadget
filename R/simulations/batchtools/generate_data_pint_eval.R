#---------------------------------------------------------------------------------------------------
# GENERATE DATA FOR SIMULATION PINT EVALUATION (SECTION 5 AND APPENDIX F)
#---------------------------------------------------------------------------------------------------


library(batchtools)

source("R/simulations/batchtools/config_pint_eval.R")
lapply(packages, require, character.only = TRUE)



# --- 1. SETUP REGISTRY ---
if (!dir.exists("data/batchtools")) dir.create("data/batchtools", recursive = TRUE)

#unlink("data/batchtools/gadget_sim_pint_eval", recursive = TRUE)
reg = makeExperimentRegistry(file.dir = "data/batchtools/gadget_sim_pint_eval", packages = packages,
                             source = c("R/simulations/batchtools/simulation_setting_definition.R","R/helper_interaction_test.R","R/pint.R", "R/simulations/batchtools/config_pint_eval.R"), seed = 123)




# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions - specify depending on setting of respective section
addProblem(name = "gadget_sim_pint", fun = create_sim_data, reg = reg)

# Appendix F
#pdes = expand.grid(n = c(500), type = c("spur_lin", "spur_nonlin"), dep = c("high", "medium", "no"), beta = c(2, 1.75, 1.5, 1.25, 1, 0.75, 0.5, 0.25), noise = c("yes", "no"))

# Section 5
#pdes = expand.grid(n = c(500), type = c("spur_int"), dep = c("no"), beta = c(3, 2.75, 2.5, 2.25, 2, 1.75, 1.5, 1.25, 1, 0.75, 0.5, 0.25,0), noise = c("yes", "no"))

# Section 5 - counterexample
pdes = expand.grid(n = c(500), type = c("spur_int_non"), dep = c("high"), beta = c(3, 2.75, 2.5, 2.25, 2, 1.75, 1.5, 1.25, 1, 0.75, 0.5, 0.25,0), noise = c("yes"))
pdes = list("gadget_sim_pint" = pdes)

# add aglorithms

# define configurations
ades = expand.grid(obj.function = list(list("SS_L2_pd","SS_L2_ale", "SS_L2_shap_rc")), learner = c("regr.ksvm"), pint = TRUE, stringsAsFactors = FALSE)

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
  repls = 1000L)




# --- 3. SUBMIT JOBS
# Sys.time()
# submitJobs(ids = reg$defs$def.id)
# Sys.time()


