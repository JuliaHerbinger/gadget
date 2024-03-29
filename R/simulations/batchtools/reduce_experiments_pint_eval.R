#---------------------------------------------------------------------------------------------------
# EXTRACT RESULT TABLES OF SIMULATION PINT
#--------------------------------------------------------------------------------------------------- 

reduce_pint = function(ades, pdes, n.repl, problems = NULL, savedir) {

	tab = data.frame(job.id = 1:(nrow(pdes)*nrow(ades)*n.repl))
	tab$learner = as.character(rep(rep(ades$learner, each = n.repl),nrow(pdes)))
	tab$n = rep(pdes$n, each = n.repl*nrow(ades))
	tab$type = as.character(rep(pdes$type, each = nrow(ades)*n.repl))
	tab$dep = as.character(rep(pdes$dep, each = nrow(ades)*n.repl))
	tab$beta = as.character(rep(pdes$beta, each = nrow(ades)*n.repl))
	tab$noise = as.character(rep(pdes$noise, each = nrow(ades)*n.repl))
#	browser()

	for (typ in unique(tab$type)) {

		    toreduce = tab[tab$type == typ, ]
		    toreduce = ijoin(toreduce, findDone())

		    if (nrow(toreduce) > 0) {

		    	# We only store the evaluation metrics
			    res.pint = reduceResultsDataTable(toreduce, function(x) x$result.pint)
			    res.pint = ijoin(tab, res.pint)
			    res.perf = reduceResultsDataTable(toreduce, function(x) x$perf.test)
			    res.perf = ijoin(tab, res.perf)

				path = file.path(savedir, "result_tables_pint2")

				if (!dir.exists(path)) 
				  dir.create(path, recursive = TRUE)
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "pint.rds"))
	       		saveRDS(res.pint, savepath)  
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "perf.rds"))
	       		saveRDS(res.perf, savepath)  
	
		}
	}
}

library(batchtools)
reg = loadRegistry("data/batchtools/gadget_sim_pint_eval2", writeable = TRUE)

# Appendix F
#pdes = expand.grid(n = c(500), type = c("spur_lin", "spur_nonlin"), dep = c("high", "medium", "no"), beta = c(2, 1.75, 1.5, 1.25, 1, 0.75, 0.5, 0.25), noise = c("yes", "no"), stringsAsFactors = FALSE)

# Section 5
#pdes = expand.grid(n = c(500), type = c("spur_int"), dep = c("no"), beta = c(3, 2.75, 2.5, 2.25, 2, 1.75, 1.5, 1.25, 1, 0.75, 0.5, 0.25,0), noise = c("yes", "no"))

# Section 5 - counterexample
pdes = expand.grid(n = c(500), type = c("spur_int_non"), dep = c("high"), beta = c(3, 2.75, 2.5, 2.25, 2, 1.75, 1.5, 1.25, 1, 0.75, 0.5, 0.25,0), noise = c("yes"))

ades = expand.grid(obj.function = list(list("SS_L2_pd","SS_L2_ale", "SS_L2_shap_rc")), learner = c("regr.ksvm"), stringsAsFactors = FALSE)

reduce_pint(ades = ades, pdes = pdes,n.repl = 1000, savedir = "data/batchtools/")




