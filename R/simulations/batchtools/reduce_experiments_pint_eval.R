#---------------------------------------------------------------------------------------------------
# EXTRACT RESULT TABLES OF SIMULATION PINT
#--------------------------------------------------------------------------------------------------- 

reduce_pint = function(ades, pdes, n.repl, problems = NULL, savedir) {

	tab = data.frame(job.id = 1:(nrow(pdes)*nrow(ades)*n.repl))
	tab$learner = as.character(rep(ades$learner, n.repl*nrow(ades)))
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
reg = loadRegistry("data/batchtools/gadget_sim_pint_test")
pdes = expand.grid(n = c(300), type = rep(c("spur_pint"), each = 1), stringsAsFactors = FALSE)
pdes = expand.grid(n = c(300, 500), type = c("spur_lin", "spur_nonlin"), dep = c("high", "medium"), beta = c(2, 1), noise = c("yes", "no"), stringsAsFactors = FALSE)

ades = expand.grid(obj.function = list(list("SS_L2_pd","SS_L2_ale", "SS_L2_shap_rc")), learner = c("regr.ksvm"), stringsAsFactors = FALSE)
ades = expand.grid(obj.function = list(list("SS_L2_pd","SS_L2_ale", "SS_L2_shap_rc")), learner = c("regr.ksvm", "regr.ranger"), pint = TRUE, stringsAsFactors = FALSE)

reduce_pint(ades = ades, pdes = pdes,n.repl = 1, savedir = "data/batchtools/")




