#---------------------------------------------------------------------------------------------------
# EXTRACT RESULT TABLES OF SIMULATION PINT
#--------------------------------------------------------------------------------------------------- 

reduce_pint = function(ades, pdes, n.repl, problems = NULL, savedir) {

	tab = data.frame(job.id = 1:(nrow(ades)*n.repl))
	tab$n = rep(pdes$n, n.repl)
	tab$type = as.character(rep(pdes$type, each = n.repl))
	tab$learner = as.character(rep(ades$learner, each = n.repl))

	for (typ in unique(tab$type)) {

		    toreduce = tab[tab$type == typ, ]
		    toreduce = ijoin(toreduce, findDone())

		    if (nrow(toreduce) > 0) {

		    	# We only store the evaluation metrics
			    res.pint = reduceResultsDataTable(toreduce, function(x) x$result.pint)
			    res.pint = ijoin(tab, res.pint)
			    res.perf = reduceResultsDataTable(toreduce, function(x) x$perf.test)
			    res.perf = ijoin(tab, res.perf)

				path = file.path(savedir, "result_tables_pint")

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
reg = loadRegistry("data/batchtools/gadget_sim_pint_eval")
pdes = expand.grid(n = c(300), type = rep(c("spur_pint"), each = 1), stringsAsFactors = FALSE)


ades = expand.grid(obj.function = list(list("SS_L2_pd","SS_L2_ale", "SS_L2_shap_rc")), learner = c("regr.ksvm"), stringsAsFactors = FALSE)


reduce_pint(ades = ades, pdes = pdes,n.repl = 1000, savedir = "data/batchtools/")




