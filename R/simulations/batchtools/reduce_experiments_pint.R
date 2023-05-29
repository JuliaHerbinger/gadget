#---------------------------------------------------------------------------------------------------
# EXTRACT RESULT TABLES OF SIMULATION PINT
#--------------------------------------------------------------------------------------------------- 

reduce_trees = function(ades, pdes, n.repl, problems = NULL, savedir) {

	tab = data.frame(job.id = 1:(nrow(ades)*n.repl))
	tab$n = rep(pdes$n, n.repl)
	tab$type = as.character(rep(pdes$type, each = n.repl))
	tab$learner = as.character(rep(ades$learner, each = n.repl))

	for (typ in unique(tab$type)) {

		    toreduce = tab[tab$type == typ, ]
		    toreduce = ijoin(toreduce, findDone())

		    if (nrow(toreduce) > 0) {

		    	# We only store the evaluation metrics
				  res.tree = reduceResultsDataTable(toreduce, function(x) x$result.tree)
			    res.tree = ijoin(tab, res.tree) 
			    res.pint = reduceResultsDataTable(toreduce, function(x) x$result.pint)
			    res.pint = ijoin(tab, res.pint)
			    res.perf = reduceResultsDataTable(toreduce, function(x) x$perf.test)
			    res.perf = ijoin(tab, res.perf)

				path = file.path(savedir, "5_result_tables")

				if (!dir.exists(path)) 
				  dir.create(path, recursive = TRUE)
	      
	      		savepath = file.path(path, paste0("eval_", typ, "_", "tree.rds"))
	       		saveRDS(res.tree, savepath)  
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "pint.rds"))
	       		saveRDS(res.pint, savepath)  
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "perf.rds"))
	       		saveRDS(res.perf, savepath)  
	
		}
	}
}

library(batchtools)
reg = loadRegistry("data/batchtools/gadget_sim_pint")
pdes = expand.grid(n = c(300, 500), type = rep(c("linear_pint","spur_pint"), each = 1), stringsAsFactors = FALSE)


ades = expand.grid(impr.par = list(list(0.15)), obj.function = list(list("SS_L2_pd","SS_L2_ale", "SS_L2_shap_rc")), n.split = 5, learner = rep(c("regr.ksvm"),4), stringsAsFactors = FALSE)


reduce_trees(ades = ades, pdes = pdes,n.repl = 30, savedir = "data/batchtools/")




