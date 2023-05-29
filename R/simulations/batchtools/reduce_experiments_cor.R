#---------------------------------------------------------------------------------------------------
# EXTRACT RESULT TABLES OF SIMULATION EXTRAPOLATION
#---------------------------------------------------------------------------------------------------

reduce_trees = function(ades, pdes, n.repl, problems = NULL, savedir) {

	tab = data.frame(job.id = 1:(nrow(ades)*n.repl))
	tab$n = rep(pdes$n, n.repl)
	tab$type = as.character(rep(pdes$type, each = n.repl))
	tab$learner = as.character(rep(ades$learner, each = n.repl))
	tab$cor = as.character(rep(pdes$cor, each = n.repl))

	for (typ in unique(tab$type)) {

		    toreduce = tab[tab$type == typ, ]
		    toreduce = ijoin(toreduce, findDone())

		    if (nrow(toreduce) > 0) {

		    	# We only store the evaluation metrics
				  res.tree = reduceResultsDataTable(toreduce, function(x) x$result.tree)
			    res.tree = ijoin(tab, res.tree) 
			    res.perf = reduceResultsDataTable(toreduce, function(x) x$perf.test)
			    res.perf = ijoin(tab, res.perf)

				path = file.path(savedir, "3_result_tables")

				if (!dir.exists(path)) 
				  dir.create(path, recursive = TRUE)
	      
	      		savepath = file.path(path, paste0("eval_", typ, "_", "tree3.rds"))
	       		saveRDS(res.tree, savepath)  
	       		
	       		savepath = file.path(path, paste0("eval_", typ, "_", "perf3.rds"))
	       		saveRDS(res.perf, savepath)  
	
		}
	}
}

library(batchtools)
reg = loadRegistry("data/batchtools/gadget_sim_cor")
pdes = expand.grid(n = c(1000), type = rep(c("xor"), each = 1), cor = rep(c(0,0.3,0.5,0.7), each = 1), stringsAsFactors = FALSE)
ades = expand.grid(impr.par = list(list(0.2)), obj.function = list(list("SS_L2_pd","SS_L2_ale", "SS_L2_shap_rc")), n.split = 5, learner = rep(c("gam", "regr.nnet"),8), stringsAsFactors = FALSE)


reduce_trees(ades = ades, pdes = pdes,n.repl = 30, savedir = "data/batchtools/")


reg = loadRegistry("data/batchtools/gadget_sim_cor")
reduce_trees(ades = ades, pdes = pdes,n.repl = 30, savedir = "data/batchtools/")

