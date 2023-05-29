# GADGET: Generalized Additive Decomposition of Global EffecTs

This repository gives access to an implementation of the methods
presented in the paper “Decomposing Global Feature Effects Based on Feature Interactions”, 
as well as all code that was used for the
experiments and the real-world examples.

This repository is structured as follows:

``` 
    ├── R/                       # All implemented methods and general helper functions  
    |   ├── analysis/            # Scripts used to create figures and tables in the paper for simulation examples (Sec. 3 - 6 and Appendix)
    |   ├── applications/        # Scripts used for modelling and to create figures for real-world examples in Section 7
    |   ├── simulations/         # helper functions for simulations
    |   |   ├── batchtools/      # Scripts used to create data for simulation experiments (Sec. 6.1-6.3)
    ├── data/                    # Location where all generated data are stored
    │   ├── application/         # Location where datasets for real-world examples are stored
    │   ├── batchtools/          # Location where data generated by simulation experiments are stored
    |   figures/                 # Location where all figures are stored
    ├── LICENSE
    └── README.md               
```



## Reproduce Experiments

Steps to reproduce the experiments of Sections 6.1 to 6.3.

1.  Install all required packages.

<!-- end list -->

``` r
# from CRAN
install.packages(c("ranger", "dplyr", "batchtools", "mlr", "ggplot2", "tidyr", "reshape2",
"ggpubr", "BBmisc", "data.table", "stringi", "stringr", "checkmate", "kernlab", "xtable", "devtools",
"tidyverse", "Rmalschains", "iml", "dtw","egg","rlist","mgcv","mvtnorm", "vip", "data.table",
"e1071", "RColorBrewer", "R6", "mlr3", "xgboost", "fastshap", "patchwork", "nnet"))

```

2.  Create an experimental registry, add experiments and problem and run simulations via
    script `R/simulations/generate_data_*.R` where `*` stands for `cor` (Sec. 6.1), `higher_order` (Sec. 6.2) or `pint` (Sec. 6.3). Data produced by the scripts is stored in 
    the subfolder `data/batchtools/` as a separate registry.
    
3.  Prepare data for analysis by running the script `R/simulations/reduce_experiments_*.R` where `*` stands for `cor` (Sec. 6.1), `higher_order` (Sec. 6.2) or `pint` (Sec. 6.3).

4.  To reproduce figures and tables of Section 6 and respective Appendices, run the scripts in folder `R/analysis/`. Figures produced within the script are stored in `figures`.


To reproduce the experiments of Sections 3 and 4, run the scripts `R/analysis/create_plots_sec_3_and_4.R`. To reproduce the results of the real-world examples, run the scripts in `R/applications/` 
