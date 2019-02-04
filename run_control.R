### Run Control for parametrization ORYZA Crop file
# EXP, WTH, SOIL folders are requiered

# Load Package, Set Cultivar name, and Wd
library(tidyverse)
library(RCurl)
library(data.table)
library(plotly)

cultivar <- "FED2000"
path <- getwd()

# Run and extract development rates and growth parameters 
source("Run_DRATES_PARAM.R")
source("Extract_DRATES_PARAM.R")


# Get partition tables
source("param_models.R")
source("PARAM_plots.R")

PF_p1
PF_p2
PF_p3
DVS <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))
pf_tbs <- Loess_crp(PF_m2, DVS)
plot_pf(PF_m2, pf_tbs, path, cultivar, span = 0.75, nse=4)


##ORYZA format
test <- split(pf_tbs$mean, pf_tbs$mean$Partition_Parameter)[[1]][-1]

crp_tb(test)





