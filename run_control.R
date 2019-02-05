### Run Control for parametrization ORYZA Crop file
# EXP, WTH, SOIL folders are requiered

## Load Package, Set Cultivar name, and Wd
library(tidyverse)
library(RCurl)
library(data.table)
library(plotly)

## Data requeriment in folders :
#       /EXP/ ---> Contain Experimental files
#       /WTH/ ---> Contain Weather files by Year
#       /SOIL/ --> Cointain Soil files
## Path location
path <- getwd()

## Select cultivar to get parameters
cultivar <- "FED2000"


# Run and extract development rates and growth parameters 
source("Run_DRATES_PARAM.R")
source("Extract_DRATES_PARAM.R")


# Get partition tables
source("param_models.R")
source("PARAM_plots.R")

## Print param plots Exploratory data analysis
# Convert PARAM_plots to function list

DVR_plot1(DVR_df, save_plot = "Y")
DVR_plot2(DVR_df, save_plot = "Y")
DVR_plot3(DVR_df, save_plot = "Y")

FSTR_plot(FSTR_df, save_plot = "Y")
BPART_plot1(PF_m2, save_plot = "Y")
DRLV_plot (DRLV_df, save_plot = "Y")
#ggplotly(DRLV_plot(DRLV_df))
### Crop Parametrization 

## DVS is a numeric vector with lenght>3, Contain the proposed Development stage
#  DVS = 0.00 0.25 0.50 0.75 1.00 1.50 2.00 2.50
DVS <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))

## Compute mean, max aand minimun value  
pf_tbs <- Loess_crp(PF_m2, DVS)
plot_pf(PF_m2, pf_tbs, path, cultivar, span = 0.75, nse=4)


##ORYZA format
test <- split(pf_tbs$mean, pf_tbs$mean$Partition_Parameter)[[1]][-1]

crp_tb(test)



## Make CRP Oryza file 
source("Make_CRP_ORYZA.R")


