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
path_data <- paste0(path, "/DATA/")

## Select cultivar to get parameters
cultivar <- "IR64"


# Run and extract development rates and growth parameters 
source("Run_DRATES_PARAM.R")
source("Extract_DRATES_PARAM.R")

## Explore outputs
#DVR_df

# Get partition tables
source("param_models.R")
source("PARAM_plots.R")

## SPGF_cal 
source('D:/03_DEVELOPER/ORYZA_Model_RTOOLS/SPGF_cal.R', encoding = 'UTF-8')

## SLA_cal HERE!!



## Print param plots Exploratory data analysis
# All plots allow interactive with '%>% ggplotly()'

## Plots for Development Rates 
DVR_plot1(DVR_df, save_plot = "Y")  #point
DVR_plot2(DVR_df, save_plot = "N")  #density
DVR_plot3(DVR_df, save_plot = "N")  #stat summary

# Plots for LAI, SLA and CC
## Plots for Biomass Partition ('PF_m1': Oryza_manual,
#                               'PF_m2: Samurai)

BPART_plot1(PF_m1, save_plot = "N") #all data
BPART_plot2(PF_m1, save_plot = "N") #facet by site & pf
BPART_plot3(PF_m1, save_plot = "N") #facet by pf


## Plot for Fraction of carbohydrates allocated to stems that is stored as reserves (FSTR)
FSTR_plot(FSTR_df, save_plot = "N") %>% # by site

## Plot Leaf death coefficient
DRLV_plot (DRLV_df, save_plot = "N")# by 
#ggplotly(DRLV_plot(DRLV_df))


##################################
###
### Crop Parametrization 
###
##################################


## DVS is a numeric vector with lenght>3, Contain the proposed Development stage
#  DVS = 0.00 0.25 0.50 0.75 1.00 1.50 2.00 2.50
DVS <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))

## Compute mean, max and minimun value. Require set span and nse arguments, 
pf_tbs <- Loess_crp(PF_m2, DVS, span = 0.75, nse = 4)
plot_pf_loess(PF_m2, pf_tbs, path, cultivar, span = 0.75, nse=4) %>% ggplotly()
DVS[2] <- 0.25
sla_tbs <- Loess_crp(SLA_df, DVS, span = 0.5, nse = 4)
plot_sla_loess(SLA_df, sla_tbs, path, cultivar, 0.5, 4) 



##
##  ORYZA format params
##

make_param_tibble <- function(summary_dvr, SLA_df, sla_tbs, FSTR_df, lm_spgf, pf_tbs) {
    
    paste_crp <- function(list_tb, param, metric){
    
    list(pf_tbs[[metric]] %>%
        spread(Partition_Parameter, Value) %>%
        select(DVS, param))
    
    }
    
    crp_params <- tibble(
        # 1. Phenological development parameters
        DVRJ = summary_dvr[1,"median"][[1]],
        DVRI = summary_dvr[2,"median"][[1]],
        DVRP = summary_dvr[3,"median"][[1]],
        DVRR = summary_dvr[4,"median"][[1]],
        
        # 2. Leaf and stem growth parameters
        RGRLMX = 0.0085, 
        RGRLMN = 0.0040, 
        SLAMAX = SLA_max(SLA_df)) %>%
        mutate(SLATB = list(as_tibble(sla_tbs$max)), 
               
               # 6. Growth parameters
               FSTR = mean(FSTR_df$FSTR),
               SPGF = round(coef(lm_spgf)[[2]]*1000, 1),
               WGRMX  = 0.0000249, 
               FSHTB = list(tibble(DVS = c(0,0.43,1,2.5),
                                   FSH = c(0.5,0.75,1,1))),
               FLVTB = paste_crp(pf_tbs, "FLV", "mean"),
               FSTTB = paste_crp(pf_tbs, "FST", "mean"),
               FSOTB = paste_crp(pf_tbs, "FSO", "mean"),
               DRLVT = list(tibble(DVS = c(0, 0.6, 1, 1.6, 2.1, 2.5),
                                   DRLV = c(0, 0, 0.015, 0.025, 0.05, 0.05))),
               
               # 8. Root parameters
               GZRT   = 0.01,
               ZRTMCW = 0.25,
               ZRTMCD = 0.40,
               
               # 9. Temperature and drought stress parameters
               COLDREP = 21., 
               CTSTER = 36.5, 
               ULLE = 1.45, 
               LLLE = 1404.,   
               FSWTD = 0.40
        )
    
return(crp_params)    
    
}

crp_params <- make_param_tibble(summary_dvr, SLA_df, sla_tbs, FSTR_df, lm_spgf, pf_tbs)
 

## Make CRP Oryza file 
source("Make_CRP_ORYZA.R")


