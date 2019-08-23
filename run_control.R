### Run Control for parametrization ORYZA Crop file
# EXP, WTH, SOIL folders are requiered


### Source utils
source("D:/03_DEVELOPER/ORYZA_Model_RTOOLS/utils_crop_model.R", encoding = "UTF-8")


## Load Package
inpack(c("tidyverse", "data.table", "lubridate", "plotly", "soiltexture", "Hmisc"))


## Data requeriment in folders :
#       /DATA/ --> Contain crop model data list
#       /EXP/ ---> Contain Experimental files
#       /WTH/ ---> Contain Weather files by Year
#       /SOIL/ --> Cointain Soil files
## Path location
path <- getwd()
path_data <- paste0(path, "/DATA/")

## Select cultivar to get parameters
cultivar <- "FED2000"

##Load data for each workbook (XLSX)   
data <- map(list.files(path = path_data, 
                       pattern = cultivar), 
            ~read_INPUT_data(paste0(path_data, .)))

##data_list
files <- list.files(path_data, pattern = cultivar) -> names(data)

## sites- locality
sites <- str_sub(files, 1,4)

#Load soil data
soil_data <- read_csv(paste0(path_data, "soil_data.csv" )) %>%
    mutate(SAMPLING_DATE = mdy(SAMPLING_DATE))

### make_exp_oryza

source("D:/03_DEVELOPER/ORYZA_Model_RTOOLS/Make_EXP_ORYZA.R", encoding = "UTF-8")

### make_wth_oryza 
source("D:/03_DEVELOPER/ORYZA_Model_RTOOLS/Make_WTH_ORYZA.R", encoding = "UTF-8")
map(data, ~wdata_list(., path))

### make_soil_oryza
source("D:/03_DEVELOPER/ORYZA_Model_RTOOLS/Make_SOIL_ORYZA.R", encoding = "UTF-8")
map(split(soil_data, soil_data$ID), ~Make_SOIL_ORYZA(., path, SATAV = 25))
map(Soil_by_loc, ~Make_SOIL_ORYZA(., path, SATAV = 25, RIWCLI = 'NO'))


### Inputs sample data 
exp_files <- str_subset(list.files("EXP", pattern = "\\.exp$"), cultivar)
#soil_files <- list.files("SOIL", pattern = "\\.sol$")

### split data into calibration set (cal_set) and evaluation set (eval_set), proportion=0.7
set.seed(1234)
cal_set <- sample(exp_files, length(exp_files)*0.7)    
eval_set <- setdiff(exp_files, cal_set)

# download_ORYZA_Tools()
### Run and extract development rates and growth parameters 
source("Run_DRATES_PARAM.R") ; run_drates_param(cal_set)
source("Extract_DRATES_PARAM.R") ; extract_drates_param(cal_set)

## Explore outputs
#DVR_df

# Get partition tables
source("param_models.R")
source("PARAM_plots.R")

## SPGF_cal 
source('D:/03_DEVELOPER/ORYZA_Model_RTOOLS/SPGF_cal.R', encoding = 'UTF-8')
SPGF_cal(path_data, files)

## SLA_cal HERE!!



## Print param plots Exploratory data analysis
# All plots allow interactive with '%>% ggplotly()'

## Plots for Development Rates 
DVR_plot1(DVR_df, save_plot = "N") #point
DVR_plot2(DVR_df, save_plot = "N")  #density
DVR_plot3(DVR_df, save_plot = "N")  #stat summary

# Plots for LAI, SLA and CC
## Plots for Biomass Partition ('PF_m1': Oryza_manual,
#                               'PF_m2: Samurai)

BPART_plot1(PF_m1, save_plot = "N") #all data
BPART_plot2(PF_m1, save_plot = "N") #facet by site & pf
BPART_plot3(PF_m1, save_plot = "N") #facet by pf


## Plot for Fraction of carbohydrates allocated to stems that is stored as reserves (FSTR)
FSTR_plot(FSTR_df, save_plot = "N")  # by site

## Plot Leaf death coefficient
DRLV_plot (DRLV_df, save_plot = "N")# by 
#ggplotly(DRLV_plot(DRLV_df))

## Plot Spikelet growth factor
SPGF_plot(SPGF_df, save_plot = "N")


##################################
###
### Crop Parametrization 
###
##################################


## DVS is a numeric vector with lenght>3, Contain the proposed Development stage
#  DVS = 0.00 0.25 0.50 0.75 1.00 1.50 2.00 2.50
DVS <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))

## Compute mean, max and minimun value. Require set span and nse arguments, 
pf_tbs <- Loess_crp(PF_m1, DVS, span = 0.75, nse = 4)
plot_pf_loess(PF_m1, pf_tbs, path, cultivar, span = 0.75, nse=4)
DVS[2] <- 0.3
sla_tbs <- Loess_crp(SLA_df, DVS, span = 0.5, nse = 4)
plot_sla_loess(SLA_df, sla_tbs, path, cultivar, 0.5, 4) 


##Param


##
##  ORYZA format params
##

# SPGF value or 'lm'

make_param_tibble <- function(summary_dvr, SLA_df, sla_tbs, pf_tbs, FSTR = 0.2, SPGF = 64900, WGRMX = 0.0000249) {
    
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
               FSTR = FSTR,
               SPGF = SPGF,
               WGRMX  = WGRMX, 
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

crp_params <- make_param_tibble(summary_dvr, SLA_df, sla_tbs, pf_tbs, SPGF = 40000)
 

## Make CRP Oryza file 
source("Make_CRP_ORYZA.R")

source('D:/03_DEVELOPER/ORYZA_Model_RTOOLS/run_ORYZA.R', encoding = 'UTF-8')

run_ORYZA(path, cultivar, cal_set)
run_ORYZA(path, cultivar, eval_set)



source('D:/03_DEVELOPER/ORYZA_Model_RTOOLS/RES_analizer.R', encoding = 'UTF-8')
sim_data <- read_res_exp("fed2000_res.dat")

# data is a list 
extract_sim_var <- function(sim_data, exp_set, variable = "dry_matter") {
    
    vars <- switch(variable, 
           dry_matter = c("date", "DVS", "WAGT", "WLVG", "WST", "WSO", "WLVD"), 
           lai = c("date", "DVS", "LAI"),
           yield = c("date", "DVS", "WRR14"), 
           phen = c("date", "DVS"))
    
    sim_dat <- sim_data %>%
    map(., ~dplyr::select(., one_of(vars))) %>% 
    set_names(str_sub(exp_set, 1, -5)) %>%
    bind_rows(.id = "exp_file") %>% na.omit()
    
    sim_select <- switch(variable, 
                         dry_matter = sim_dat %>% 
                             gather(key = "dm_var", value = "dry_matter", -(1:3)) %>% 
                             as_tibble(), 
                         lai = sim_dat,
                         yield = sim_dat)
    
    return(sim_select)
    
    
}

### extract from base data
extract_obs_var <- function(obs_data, exp_set, variable = "dry_matter") {
    
# vars select shet names required
    vars <- switch(variable, 
                   dry_matter = "PLANT_gro", 
                   lai = "PLANT_gro",
                   yield = "YIELD_obs", 
                   phen = "PHEN_obs")
    
    
    set <- exp_set %>%
        str_sub(1,-5) %>% enframe(name = NULL, value = "exp_file") %>%
        separate(exp_file, c("LOC_ID", "CULTIVAR","PROJECT", "TR_N"), sep = "_", remove = F) %>%
        mutate(ID = paste0(LOC_ID, TR_N, PROJECT),
               soil_file = paste0(LOC_ID, ".sol"))
    
    
    obs_data <- obs_data %>%
        map(., ~.[[vars]]) %>%
        bind_rows() %>% select(-LOC_ID, -CULTIVAR) %>%
        nest(-ID) %>% right_join(set) %>% unnest(data) %>%
        select(-c(LOC_ID, CULTIVAR, PROJECT, TR_N)) 
    
    
    op <- switch(variable, 
                 dry_matter = obs_data %>%
                     rename(date=SAMPLING_DATE) %>%
                     select(ID:WAGT_SD, -contains("LAI")) %>% 
                     select(ID, exp_file, date, contains("OBS")) %>%
                     gather("dm_var", "dry_matter", -(1:3)) %>% 
                     bind_cols(
                         obs_data %>%
                             rename(date=SAMPLING_DATE) %>%
                             select(ID:WAGT_SD, -contains("LAI")) %>%
                             select(ID, date, contains("SD")) %>%
                             gather("sd_var", "dry_matter_sd", -(1:2))) %>%
                     select(-c(date1, ID1, sd_var)) %>%
                     mutate(date= as.Date(date),
                            dm_var = str_sub(dm_var, 1, -5)), 
                 lai = obs_data %>%
                     rename(date=SAMPLING_DATE) %>%
                     select(ID, exp_file, date, contains("LAI")) %>% 
                     mutate(date= as.Date(date)),
                 yield = obs_data,
                 
                 phen = obs_data)
    

    return(op)
    
    
}


phen <- data %>%
    map(., ~.[["PHEN_obs"]]) %>%
    bind_rows() %>% select(-LOC_ID, -CULTIVAR) %>%
    nest(-ID) %>% right_join(set) %>% unnest(data) %>%
    select(-c(LOC_ID, CULTIVAR, PROJECT, TR_N))

phen 







sim_dm <- extract_sim_var(sim_data, cal_set, variable = "dry_matter")
obs_dm <- extract_obs_var(data, cal_set, variable = "dry_matter")


## Plot dm sim vs obs 
sim_dm %>%
    ggplot(aes(date, dry_matter)) +
    geom_line(aes(color = dm_var)) +
    geom_point(data= obs_dm, aes(date, dry_matter, shape=dm_var, color=dm_var)) + 
    geom_errorbar(data = obs_dm, aes(ymin = dry_matter - dry_matter_sd, 
                                         ymax = dry_matter + dry_matter_sd), 
                  width=.2,
                  position=position_dodge(0.2)) + 
    facet_wrap(exp_file~., scales = "free_x") +
    theme_bw()



### test metrics

eval_sim_oryza <- function(obs_data, sim_data, variable = "dry_matter")
    
test <- obs_dm %>%
    left_join(sim_dm, by = c("exp_file", "date", "dm_var"))


#d = 1 - [ ( sum( (obs - sim)^2 ) ] / sum( ( abs(sim - mean(obs)) + abs(obs - mean(obs)) )^2 )
test %>%
    group_by(dm_var) %>%
    summarise(n = n(),
              rmse = sqrt(mean((dry_matter.y - dry_matter.x)^2, na.rm = T)),
              nrmse = rmse/mean(dry_matter.x, na.rm = T),
              d = 1 - ((sum((dry_matter.y - dry_matter.x)^2, na.rm = T))/
                           sum((abs(dry_matter.y - mean(dry_matter.x, na.rm = T)) +
                                    abs(dry_matter.x - mean(dry_matter.x, na.rm = T)))^2, na.rm = T)),
              E = 1 - ((sum((dry_matter.y - dry_matter.x)^2, na.rm = T))/
                           sum((dry_matter.x - mean(dry_matter.x, na.rm = T))^2, na.rm = T)),
              rsq = summary(lm(dry_matter.y ~ dry_matter.x))$r.squared)



