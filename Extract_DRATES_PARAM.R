#########################################################
####       READ  DRATE.OUT  and param.out            ####
####     By https://github.com/jrodriguez88          ####
#########################################################

################################
#### Load Requeriments
################################
#knitr:::input_dir()
library(ggplot2)
library(tidyverse)
library(stringr)
library(magrittr)
library(data.table)
library(plyr)
#if(require(openxlsx)==FALSE){install.packages("openxlsx")}


# Work directory  :: #dirFol    <- "C:/Users/nameUser/Desktop/workspace/"
#dirFol    <- "C:/Users/jrespinosa/Dropbox/2017/ORYZA/4. Practica Parametros/CT21375/"
setwd(dirFol)
dir.create(paste0(dirFol,"/_OutPut_Df"),showWarnings=F)
dir.create(paste0(dirFol,"/_OutPut_Graphic"),showWarnings=F)
dir.create(paste0(dirFol,"/_OutPut_Graphic/_1_Development_Rates_"),showWarnings=F)
dir.create(paste0(dirFol,"/_OutPut_Graphic/_2_Growth_"),showWarnings=F)
dir.create(paste0(dirFol,"/_OutPut_Graphic/_3_Partitioning_Factors"),showWarnings=F)
dir.create(paste0(dirFol,"/_OutPut_Graphic/_4_Partitioning_Factors"),showWarnings=F)


#file <- "DRATE.OUT"
#file <- "param.out"

################################
### Create EXP data.frame
################################
# Make Master Table called "exp_df"

#exp_names <- list.files("EXP",pattern = "\\.exp$")

exp_df <- list.files("EXP",pattern = "\\.exp$")
    exp_df <- substring(exp_df,1,nchar(exp_df)-4) %>%
    str_split(pattern = "_") 
    exp_df <- as.data.frame(do.call("rbind",exp_df))
    colnames(exp_df) <-c("LOC_ID", "CULTIVAR","PROJECT", "TR_N" )
    exp_df$ID <- paste0(exp_df$LOC_ID, exp_df$TR_N, exp_df$PROJECT)
           
################################
### Create DVR data.frame       
################################

read_DVR_drate <- function(file){
    
    find_DVR <- file %>%
        read_lines() %>%
        str_detect(pattern = "crop development") %>%
        which() %>%
        +0
    
    DVR <- list()
    for (i in 1:length(find_DVR)){
        DVR[[i]] <- read_lines(file, skip = find_DVR[i], n_max = 4) %>%
            str_split(pattern = "=") %>%
            sapply("[", 2) %>%
            as.numeric()%>%
            matrix(ncol = 4)%>%
            tbl_df()
        
        
        for (i in 1:length(DVR)) {
            
            DVR[[i]]$ID<- exp_df$ID[i]
            DVR[[i]]$LOC_ID<- exp_df$LOC_ID[i]
            DVR[[i]]$CULTIVAR<- exp_df$CULTIVAR[i]
            DVR[[i]]$PROJECT<- exp_df$PROJECT[i]
            DVR[[i]]$TR_N<- exp_df$TR_N[i]
        }
        
    }
    DVR_df <- ldply(DVR, data.frame)
    colnames(DVR_df) <- c("DVRJ", "DVRI", "DVRP", "DVRR", "ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N")
    
    return(DVR_df[c(5:9,1:4)])
    
}
DVR_df <- read_DVR_drate("DRATE.OUT")
#str(DVR)

################################
### Create PHEN data.frame
################################

read_PHEN_drate <- function(file){
    find_TS <- file %>%
        read_lines() %>%
        str_detect(pattern = "TSTR") %>%
        which()
    
    TS <- list()
    for (i in 1:length(find_TS)){
        TS[[i]] <- read_lines(file, skip = find_TS[i], n_max = 1) %>%
            str_split(pattern = ",")
        TS[[i]] <- as.data.frame(do.call("rbind",TS[[i]]))
    } 
    
    GDD <- list()
    for (i in 1:length(find_TS)){
        GDD[[i]] <- read_lines(file, skip = find_TS[i]+2, n_max = 1) %>%
            str_split(pattern = ",")
        GDD[[i]] <- as.data.frame(do.call("rbind",GDD[[i]]))
    } 
    
    DAE <- list()
    for (i in 1:length(find_TS)){
        DAE[[i]] <- read_lines(file, skip = find_TS[i]+4, n_max = 1) %>%
            str_split(pattern = ",")
        DAE[[i]] <- as.data.frame(do.call("rbind",DAE[[i]]))
    } 
    PHEN <- list()
    for (i in 1:length(find_TS)) {
        PHEN[[i]] <- cbind(TS[[i]], GDD[[i]], DAE[[i]]) 
    }
    
    for (i in 1:length(find_TS)) {
        
        PHEN[[i]]$ID<- exp_df$ID[i]
        PHEN[[i]]$LOC_ID<- exp_df$LOC_ID[i]
        PHEN[[i]]$CULTIVAR<- exp_df$CULTIVAR[i]
        PHEN[[i]]$PROJECT<- exp_df$PROJECT[i]
        PHEN[[i]]$TR_N<- exp_df$TR_N[i]
    }
    
    
    PHEN_df <- ldply(PHEN, data.frame)%>%
        tbl_df()%>%
        `colnames<-`(c("TSTR", "TSPI", "TSF",  "TSM",
                       "TGDDTR","TGDDPI","TGDDF","TGDDM",
                       "DAETR" , "DAEPI" , "DAEF"  , "DAEM",
                       "ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N"))
    PHEN_df[, 1:12] <- sapply(PHEN_df[, 1:12],as.character)
    PHEN_df[, 1:12] <- sapply(PHEN_df[, 1:12],as.numeric)
    return(PHEN_df[c(13:17, 1:12)])
    
} 
PHEN_df <- read_PHEN_drate("DRATE.OUT")
#str(PHEN_df)

###############################
### Create Biomass data.frame
###############################
    
read_biomass_param <- function(file){
    
    find_BM <- file %>%
        read_lines() %>%
        str_detect(pattern ="biomass values") %>%
        which() %>%
        -0
    
    BM <- list()
    for (i in 1:length(find_BM)){

    BM[[i]] <- suppressWarnings(fread(file, autostart = find_BM[i], showProgress = FALSE)) %>%
        tbl_df() %>%
        mutate(DVS  = as.numeric(DVS), 
               WLVG = as.numeric(WLVG), 
               WLVD = as.numeric(WLVD),
               WST  = as.numeric(WST),
               WSO  = as.numeric(WSO))%>%
        dplyr::select(everything())
    
        for (i in 1:length(BM)) {
        
            BM[[i]]$ID<- exp_df$ID[i]
            BM[[i]]$LOC_ID<- exp_df$LOC_ID[i]
            BM[[i]]$CULTIVAR<- exp_df$CULTIVAR[i]
            BM[[i]]$PROJECT<- exp_df$PROJECT[i]
            BM[[i]]$TR_N<- exp_df$TR_N[i]
        }
    
    }
    BM_df <- ldply(BM, data.frame)
    colnames(BM_df)[6:10]<- c("ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N")
    return(BM_df[c(6:10, 1:5)])
        
}
BMASS_df <- read_biomass_param("param.out")
#str(Biomass)

###############################
### Create Biomass Partition data.frame
###############################
    
read_biomass_partition_param <- function(file){
    
    find_PF <- file %>%
        read_lines() %>%
        str_detect(pattern ="FSO") %>%
        which()
        
    
    PF <- list()
    for (i in 1:length(find_PF)){
        
        PF[[i]] <- suppressWarnings(fread(file, autostart = find_PF[i] , showProgress = FALSE)) 
            colnames(PF[[i]]) <- c("DVSM", "FLV", "FST", "FSO")
                    PF[[i]] <- PF[[i]][2:nrow(PF[[i]]),]%>%
            mutate(DVSM  = as.numeric(DVSM), 
                   FLV = as.numeric(FLV), 
                   FST = as.numeric(FST),
                   FSO  = as.numeric(FSO))
        
        for (i in 1:length(PF)) {
            
            PF[[i]]$ID<- exp_df$ID[i]
            PF[[i]]$LOC_ID<- exp_df$LOC_ID[i]
            PF[[i]]$CULTIVAR<- exp_df$CULTIVAR[i]
            PF[[i]]$PROJECT<- exp_df$PROJECT[i]
            PF[[i]]$TR_N<- exp_df$TR_N[i]
        }
        
    }
    PF_df <- ldply(PF, data.frame)
    colnames(PF_df)[5:9]<- c("ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N")
    
    return(PF_df[c(5:9, 1:4)])
    
}
BPART_df <- read_biomass_partition_param("param.out")
#str(BPartition_f)

###############################
### Create LAI data.frame
###############################
    
read_LAI_param <- function(file){
    find_LAI <- file %>%
        read_lines() %>%
        str_detect(pattern = "Observed LAI") %>%
        which() %>%
        +1
    End_LAI <- file %>%
        read_lines() %>%
        str_detect(pattern = "Calculated ORYZA1") %>%
        which() %>%
        -3
    LAI <- list()
    for (i in 1:length(find_LAI)){
        LAI[[i]] <- read_lines(file, skip = find_LAI[i], n_max = End_LAI[i]-find_LAI[i]) %>%
            str_split(pattern = "      ")
        LAI[[i]] <- as.data.frame(do.call("rbind",LAI[[i]]))

      if (nrow(LAI[[i]])<1){ 
      LAI[[i]]<- data.frame(matrix(nrow = 1, ncol = 2))
      }  
    } #Extract LAI from param.out --> list()
        
               
    for (i in 1:length(find_LAI)) {
        
        LAI[[i]]$ID<- exp_df$ID[i]
        LAI[[i]]$LOC_ID<- exp_df$LOC_ID[i]
        LAI[[i]]$CULTIVAR<- exp_df$CULTIVAR[i]
        LAI[[i]]$PROJECT<- exp_df$PROJECT[i]
        LAI[[i]]$TR_N<- exp_df$TR_N[i]
    } # Assing ID
        
    
    LAI_df <- ldply(LAI, data.frame)%>%
        tbl_df()%>%
       `colnames<-`(c("DVS", "LAI","ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N"))%>%
        mutate(DVS =as.numeric(as.character(DVS)),
               LAI =as.numeric(as.character(LAI)))
#        na.omit()
#    `colnames<-`(c("DVS", "LAI","ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N"))

#   as.numeric(gsub("([0-9]+).*$", "\\1", LAI_df$DVS))
    return(LAI_df[c(3:7, 1:2)])
    
}
LAI_df <- read_LAI_param("param.out")
#str(LAI)
#LAI_SLA <- dplyr::left_join(LAI, Biomass, by="ID")
#file <- "DRATE.OUT"


##############################
### Create csv files
##############################
    
#file.names <-list("DVR_df", "PHEN_df", "BMASS_df", "BPART_df", "LAI_df")
write.csv.df <- function(df){
        df.name <- deparse(substitute(df))
        write.csv(df, paste0(dirFol,"//_OutPut_Df//",df.name,".csv"),row.names=F,quote =F)
    }
    write.csv.df(DVR_df)
    write.csv.df(PHEN_df)
    write.csv.df(BMASS_df)
    write.csv.df(BPART_df)
    write.csv.df(LAI_df)

#write.xlsx(PHEN_df,  file=paste0(dirFol,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet1", row.names=FALSE)
#write.xlsx(DVR_df,   file=paste0(dirFol,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet2", append=TRUE, row.names=FALSE)
#write.xlsx(BMASS_df, file=paste0(dirFol,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet3", append=TRUE, row.names=FALSE)
#write.xlsx(BPART_df, file=paste0(dirFol,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet4", append=TRUE, row.names=FALSE)
#write.xlsx(LAI_df,   file=paste0(dirFol,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet5", append=TRUE, row.names=FALSE)

#list_of_datasets <- list("DVR_df"=DVR_df, "PHEN_df="=PHEN_df, "BMASS_df"=BMASS_df, "BPART_df"=BPART_df, "LAI_df"=LAI_df)
#write.xlsx(list_of_datasets, file = "writeXLSX2.xlsx")
