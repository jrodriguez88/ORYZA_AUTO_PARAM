#########################################################
####       Run DRATES.EXE and PARAM.EXE              ####
####     By https://github.com/jrodriguez88          ####
#########################################################

#######################################################################################
#####                                                                             #####
##### GO to ---> Session ---> Set Working Directory ---> To Source File Location  #####
#####                                                                             #####
#######################################################################################

#########################
### Load Requeriments ###
#########################
if(require(RCurl)==FALSE){install.packages("RCurl")}
if(require(tidyverse)==FALSE){install.packages("tidyverse")}
#if(require(magrittr)==FALSE){install.packages("magrittr")}
if(require(data.table)==FALSE){install.packages("data.table")}
#if(require(plyr)==FALSE){install.packages("plyr")}

#if(require(xlsx)==FALSE){install.packages("xlsx")}
# Work directory  :: #dirFol    <- "C:/Users/nameUser/Desktop/workspace/"
#dirFol    <- "C:/Users/jrespinosa/Dropbox/2017/ORYZA/4. Practica Parametros/CT21375/"

dirFol <- getwd()
download_ORYZA_Tools <- function(){
    havingIP <- function() {
        if (.Platform$OS.type == "windows") {
            ipmessage <- system("ipconfig", intern = TRUE)
        } else {
            ipmessage <- system("ifconfig", intern = TRUE)
        }
        validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
        any(grep(validIP, ipmessage))
    }  
    
  if ((file.exists("ORYZA3.exe")) &&
      (file.exists("drate(v2).exe")) &&
      (file.exists("PARAM(v2).exe")) &&
      (file.exists("standard.crp"))){
  } else if(havingIP()==T){

    # Download DRATES and PARAM app  
    download.file(url='https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/AllTools.zip',
              destfile='AllTools.zip', method='auto')
    ls_tools<- unzip('AllTools.zip', list = T)
    unzip('AllTools.zip', files = ls_tools$Name[c(2,4)])
    
    # Download ORYZA.exe
    download.file(url='https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/ORYZA3.zip',
                  destfile='ORYZA3.zip', method='auto')
    unzip('ORYZA3.zip', files="ORYZA3.exe")

    #Download standard.crp
    download.file("https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/standard.crp",
              destfile = "standard.crp", method='auto')

    file.remove('AllTools.zip')
    file.remove('ORYZA3.zip')
} else {
    mens <- rbind(paste0('#####################################################'),
                  paste0('####       WARNING! NO INTERNET CONECTION        ####'),
                  paste0('####      It is need copy ORYZA model Tools:     ####'),
                  paste0('####  ORYZA3.exe & drate(v2).exe & PARAM(v2).exe ####'),
                  paste0('####        AND CROP FILE standard.crp           ####'),
                  paste0('#####################################################'))
    
    stopifnot((file.exists("ORYZA3.exe")) &&
                  (file.exists("drate(v2).exe")) &&
                  (file.exists("PARAM(v2).exe")) &&
                  (file.exists("standard.crp")))
    
    print(mens)
}

}
download_ORYZA_Tools()

#setwd(dirFol)
#file <- "DRATE.OUT"
########################################################
### *.EXP files must be copy to folder called "EXP"  ###
########################################################

exp_names <- str_subset(list.files("EXP",pattern = "\\.exp$"), "FED2000")

#######################
### Create PARAM.in ###
#######################

make_param <- function(rer,file_name){
    
    if(file.exists(file_name)){
        
        file.remove(file_name)
        
    } 

sink(file = file_name, append = T)
for (i in 1:1) {
    
    cat('*PARAMFILE = PARAM.in', sep = '\n')
    cat('strun = 1', sep = '\n')
    cat('*endrun = 72', sep = '\n')
    cat('*----------------------------------------------------------------------*', sep = '\n')
    cat('* control file for ORYZA model AUTOCOLIBRATION                         *', sep = '\n')
    cat('*----------------------------------------------------------------------*', sep = '\n')
    cat(paste("FILEOP = ", "'","PARAM.OUT","'", sep = ''), sep = '\n')
    cat(paste("FILEOR = ", "'","DRATE.OUT","'", sep = ''), sep = '\n')
    cat(paste("FILEOL = ", "'","MODEL.LOG","'", sep = ''), sep = '\n')
    cat(paste("FILEIR = ", "'","reruns",rer,".rer","'", sep = ''), sep = '\n')
    cat(paste("FILEIT = ", "'", "EXP", "\\", exp_names[1], "'", sep=""), sep = '\n')
    cat(paste("FILEI1 = ", "'standard.crp'", sep = ""), sep = '\n')
    cat('PRDEL  = 1.', sep = '\n')
    cat('IPFORM = 5' , sep = '\n')
    cat(paste("COPINF = ", "'","N","'", sep = ''), sep = '\n')
    cat(paste("DELTMP = ", "'","N","'", sep = ''), sep = '\n')
    cat('IFLAG  = 1100', sep = '\n')
}
sink()
}
make_param(1,"PARAM.IN")

############################################
### Create Reruns1, file for DRATES.EXE  ###
############################################

make_reruns1 <- function(file_name="reruns1.rer"){
    
    if(file.exists(file_name)){
        
        file.remove(file_name)
        
    }
    
sink(file = file_name, append = T)
for(i in 1:length(exp_names)){
    
            cat('********************', sep = '\n')
            cat('\n')
            cat(paste("* Rerun set", i," - ", exp_names[i]), sep = '\n')
            cat(paste("FILEIT = ", "'", "EXP", "\\", exp_names[i], "'", sep=""), sep = '\n')
            cat(paste("FILEI1 = ", "'standard.crp'", sep = ""), sep = '\n')
            cat('\n')

}
sink()
}
make_reruns1()

######################
### Run DRATES.EXE ###
######################

system("drate(v2).exe")
Sys.sleep(1)

##########################################
### Create Reruns2, file for PARAM.EXE ###
##########################################

make_reruns2 <- function(file="DRATE.OUT"){
    
    find_DVR <- file %>%
        read_lines() %>%
        str_detect(pattern = "crop development") %>%
        which() %>%
        +0
    
    DVR2 <- list()
    for (i in 1:length(find_DVR)){
        DVR2[[i]] <- read_lines(file, skip = find_DVR[i], n_max = 4) 
        
    }
    
    if(file.exists("reruns2.rer")){
        
        file.remove("reruns2.rer")
        
    }

sink(file = "reruns2.rer", append = T)
for(i in 1:length(exp_names)){
    
    cat('********************', sep = '\n')
    cat('\n')
    cat(paste("* Rerun set", i," - ", exp_names[i]), sep = '\n')
    cat(paste("FILEIT = ", "'", "EXP", "\\", exp_names[i], "'", sep=""), sep = '\n')
    cat(paste("FILEI1 = ", "'standard.crp'", sep = ""), sep = '\n')
    cat(DVR2[[i]][1], sep = '\n')
    cat(DVR2[[i]][2], sep = '\n')
    cat(DVR2[[i]][3], sep = '\n')
    cat(DVR2[[i]][4], sep = '\n')
    cat('\n')
    
}
sink()

}
make_reruns2()

#####################
### Run PARAM.EXE ###
#####################

make_param(2,"PARAM.IN")
Sys.sleep(3)
system("PARAM(v2).exe")

######################################
### TEST OUTPUT=TRUE//DELETE *.BIN ###
######################################

Test <- function(ms){
    mens <- rbind(paste0('###############################################'),
                  paste0('####  The process was performed correctly. ####'),
                  paste0('####      --->  Go to  _OutPut Folders   . ####'),
                  paste0('###############################################'))
    
if((file.exists(ms)==T) && (file.info(ms)$size>10000)==T)
    {
        if (file.exists("res.bin")==T){file.remove("res.bin")}
    
        print(mens)
        
} else 
    {system("PARAM(v2).exe")
     if (file.exists("res.bin")==T){ 
         file.remove("res.bin")}
     print(mens)
    }
    
}
Test("param.out")
Sys.sleep(1)

###################################
### Extract_Data_DRATES_PARAM.R ###
###################################
source("Extract_DRATES_PARAM.R")
#source("Graphics_DRATES_PARAM.R")
