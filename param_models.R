### Script to apply Loess smooth method to get ORYZA CRP parameters


# Set input data

# Growth phases 
gstage <- data.frame(
    Growth_Phase=c("Vegetative", "Reproductive", "Ripening"),
    start=c(0, 0.65, 1),
    end=c(0.65,1,2)) %>% mutate(Growth_Phase=factor(Growth_Phase, c("Vegetative", "Reproductive", "Ripening")))


### According to ORYZA_V3_User_Manual_2014
### Apply filters by DVSM
PF_m1 <- BPART_df %>% 
    gather(key= "Partition_Parameter", value = "Value", -(1:6)) %>%
    mutate(
        #        Growth_phase = case_when(
        #            DVSM<=0.65 ~ "Vegetative",
        #            DVSM>0.65&DVSM<=1 ~"Reproductive",
        #            DVSM>1 ~"Ripening"),
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
            DVSM>1.1 &  Partition_Parameter == "FLV" ~ 0,
            DVSM>1.1 &  Partition_Parameter == "FST" ~ 0,
            DVSM>1.1 &  Partition_Parameter == "FSO" ~ 1,
            Value<0 ~ 0,
            Value>1 ~ 1,
            TRUE ~ Value))

### Method 2. Grafical analysis, logical-physiological filters
PF_m2 <- BPART_df %>%
    filter(FLV>=-0.2, FLV<=1.2,
           FST>=-0.2, FST<=1.2,
           FST>=-0.2, FSO<=1.2) %>%
    gather(key= "Partition_Parameter", value = "Value", -(1:6)) %>%
    mutate(
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
            Partition_Parameter == "FSO" & DVSM>1 & Value<0.30   ~ NA_real_,
            Partition_Parameter == "FLV" & DVSM>1 & Value>0.25   ~ NA_real_,
            Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
            Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
            Partition_Parameter == "FLV" & DVSM<1 & Value>0.75   ~ NA_real_,
            Partition_Parameter == "FST" & DVSM<0.75 & Value<0.4 ~ NA_real_,
            Partition_Parameter == "FST" & DVSM>1 & Value>0.5    ~ NA_real_,
            Partition_Parameter == "FST" & DVSM<1 & Value>0.80   ~ NA_real_,
            Value<0 ~ 0,
            Value>1 ~ 1,
            TRUE ~ Value))

### SLA_df
SLA_max <- function(SLA_df, fr=0.5) {
    SLA_df %>%
        filter(DVSM<fr) %>%
        lm(SLA~DVSM, data = .) %>%
        summary() %>%
        .$coefficients %>% as_tibble() %>%
        mutate(par=Estimate+1.96*`Std. Error`) %>%
        .$par %>%
        .[1]
}

SLA_df <- LAI_df %>%
    select(ID, LOC_ID, DVS, LAI) %>% na.omit() %>%
    left_join(BMASS_df) %>%
    mutate(SLA = LAI/WLVG, Value = SLA) %>%
    filter(SLA <= 0.0045) %>%
    dplyr::rename(DVSM=DVS)



# Function to get mean, minimun and maximun partition factor tables
Loess_crp <- function(data, DVS, span=0.5, nse=4) {
    
    #data convert to list
    if(any(str_detect(names(data), "SLA"))){
        SLA_max = SLA_max(data)
        data = list(SLA=data)
    } else {data=split(data, data$Partition_Parameter)}
    
    
    
    # Function to create crp tb
    crp_pf_tb <- function(data) {
        
        data %>% bind_rows(.id = "Partition_Parameter") %>%
            mutate(
                Value=case_when(
                    Partition_Parameter != "FSO" & DVS == 0 ~ 0.5,
                    Partition_Parameter == "FSO" & DVS > 1.5 ~ 1,
                    Partition_Parameter == "FSO" & DVS < 0.75 ~ 0,
                    Value<0 | is.na(Value) ~ 0,
                    TRUE ~ Value)) %>%
            spread(Partition_Parameter, Value) %>%
            mutate(PF_sum=FLV+FSO+FST,
                   PF_diff=1-PF_sum,
                   FLV = case_when(
                       DVS<1 ~ FLV+(PF_diff/2),
                       TRUE ~ FLV),
                   FST = case_when(
                       DVS<1 ~ FST+(PF_diff/2),
                       TRUE ~ FST),
                   FSO = case_when(
                       DVS>=1 ~ FSO+PF_diff,
                       TRUE ~ FSO),
                   PF_sum2= FLV+FSO+FST,
                   Test_log = (FLV+FSO+FST)==1) %>%
#            rename(DVSM=DVS) %>%
            select(DVS, FLV, FST, FSO) %>%
            gather(Partition_Parameter, Value, -1) %>%
            mutate(Partition_Parameter = factor(Partition_Parameter,
                                                c("FLV", "FST", "FSO"))) %>%
            select(Partition_Parameter, DVS, Value)
        
    }
    
    crp_sla_tb <- function(data, SLA_max) {
        
        data %>% bind_rows() %>%
            mutate(
                Value=case_when(
                    DVS == 0 ~ SLA_max,
                    Value < 0 | is.na(Value) ~ min(Value, na.rm = T),
                    TRUE ~ Value))
        
    }
    

    
    
    #Loess model by Partition factor    
    mod1 <- lapply(data, function(x){loess(Value~DVSM, data = x, span = span)}) %>%
        #predicted_list
        lapply(., function(x){predict(x, newdata = DVS, se=T)})
    
    #Cal mean
    PTB_crp_mean <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                        Value=x$fit)})
    #Cal min
    PTB_crp_min <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                       Value=x$fit+nse*x$se.fit)})
    #Cal_max
    PTB_crp_max <- lapply(mod1, function(x){data.frame(DVS=round(DVS,2), 
                                                       Value=x$fit-nse*x$se.fit)})
    
    
    if(any(str_detect(names(data), "SLA"))){
        crp_list <- list(
            mean = crp_sla_tb(PTB_crp_mean, SLA_max),
            min =  crp_sla_tb(PTB_crp_max, SLA_max),
            max =  crp_sla_tb(PTB_crp_min, SLA_max))
    } else {
        crp_list <- list(
            mean = crp_pf_tb(PTB_crp_mean),
            min = crp_pf_tb(PTB_crp_min),
            max = crp_pf_tb(PTB_crp_max))}
    
    
    return(crp_list)
    
}

