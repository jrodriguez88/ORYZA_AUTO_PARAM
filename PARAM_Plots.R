

### Set Cultivar Name
#cultivar <- "FED174"
#path <- getwd()

#DVR_cal
DVR_tidy <- DVR_df %>%
    gather(key = "DVR", value = "value", -c(1:5)) %>%
    filter(value<0.0040) %>% #DVR!= "DVRI", 
    mutate(DVR=factor(DVR, c("DVRJ", "DVRI", "DVRP", "DVRR")))

##Summary of development rates, 0.0040 maximun value
DVR_tidy %>%
    group_by(DVR) %>%
        summarise(median=median(value), 
              mean=mean(value),
              sd=sd(value),
              min=min(value),
              max=max(value))


#Boxplot of development rates 
DVR_tidy %>% 
    ggplot(aes(x=DVR, y=value)) +
#    geom_boxplot(fill="gray") +
    geom_jitter(aes(shape=LOC_ID)) +
    stat_summary(fun.data = mean_sdl, color="red")+
    labs(x=NULL, title = paste0("Development Rates for ", cultivar) , y=bquote('DVR; ('*~degree*Cd^-1*')'))+
#    facet_grid(.~ DVR, scales="free") +
    theme_bw() + 
    scale_shape_discrete(name="Locality") + 
#    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggsave(paste0("DVR for ", cultivar, "_1.png"), width=5, height=3)


#Density plots by locality. No useful with small data
#DVR_tidy %>%  filter(DVR!= "DVRI") %>% 
#    ggplot(data = . , aes(y=DVR, x = value)) + #, fill=LOC_ID))
#    geom_joy()


DVR_tidy %>%  filter(DVR!= "DVRI") %>% 
    ggplot(aes(value)) + #, fill=LOC_ID)) + 
    geom_density(aes(fill=DVR), alpha=.3, adjust = 1) +
    geom_rug(aes(x = value, y = 0, color=DVR), position = position_jitter(height = 0)) +
    labs(y=NULL, title = paste0("Development Rates for ", cultivar) , x=bquote('DVR; ('*~degree*Cd^-1*')'))+
#    facet_grid(.~ DVR) +
    theme_bw() + 
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    ggsave(paste0("DVR for ", cultivar, "_3.png"), width=5, height=3)

# DVR Stat_Summary by locality. Plot saved in working directory
DVR_tidy %>%
    ggplot(aes(x=LOC_ID, y=value)) + 
    stat_summary(fun.data = mean_cl_boot, position = position_dodge(width=0.2))+
    labs(x="Locality",
         title = paste0("Development Rates for ", cultivar),
         y=bquote('DVR; ('*~degree*Cd^-1*')'))+
    facet_grid(.~ DVR, scales="free") +
    theme_bw() + 
    theme(legend.title = element_blank(), 
          strip.background = element_rect(fill="white"), 
          axis.text.x = element_text(angle=90, hjust = 1)) +
    ggsave(paste0("DVR for ", cultivar, "_2.png"), width=6, height=3)


### According to ORYZA_V3_User_Manual_2014
### Apply filters by DVSM

#PF_m1 <- BPART_df %>% 
#    gather(key= "Partition_Parameter", value = "Value", -(1:6)) %>%
#    mutate(
##        Growth_phase = case_when(
##            DVSM<=0.65 ~ "Vegetative",
##            DVSM>0.65&DVSM<=1 ~"Reproductive",
##            DVSM>1 ~"Ripening"),
#        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
#        Value = case_when(
#            DVSM>1.1 &  Partition_Parameter == "FLV" ~ 0,
#            DVSM>1.1 &  Partition_Parameter == "FST" ~ 0,
#            DVSM>1.1 &  Partition_Parameter == "FSO" ~ 1,
#            Value<0 ~ 0,
#            Value>1 ~ 1,
#            TRUE ~ Value))
#
#gstage <- data.frame(
#    Growth_Phase=c("Vegetative", "Reproductive", "Ripening"),
#    start=c(0, 0.65, 1),
#    end=c(0.65,1,2)) %>% mutate(Growth_Phase=factor(Growth_Phase, c("Vegetative", "Reproductive", "Ripening")))

PF_p1 <- PF_m1 %>%
    ggplot(aes(x=DVSM, y=Value)) +
    geom_smooth(se=F, aes(color=Partition_Parameter), span=0.5) + 
    geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin=0, ymax=1) + 
    scale_fill_manual(values = alpha(c("darkgreen", "red", "orange1"), 0.2)) + 
    geom_point(aes(color=Partition_Parameter, label=ID)) +
    theme_bw()


#ggplotly(PF_p2)


PF_p2 <- PF_m1 %>%
    ggplot(aes(x=DVSM, y=Value, label=ID)) +
    geom_point(shape=1,aes(label=ID)) +
    geom_smooth(se=F, linetype="twodash", col="red", span=0.75) + 
    facet_grid(Partition_Parameter ~ LOC_ID) + 
    theme_bw()




PF_p3 <- PF_m1 %>%
    ggplot(aes(x=DVSM, y=Value)) +
    geom_smooth(se=F, aes(color=LOC_ID), span=0.5) + 
    geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin= -Inf, ymax=+Inf) + 
    scale_fill_manual(values = alpha(c("darkgreen", "red", "orange1"), 0.2)) + 
    geom_point(aes(shape=LOC_ID, label=ID), size=2) +
    facet_grid(Partition_Parameter ~.) + 
    theme_bw()



#### Method 2. Grafical analysis, logical-physiological filters
#PF_m2 <- BPART_df %>%
#    filter(FLV>=-0.2, FLV<=1.2,
#           FST>=-0.2, FST<=1.2,
#           FST>=-0.2, FSO<=1.2) %>%
#    gather(key= "Partition_Parameter", value = "Value", -(1:6)) %>%
#    mutate(
#        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
#        Value = case_when(
#            Partition_Parameter == "FSO" & DVSM>1 & Value<0.30   ~ NA_real_,
#            Partition_Parameter == "FLV" & DVSM>1 & Value>0.25   ~ NA_real_,
#            Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
#            Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
#            Partition_Parameter == "FLV" & DVSM<1 & Value>0.75   ~ NA_real_,
#            Partition_Parameter == "FST" & DVSM<0.75 & Value<0.4 ~ NA_real_,
#            Partition_Parameter == "FST" & DVSM>1 & Value>0.5    ~ NA_real_,
#            Partition_Parameter == "FST" & DVSM<1 & Value>0.80   ~ NA_real_,
#            Value<0 ~ 0,
#            Value>1 ~ 1,
#            TRUE ~ Value))

PF_m2 %>%
    group_by(ID, Partition_Parameter) %>%
    summarize(n()) %>% View()
PF_m2 %>%
    group_by(LOC_ID, Partition_Parameter) %>%
    summarize(n()) %>% View()

#DVS <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))


#
#pf_tbs <- Loess_crp(PF_m2, DVS=c(0,0.5,1,2.5))
#pf_tbs <- Loess_crp(PF_m2, DVS)



plot_pf <- function(data_obs, data_sim, path, cultivar=cultivar, span=span, nse=nse, width=10, height=4) {
    
    plot <- data_obs %>% 
        #    mutate(Partition_Parameter=case_when(
        #        Partition_Parameter=="FLV" ~ "Leaves (FLV)",
        #        Partition_Parameter=="FST" ~ "Stems (FST)",
        #        Partition_Parameter=="FSO" ~ "Panicles (FSO)"
        #)) %>% 
        ggplot(aes(x=DVSM, y=Value)) +
        geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), 
                  ymin= 0, ymax=1) + 
        geom_point(aes(shape=LOC_ID), size=2) +
        geom_line(data=data_sim$mean,
                  aes(color=paste0("Loess - span:", span)), size=1) + 
        #    geom_smooth(se=T, span=0.3) + 
        geom_line(data=data_sim$max, 
                  aes(color=paste0("SE*", nse)), linetype="twodash") +
        geom_line(data=data_sim$min, color="red", linetype="twodash") +
        theme_bw() +
        theme(strip.background = element_rect(fill="white")) +
        labs(x="Development Stage (DVS)",
             title = paste0("Shoot dry matter partition - ", cultivar),
             y="Fraction") +
        scale_x_continuous(limits = c(-0.055, 2.055), expand = c(0, 0)) +
        scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
        scale_fill_manual(name="Growth phase",values = alpha(c("chartreuse", "darkgreen", "orange1"), 0.2)) + 
        scale_shape_discrete(name="Locality") + 
        scale_color_manual(name="Smoth curve", values= c("darkgreen", "red")) +
        #    scale_fill_discrete(name="Growth phase") +
        facet_grid(. ~ Partition_Parameter)
    
        ggsave(plot, filename = paste0(path, "/Partition Factors for ", cultivar, ".png"), width=width, height=height)
        
        plot
    
    
}  


#plot_pf(PF_m2, pf_tbs, path, cultivar, span = 0.75, nse=4)
#
#plot_pf(data_obs = PF_m2, data_sim = pf_tbs, path = path, cultivar=cultivar, span = 0.75, nse=4, width = 10, height = 4)
#
#pf_tbs

FSTR_df %>% filter(FSTR>0) %>%
    ggplot(aes(LOC_ID, FSTR)) +
    geom_jitter(width = 0.1)+
    stat_summary(fun.data = mean_se, color="red")+
    theme_bw()