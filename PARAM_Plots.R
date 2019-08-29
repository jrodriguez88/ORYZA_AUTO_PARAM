

### Set Cultivar Name
#cultivar <- "FED174"
#path <- getwd()
require(scales)

# DVR_plots
DVR_tidy <- DVR_df %>%
    gather(key = "DVR", value = "value", -c(1:5)) %>%
    filter(value<0.0040) %>% #DVR!= "DVRI", 
    mutate(DVR=factor(DVR, c("DVRJ", "DVRI", "DVRP", "DVRR")))

summary_dvr <- DVR_tidy %>%
    group_by(DVR) %>%
    summarise(n = n(),
              median=median(value), 
              mean=mean(value),
              sd=sd(value),
              min=min(value),
              max=max(value))
View(summary_dvr)

#Function to create Stat-Summary plot of development rates 
DVR_plot1 <- function(DVR_df, save_plot = "N") {
    
    plot <- DVR_df %>%
            gather(key = "DVR", value = "value", -c(1:5)) %>%
            filter(value<0.0040) %>% #DVR!= "DVRI", 
            mutate(DVR=factor(DVR, c("DVRJ", "DVRI", "DVRP", "DVRR"))) %>%
            ggplot(aes(x=DVR, y=value)) +
#           geom_boxplot(fill="gray") +
            geom_jitter(aes(shape=LOC_ID)) +
            stat_summary(fun.data = mean_sdl, color="red")+
            labs(x=NULL, title = paste0("Development Rates for ", cultivar),
                 y = bquote('DVR; ('*~degree*Cd^-1*')'))+
#           facet_grid(.~ DVR, scales="free") +
            theme_bw() + 
            scale_shape_discrete(name="Locality") 
#           theme(legend.position = "bottom", legend.title = element_blank()) +
        
        switch(save_plot,
               N = NULL, 
               Y = ggsave(plot, filename = paste0("DVR for ", cultivar, "_1.png"), width=7, height=3))
    
    plot
            
}


#Density plots by locality. No useful with small data
#DVR_tidy %>%  filter(DVR!= "DVRI") %>% 
#    ggplot(data = . , aes(y=DVR, x = value)) + #, fill=LOC_ID))
#    geom_joy()


DVR_plot2 <- function(DVR_df, save_plot = "N") {
    
    plot <- DVR_df %>%
        gather(key = "DVR", value = "value", -c(1:5)) %>%
        filter(value<0.0040) %>% #DVR!= "DVRI", 
        mutate(DVR=factor(DVR, c("DVRJ", "DVRI", "DVRP", "DVRR"))) %>%  
        filter(DVR!= "DVRI") %>% 
        ggplot(aes(value)) + #, fill=LOC_ID)) + 
        geom_density(aes(fill=DVR), alpha=.3, adjust = 1) +
        geom_rug(aes(x = value, y = 1, color=DVR), position = position_jitter(height = 0)) +
        labs(y=NULL, title = paste0("Density of Probability - Development Rates for ", cultivar))+
        facet_grid(.~ DVR) +
        theme_bw() + 
        theme(legend.position="none", axis.text.y = element_blank())
        
    
#    scale_x_continuous(labels = scientific) +
    

    switch(save_plot,
           N = NULL, 
           Y = ggsave(paste0("DVR for ", cultivar, "_2.png"), width=7, height=3.2))
    
    plot
    
}

# DVR Stat_Summary by locality. Plot saved in working directory    
DVR_plot3 <- function(DVR_df, save_plot = "N") {
    
    plot <- DVR_df %>%
        gather(key = "DVR", value = "value", -c(1:5)) %>%
        filter(value<0.0040) %>% #DVR!= "DVRI", 
        mutate(DVR=factor(DVR, c("DVRJ", "DVRI", "DVRP", "DVRR"))) %>%
        ggplot(aes(x=LOC_ID, y=value)) + 
        stat_summary(fun.data = mean_cl_boot, position = position_dodge(width=0.2))+
        labs(x="Locality",
             title = paste0("Development Rates for ", cultivar),
             y=bquote('DVR; ('*~degree*Cd^-1*')'))+
        facet_grid(.~ DVR, scales="free") +
        theme_bw() + 
        theme(legend.title = element_blank(), 
              strip.background = element_rect(fill="white"), 
              axis.text.x = element_text(angle=90, hjust = 1))
    

    

    switch(save_plot,
           N = NULL, 
           Y = ggsave(paste0("DVR for ", cultivar, "_3.png"), width=7, height=3))
    
    plot
    
}
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
BPART_plot1 <- function(data, save_plot = "N") {
    
    plot <-  data %>%
        ggplot(aes(x=DVSM, y=Value)) +
        geom_smooth(se=F, aes(color=Partition_Parameter), span=0.5) + 
        geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin=0, ymax=1) + 
        scale_fill_manual(values = alpha(c("chartreuse", "darkgreen", "orange1"), 0.2)) +
        labs(x="Development Stage (DVS)",
             title = paste0("Shoot dry matter partition - ", cultivar),
             y="Fraction") +
        scale_x_continuous(limits = c(-0.055, 2.055), expand = c(0, 0)) +
        scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
        geom_point(aes(color=Partition_Parameter, label=ID)) +
        theme_bw()
    
    switch(save_plot,
           N = NULL, 
           Y = ggsave(plot, filename = paste0("Partition Factors ", cultivar, "1.png"), width=7, height=4))
    
    plot
    
}


#ggplotly(PF_p2)

BPART_plot2 <- function(data, save_plot = "N") {
    
    plot <-  data %>%
        ggplot(aes(x=DVSM, y=Value, label=ID)) +
        geom_point(shape=1,aes(label=ID), fill="gray") +
        geom_smooth(se=F, linetype="twodash", col="red") + 
        facet_grid(Partition_Parameter ~ LOC_ID) + 
        labs(x="Development Stage (DVS)",
             title = paste0("Shoot dry matter partition by Locality - ", cultivar),
             y="Fraction") +
        theme_bw()
    
    switch(save_plot,
           N = NULL, 
           Y = ggsave(plot, filename = paste0("Partition Factors ", cultivar, "2.png"), width=7, height=5))
    
    plot
    
}
        

BPART_plot3 <- function(data, save_plot = "N") {
    
    plot <-  data %>%
        ggplot(aes(x=DVSM, y=Value)) +
        geom_smooth(se=F, aes(color=LOC_ID), span=0.5) + 
        geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin= -Inf, ymax=+Inf) + 
        scale_fill_manual(values = alpha(c("chartreuse", "darkgreen", "orange1"), 0.2)) + 
        geom_point(aes(shape=LOC_ID, label=ID), size=2) +
        scale_x_continuous(limits = c(-0.055, 2.055), expand = c(0, 0)) +
        scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
        facet_grid(Partition_Parameter ~.) + 
        labs(x="Development Stage (DVS)",
             title = paste0("Shoot dry matter partition - ", cultivar),
             y="Fraction") +
        theme_bw()
    
    switch(save_plot,
           N = NULL, 
           Y = ggsave(plot, filename = paste0("Partition Factors ", cultivar, "3.png"), width=7, height=5))
    
    plot
    
}






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


PF_m1 %>% bind_rows(PF_m2, .id = "PF_m") %>% 
    group_by(PF_m, Partition_Parameter) %>%
    dplyr::summarize(N_obs=n()) %>% View()

#DVS <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))


#
#pf_tbs <- Loess_crp(PF_m2, DVS=c(0,0.5,1,2.5))
#pf_tbs <- Loess_crp(PF_m2, DVS)



plot_pf_loess <- function(data_obs, data_sim, path, cultivar=cultivar, span=span, nse=nse, width=10, height=4) {
    
    plot <- data_obs %>% rename(DVS=DVSM) %>%
        #    mutate(Partition_Parameter=case_when(
        #        Partition_Parameter=="FLV" ~ "Leaves (FLV)",
        #        Partition_Parameter=="FST" ~ "Stems (FST)",
        #        Partition_Parameter=="FSO" ~ "Panicles (FSO)"
        #)) %>% 
        ggplot(aes(x=DVS, y=Value)) +
        geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), 
                  ymin= 0, ymax=1) + 
        geom_point(aes(shape=LOC_ID, label=ID), size=2) +
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
        scale_fill_manual(name="Growth phase",
                          values = alpha(c("chartreuse", "darkgreen", "orange1"), 0.2)) + 
        scale_shape_discrete(name="Locality") + 
        scale_color_manual(name="Smoth curve", values= c("darkgreen", "red")) +
        #    scale_fill_discrete(name="Growth phase") +
        facet_grid(. ~ Partition_Parameter)
    
        ggsave(plot, filename = paste0(path, "/Partition Factors for ", cultivar, ".png"),
               width=width, height=height)
        
        plot
    
    
}  

#plot_pf(PF_m2, pf_tbs, path, cultivar, span = 0.75, nse=4)
#
#plot_pf(data_obs = PF_m2, data_sim = pf_tbs, path = path, cultivar=cultivar, span = 0.75, nse=4, width = 10, height = 4)
#
#pf_tbs
plot_sla_loess <- function(data_obs, data_sim, path, cultivar, span, nse, width=10, height=4) {
    
    plot <- data_obs %>% rename(DVS=DVSM) %>%
        #    mutate(Partition_Parameter=case_when(
        #        Partition_Parameter=="FLV" ~ "Leaves (FLV)",
        #        Partition_Parameter=="FST" ~ "Stems (FST)",
        #        Partition_Parameter=="FSO" ~ "Panicles (FSO)"
        #)) %>% 
        ggplot(aes(x=DVS, y=Value)) +
        geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), 
                  ymin= 0, ymax=1) + 
        geom_point(aes(shape=LOC_ID, label=ID), size=2) +
        geom_line(data=data_sim$mean,
                  aes(color=paste0("Loess - span:", span)), size=1) + 
        #    geom_smooth(se=T, span=0.3) + 
        geom_line(data=data_sim$max, 
                  aes(color=paste0("SE*", nse)), linetype="twodash") +
        geom_line(data=data_sim$min, color="red", linetype="twodash") +
        theme_bw() +
        theme(strip.background = element_rect(fill="white")) +
        labs(x="Development Stage (DVS)",
             title = paste0("Specific Leaf Area (SLA) - ", cultivar),
             y="ha/kg") +
        scale_x_continuous(limits = c(-0.055, 2.055), expand = c(0, 0)) +
#        scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
        scale_fill_manual(name="Growth phase",
                          values = alpha(c("chartreuse", "darkgreen", "orange1"), 0.2)) + 
        scale_shape_discrete(name="Locality") + 
        scale_color_manual(name="Smoth curve", values= c("darkgreen", "red")) 
        #    scale_fill_discrete(name="Growth phase") +
#        facet_grid(. ~ Partition_Parameter)
    
    ggsave(plot, filename = paste0(path, "/SLA - ", cultivar, ".png"),
           width=width, height=height)
    
    plot
    
    
}

## Function to plot Fraction of carbohydrates allocated to stems that is stored as reserves
FSTR_plot <- function(FSTR_df, save_plot = "N") {
    
    plot <- FSTR_df %>% filter(FSTR>0) %>%
        ggplot(aes(LOC_ID, FSTR, label=ID)) +
        geom_jitter(width = 0.1) +
        stat_summary(fun.data = mean_se, color="red") +
        geom_hline(yintercept = mean(FSTR_df$FSTR), color="blue", linetype="twodash") +
        annotate("text", x = 0.65, y = mean(FSTR_df$FSTR), 
                 label =  paste0("mean =\n", round(mean(FSTR_df$FSTR),3))) + 
        labs(title = paste0("Fraction of carbohydrates allocated to stems (stored as reserves) - ", cultivar),
             x="Locality") +
        theme_bw()
    
    switch(save_plot,
        N = NULL, 
        Y = ggsave(plot, filename = paste0("FSTR for ", cultivar, ".png"), width=7, height=3))
    
    plot
    
} 

## Function to plot leaf death coefficient
DRLV_plot <- function(DRLV_df, save_plot = "N") {
    
    plot  <- DRLV_df %>% filter(DVSM>0.6) %>%
        ggplot(aes(DVSM, DRLV, label=ID)) +
        geom_point(aes(shape=LOC_ID)) + 
        geom_smooth(se=F, linetype="twodash", color="red") + 
        labs(title = paste0("Leaf Death Coefficient - ", cultivar),
             x = "Development Stage (DVS)") +
        theme_bw()
    
    
    
    switch(save_plot,
           N = NULL, 
           Y = ggsave(plot, filename = paste0("DRLV for ", cultivar, ".png"), width=7, height=3))
    
    plot
    
}

## Function to plot leaf area index and SLA


## Function to plot Spikelet growth factor
## Scatterplot data and lm coef
SPGF_plot <- function(SPGF_df, save_plot = "N") {
    
    lm_spgf <- lm(formula = SPIK_M2_max ~ diff_pi_fl, data = SPGF_df)
    
    plot <- ggplot(SPGF_df, aes(diff_pi_fl, SPIK_M2_max))+
        geom_point(aes(shape=LOC_ID, label=ID, color = LOC_ID))+
        geom_smooth(method = "lm", se = F, linetype="twodash")+
        theme_bw()+
        xlab("Growth between PI and flowering (g/m²)")+
        ylab("Spikelets/m²") +
        annotate("text", x=-Inf, y=c(max(SPGF_df$SPIK_M2_max), max(SPGF_df$SPIK_M2_max)*0.95, max(SPGF_df$SPIK_M2_max)*0.90, max(SPGF_df$SPIK_M2_max)*0.85) , hjust=-0.1,vjust=0, 
                 label=c(paste0(" y = ", round(summary(lm_spgf)$`coefficients`[2,1],2),"x"),
                         paste(" n = ", nrow(SPGF_df)),
                         paste(" r² = ", round(summary(lm_spgf)$`r.squared`,2)), 
                         paste("Pr(>|t|) =", round(summary(lm_spgf)$`coefficients`[2,4],4))))
    
    switch(save_plot,
           N = NULL, 
           Y = ggsave(plot, filename = paste0("SPGF for ", cultivar, ".png"), width=5, height=3))
    
    plot
    
}
    

