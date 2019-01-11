#DVR_cal
options()

### Set Cultivar Name
cultivar <- "F2000"


##Summary of development rates, 0.0040 maximun value
DVR_df %>%
    gather(key = "DVR", value = "value", -c(1:5)) %>%
    filter(value<0.0040) %>%
    group_by(DVR) %>%
    summarise(median=median(value), 
              mean=mean(value),
              sd=sd(value),
              min=min(value),
              max=max(value))


#Boxplot of development rates 
DVR_df %>%
    gather(key = "DVR", value = "value", -c(1:5)) %>%
    filter(DVR!= "DVRI", value<0.0040) %>%
    ggplot(aes(x=DVR, y=value))+geom_boxplot(fill="gray") +
    geom_jitter(aes(color=LOC_ID))+
    labs(x=NULL, title = paste0("Development Rates for ", cultivar) , y=bquote('DVR; ('*~degree*Cd^-1*')'))+
#    facet_grid(.~ DVR, scales="free") +
    theme_bw() + 
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggsave(paste0("DVR for ", cultivar, "2.png"), width=5, height=3)


#Density plots by locality. No useful with small data
DVR_df %>%
    gather(key = "DVR", value = "value", -c(1:5)) %>%
    filter(DVR!= "DVRI", value<0.0040) %>%   
    ggplot(aes(x=value, fill=LOC_ID)) + 
    geom_density(alpha=.3) +
    labs(y=NULL, title = paste0("Development Rates for ", cultivar) , x=bquote('DVR; ('*~degree*Cd^-1*')'))+
    facet_grid(.~ DVR, scales="free") +
    theme_bw() + 
    theme(legend.position = "bottom", legend.title = element_blank())

# DVR Stat_Summary by locality. Plot saved in working directory
DVR_df %>%
    gather(key = "DVR", value = "value", -c(1:5)) %>%
    filter(DVR!= "DVRI", value<0.0040) %>%
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
    ggsave(paste0("DVR for ", cultivar, ".png"), width=6, height=3)




### According to ORYZA_V3_User_Manual_2014
### Apply filters by DVSM

PF_m1 <- BPART_df %>% 
    gather(key= "Partition_Parameter", value = "Value", -(1:6)) %>%
    mutate(
        Growth_phase = case_when(
            DVSM<=0.65 ~ "Vegetative",
            DVSM>0.65&DVSM<=1 ~"Reproductive",
            DVSM>1 ~"Ripening"),
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
            DVSM>1.1 &  Partition_Parameter == "FLV" ~ 0,
            DVSM>1.1 &  Partition_Parameter == "FST" ~ 0,
            DVSM>1.1 &  Partition_Parameter == "FSO" ~ 1,
            Value<0 ~ 0,
            Value>1 ~ 1,
            TRUE ~ Value))

gstage <- data.frame(
    Growth_Phase=c("Vegetative", "Reproductive", "Ripening"),
    start=c(0, 0.65, 1),
    end=c(0.65,1,2)) %>% mutate(Growth_Phase=factor(Growth_Phase, c("Vegetative", "Reproductive", "Ripening")))

PF_p1 <- PF_m1 %>%
    ggplot(aes(x=DVSM, y=Value)) +
    geom_smooth(se=F, aes(color=Partition_Parameter), span=0.5) + 
    geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin=0, ymax=1) + 
    scale_fill_manual(values = alpha(c("darkgreen", "red", "orange1"), 0.2)) + 
    geom_point(aes(color=Partition_Parameter, label=ID)) +
    theme_bw()

library(plotly)
ggplotly(PF_p1)


PF_p2 <- PF_m1 %>%
    ggplot(aes(x=DVSM, y=Value, label=ID)) +
    geom_point(shape=1,aes(label=ID)) +
    geom_smooth(se=F, linetype="twodash", col="red", span=0.75) + 
    facet_grid(Partition_Parameter ~ LOC_ID) + 
    theme_bw()


ggplotly(PF_p2)

PF_m1 %>%
    ggplot(aes(x=DVSM, y=Value)) +
    geom_smooth(se=F, aes(color=LOC_ID), span=0.5) + 
    geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin= -Inf, ymax=+Inf) + 
    scale_fill_manual(values = alpha(c("darkgreen", "red", "orange1"), 0.2)) + 
    geom_point(aes(shape=LOC_ID, label=ID), size=2) +
    facet_grid(Partition_Parameter ~.) + 
    theme_bw()



### Method 2. Grafical analysis, logical-physiological filters
PF_m2 <- BPART_df %>%
    filter(FLV>=-0.2, FLV<=1.2,
           FST>=-0.2, FST<=1.2,
           FST>=-0.2, FSO<=1.2) %>%
    gather(key= "Partition_Parameter", value = "Value", -(1:6)) %>%
    mutate(
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
            DVSM>1.25 &  Partition_Parameter == "FLV" ~ 0,
            DVSM>1.25 &  Partition_Parameter == "FST" ~ 0,
            DVSM>1.25 &  Partition_Parameter == "FSO" ~ 1,
            Value<0 ~ 0,
            Value>1 ~ 1,
            TRUE ~ Value))

x <- c(seq(0, 0.8, 0.25), seq(1, 2.5, 0.5))
Loess_crp <- function(data, DVS=x, span=0.5)
Loess_model <- loess(Value~DVSM, data = PF_m2%>%filter(Partition_Parameter=="FLV"), span = span)

mod1 <- predict(Loess_model, newdata = x, se=T)

FLV_crp <- data.frame(DVS=round(x,2), Value=round(mod1$fit, 3))%>%
    mutate(
        Value=case_when(
            DVS==0 ~ 0.5,
            Value<0 | is.na(Value) ~ 0,
            TRUE ~ Value))
        



PF_m2 %>% filter(Partition_Parameter=="FLV") %>%
    ggplot(aes(x=DVSM, y=Value)) +
    geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin= -Inf, ymax=+Inf) + 
    scale_fill_manual(values = alpha(c("darkgreen", "red", "orange1"), 0.2)) + 
    geom_point(aes(shape=LOC_ID), size=2) +
    geom_smooth(se=T, span=0.5) + 
    geom_line(data=data.frame(Value=mod1$fit+4*mod1$se.fit, DVSM=x), color="darkgreen", linetype="dashed") +
    geom_line(data=data.frame(Value=mod1$fit-4*mod1$se.fit, DVSM=x), color="red", linetype="dashed") +
    facet_grid(Partition_Parameter ~.) + 
    theme_bw()



