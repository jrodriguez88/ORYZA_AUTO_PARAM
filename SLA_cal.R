##### LAI Analizer #####

LAI_df %>% select(-(`DVS       LAI`)) %>%
    ggplot(aes(DVS, LAI)) +
    geom_point()+#aes(color=LOC_ID))+
    geom_smooth(method = 'loess') +
    theme_bw() +
    facet_grid(.~LOC_ID)


### Calculate Canopy Cover (CC) 
LAI_df %>% select(-(`DVS       LAI`)) %>% na.omit() %>%
    mutate(
        k = case_when(
            DVS <= 0.65 ~ 0.4,
            DVS >= 1    ~ 0.6,
            TRUE        ~ 0.5),
        CC = 1 - exp(-k*LAI)) %>% 
    ggplot(aes(DVS, CC)) + geom_point() + geom_smooth(se=F) +
    facet_wrap(. ~ LOC_ID) +
    theme_bw() +
    labs(title = paste0("Canopy Cover (CC) - ", cultivar),
         x = "Development Stage (DVS)")

LAI <- LAI_df %>%
    left_join(BMASS_df) 

ggplot(LAI, aes(WLVG, LAI)) +
    geom_point()+
    geom_smooth(method = 'lm')

    lm(LAI~WLVG, data = LAI)%>%
    summary()
    
LAI %>%
    mutate(SLA=LAI/WLVG) %>%
    ggplot(aes(DVS, SLA, color=LOC_ID)) +
    geom_point()+
    geom_smooth(se=F)

#SLA_max <- LAI %>%
#    mutate(SLA=LAI/WLVG) %>%
#    filter(DVS<=0.3) %>%
#    summarise(SLA_max=quantile(SLA, 0.95))%>%
#    .$SLA_max
    
    

SLA <- LAI %>%
    mutate(SLA=LAI/WLVG) %>%
    filter(SLA<=0.004) %>%
    na.omit()

SLA_max <- SLA %>%
    filter(DVS<0.75) %>%
    lm(SLA~DVS, data = .) %>%
    summary() %>%
    .$coefficients %>% as.tibble() %>%
    mutate(par=Estimate+1.96*`Std. Error`) %>%
    .$par %>%
    .[1]
    


Loess_span25 <- loess(SLA~DVS, data = SLA, span = 0.25)
Loess_span50 <- loess(SLA~DVS, data = SLA, span = 0.50)
Loess_span75 <- loess(SLA~DVS, data = SLA, span = 0.75)
Loess_span90 <- loess(SLA~DVS, data = SLA, span = 0.90)

x <- c(seq(0, 0.8, 0.25), seq(1, 2, 0.5))
span25 <- predict(Loess_span25,newdata = x, se=T)
span50 <- predict(Loess_span50, newdata = x, se=T)
span75 <- predict(Loess_span75, newdata = x, se=T)
span90 <- predict(Loess_span90, newdata = x, se=T)

ggplot(SLA, aes( DVS, SLA))+# +"F2000 SLA Loess", xlab="Development Stage", ylab="Specific Leaf Area")
    geom_point(shape=1)+
    geom_smooth(se=F, color="black") +
    geom_line(data=data.frame(SLA=span25$fit, DVS=x), color="darkgreen", linetype="dashed")+
    geom_line(data=data.frame(SLA=span50$fit, DVS=x), color="red", linetype="dashed")+
    geom_line(data=data.frame(SLA=span75$fit, DVS=x), color="yellow", linetype="dashed")+
    geom_line(data=data.frame(SLA=span90$fit, DVS=x), color="blue", linetype="dashed")+
    theme_bw()




ggplot(SLA, aes( DVS, SLA))+# +"F2000 SLA Loess", xlab="Development Stage", ylab="Specific Leaf Area")
           geom_point(shape=1)+
    geom_smooth(se=F) +
    geom_line(data=data.frame(SLA=span50$fit+4*span50$se.fit, DVS=x), color="darkgreen", linetype="dashed")+
    geom_line(data=data.frame(SLA=span50$fit-4*span50$se.fit, DVS=x), color="red", linetype="dashed")+
    theme_bw()
    
           
           
#lines(span25$fit+4*span25$se.fit, x=x, col="red")
#lines(span50$fit+4*span50$se.fit, x=x, col="green")
#lines(span75$fit+4*span75$se.fit, x=x, col="blue")
#lines(span90$fit+4*span90$se.fit, x=x, col="brown")


SLA_df <- data.frame(DVS=round(x,2), SLA=round(span50$fit+4*span50$se.fit, 5))%>%
    mutate(SLA=replace_na(SLA, round(SLA_max, 5)),
           DVS=replace(DVS, 7, 2.5))


crp_tb <- function(tb, sig=5) {
   tb1 <-  cbind(
       paste0(sprintf("%.2f", tb[[1]]), ","),
       paste0(sprintf(paste0("%.",sig,"f"), tb[[2]]), 
              c(rep(",", nrow(tb)-1), "")))
   
   write.table(tb1 ,row.names = F, col.names = F, quote = F)
}
    
    
 cat(crp_tb(SLA_df))   
    
   

print(sla_crp, row.names = F, header=F)
    
write.table(SLA_df$DVS, quote=F, col.names = F, row.names = F, sep=',')

optimize(function(x, m) predict(m, x)^2, c(0, 1.9), Loess_span50)



#calcSSE <- function(x){
#    loessMod <- try(loess(SLA ~ DVS, data=SLA, span=x), silent=T)
#    res <- try(loessMod$residuals, silent=T)
#    if(class(res)!="try-error"){
#        if((sum(res, na.rm=T) > 0)){
#            sse <- sum(res^2)  
#        }
#    }else{
#        sse <- 99999
#    }
#    return(sse)
#}
#optim(par=c(0.5), calcSSE, method="SANN")
#    
#
#
#loess_optim <- loess(SLA ~ DVS, data = SLA, span = 0.04197)



