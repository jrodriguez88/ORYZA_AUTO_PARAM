#########################################################
#### Make Graphics from  DRATE.OUT  and param.out    ####
####     By https://github.com/jrodriguez88          ####
#########################################################


#### Load Requeriments

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


#################################################
#######   Development Rate of Crop (DVR)    #####
#################################################

######################################
############## BOXPLOT
######################################

DVR.m <- melt(DVR_df,id.vars=c(1:5) , measure.vars=c(6:9))
DVR_bp <- ggplot(data = DVR.m, aes(x=variable, y=value, fill=LOC_ID)) +
    theme_bw() +
    #    scale_fill_manual(values=c("gray95", "gray60")) +
    geom_boxplot(outlier.shape = 1) +    ##3text in Outlier
    geom_point(aes(colour=ID, shape=LOC_ID), position = position_jitter(width = 0.2), size=2.5) +
    #guides(colour=FALSE) +
    labs(title = paste0("Development Rates for ",DVR.m$CULTIVAR[1]) , y=bquote('DVR; ('*~degree*Cd^-1*')'), x="") 

png(filename=paste0(dirFol,"//_OutPut_Graphic//_1_Development_Rates_//", "DVR_boxplot", ".png",sep=""), width=10,height=6.25,units="in",res = 600)
print(DVR_bp)
dev.off()

DVR_vp <- ggplot(data = DVR.m, aes(x=variable, y=value, fill=LOC_ID)) +
    theme_bw() +
    #    scale_fill_manual(values=c("gray95", "gray60")) +
    geom_violin(scale = "width")  + 

    geom_point(aes(colour=ID, shape=LOC_ID), position = position_jitter(width = 0.2), size=2.5) +
    #guides(colour=FALSE) +
    labs(title = paste0("Development Rates for ",DVR.m$CULTIVAR[1]), y=bquote('DVR; ('*~degree*Cd^-1*')'), x="") 

png(filename=paste0(dirFol,"//_OutPut_Graphic//_1_Development_Rates_//", "DVR_violinplot", ".png",sep=""), width=10,height=6.25,units="in",res = 600)
print(DVR_vp)
dev.off()





######################################
############## VIOPLOT
######################################

DVRJ_vp <- ggplot(DVR_df, aes(x=as.factor(LOC_ID), y=DVRJ)) +
    theme_bw() +
    geom_violin(fill = "grey80", colour = "#3366FF", draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    geom_jitter(aes(colour=PROJECT, shape=TR_N), height = 0, width = 0.2, size=2.5) +
    guides(size=FALSE) +
    labs(title = "DVRJ - Development rate during \n Juvenile phase", y=bquote('DVR; ('*~degree*Cd^-1*')'), x="")

DVRP_vp <- ggplot(DVR_df, aes(x=as.factor(LOC_ID), y=DVRP)) +
    theme_bw() +
    geom_violin(fill = "grey80", colour = "#3366FF", draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    geom_jitter(aes(colour=PROJECT, shape=TR_N), height = 0, width = 0.2, size=2.5) +
    guides(size=FALSE) +
    labs(title = "DVRP - Development rate during \n Panicle development phase", y=bquote('DVR; ('*~degree*Cd^-1*')'), x="")

DVRR_vp <- ggplot(DVR_df, aes(x=as.factor(LOC_ID),y=DVRR)) +
    theme_bw() +
    geom_violin(fill = "grey80", colour = "#3366FF", draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    geom_jitter(aes(colour=PROJECT, shape=TR_N), height = 0, width = 0.2, size=2.5) +
    guides(size=FALSE) +
    labs(title = "DVRR - Development rate in reproductive phase \n (Post anthesis)", y=bquote('DVR; ('*~degree*Cd^-1*')'), x="")

DVR_viplot <- ggplot(data = DVR.m, aes(x=variable, y=value)) +
    theme_bw() +
    geom_violin(fill = "grey80", colour = "#3366FF", draw_quantiles = c(0.25, 0.5, 0.75), scale = "width")  + 
    #geom_point(aes(x=variable, y=mean(value)), colour="red")+
    labs(title = paste0("Development Rates for ",DVR.m$CULTIVAR[1]), y=bquote('DVR; ('*~degree*Cd^-1*')'), x="") 


png(filename=paste0(dirFol,"//_OutPut_Graphic//_1_Development_Rates_//", "DVR_vplots", ".png",sep=""), width=10,height=6.25,units="in",res = 600)
print(multiplot(DVR_viplot, DVRP_vp, DVRJ_vp, DVRR_vp,  cols=2))
dev.off()

#PHEN.m <- melt(PHEN_df,id.vars=c(1:5) , measure.vars=c(6:17))
#PHEN_bp <- ggplot(data = PHEN.m, aes(x=variable, y=value, fill=LOC_ID)) +
#    theme_bw() +
#    #    scale_fill_manual(values=c("gray95", "gray60")) +
#    geom_boxplot(outlier.shape = 1) +    ##3text in Outlier
#    geom_point(aes(colour=ID, shape=LOC_ID), position = position_jitter(width = 0.2), size=2.5) +
#    #guides(colour=FALSE) +
#    labs(title = "Development Rates", y=bquote('DVR; ('*~degree*Cd^-1*')'), x="") 

# png(filename=paste0(dirFol,"//_OutPut_Graphic//_1_Development_Rates_//", "PHEN_boxplot", ".png",sep=""), width=10,height=6.25,units="in",res = 600)
# print(PHEN_bp)
# dev.off()
# 
# 
# names(PHEN_df)
# 
# 
# 
# length(select_if(PHEN_df, is.numeric))


##############################


