plot_ed_figure3 <- function(inDF,
                           ppool) {
    
    
    ### calculate original CP ratio under aCO2
    ### calculate original CP ratio under eCO2
    ### calculate new CP ratio under aCO2, this is the 10% scenario
    
    
    ### reshape
    reDF1 <- melt(setDT(inDF), id.vars = c("Ring"), variable.name = "Variable")
    
    ### add treatment
    reDF1$Trt <- "amb"
    reDF1$Trt[reDF1$Ring%in%c(1,4,5)] <- "ele"
    

    myDF <- reDF1
    myDF2 <- summaryBy(value~Variable+Trt, FUN=c(mean,sd),
                       data=myDF, na.rm=T, keep.names=T)
    
    ### ignore soil, litter
    plotDF1 <- myDF2[myDF2$Variable%in%c("canopy", "leaflitter",
                                         "fineroot", "understorey",
                                        "understorey_litter", "frass")]
    
    plotDF2 <- myDF2[myDF2$Variable%in%c("wood", "sapwood",
                                         "heartwood")]
    
    
    plotDF1i <- myDF[myDF$Variable%in%c("canopy", "leaflitter",
                                        "fineroot", "understorey",
                                        "understorey_litter", "frass")]
    
    plotDF2i <- myDF[myDF$Variable%in%c("wood", "sapwood",
                                        "heartwood")]
    
    # read in tmpDF
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("data/raw/belowground_P_working_sheet.csv")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    ### there are two NANs in the Cmic dataset, fill the gap
    ## multiple possible ways to fill the gap
    ## 1. taking the mean
    v <- mean(tmpDF$Cmic[tmpDF$Depth=="transition"], na.rm=T)
    #tmpDF$Cmic[tmpDF$Depth=="transition"&tmpDF$Ring%in%c(6,2)] <- v
    
    ## 2. calculating reduction coefficient
    v2 <- sum(tmpDF$Cmic[tmpDF$Ring%in%c(1,3,4,5)&tmpDF$Depth=="transition"])/sum(tmpDF$Cmic[tmpDF$Ring%in%c(1,3,4,5)&tmpDF$Depth=="10_30"])
    
    tmpDF$Cmic[tmpDF$Depth=="transition"&tmpDF$Ring%in%c(6,2)] <- v2 * tmpDF$Cmic[tmpDF$Depth=="10_30"&tmpDF$Ring%in%c(6,2)]
    
    
    ### calculate microbial CP ratio
    tmpDF$CPratio <- with(tmpDF, Cmic/Pmic)
    
    ### summary
    plotDF3 <- summaryBy(CPratio+Pmic+Cmic~Trt+Depth, data=tmpDF, FUN=c(mean,sd),
                      na.rm=T, keep.names=T)
    
    
    ###plot
    p1 <- ggplot(plotDF1, aes(x=Variable, y=value.mean, group=Trt))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge", color="black")+
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd, 
                          color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF1i, aes(x=Variable, y=value, pch=Trt), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
        labs(x="", y="CP ratio")+
        theme_linedraw() +
        #ylim(0,0.15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position=c(0.9, 0.8),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_discrete(limits=c("canopy", "leaflitter", "fineroot",
                                  "understorey", "understorey_litter", "frass"),
                         labels=c("canopy", "canopy leaf litter", "fineroot",
                                  "understorey", "understorey litter", "frass"))+
        scale_fill_manual(name="Treatment", values = c("amb" = Pastel1Palette[6], "ele" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="Treatment", values = c("amb" = "black", "ele" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_shape_manual(name="Treatment",
                           values=c("amb"=22,
                                    "ele"=24),
                           labels=c("amb"=expression(aCO[2]),
                                    "ele"=expression(eCO[2])))+
        guides(linetype=FALSE,color=FALSE)
    
    
    p2 <- ggplot(plotDF2, aes(x=Variable, y=value.mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge", color="black")+
        geom_errorbar(aes(ymax=value.mean+value.sd, ymin=value.mean-value.sd, 
                          color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF2i, aes(x=Variable, y=value, pch=Trt), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
        labs(x="", y="CP ratio")+
        theme_linedraw() +
        #ylim(0,0.15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("wood", "sapwood", "heartwood"),
                         labels=c("wood", "sapwood", "heartwood"))+
        scale_fill_manual(name="", values = c("amb" = Pastel1Palette[6], "ele" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("amb" = "black", "ele" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_shape_manual(name="Treatment",
                           values=c("amb"=22,
                                    "ele"=24),
                           labels=c("amb"=expression(aCO[2]),
                                    "ele"=expression(eCO[2])))
    
    
    p3 <- ggplot(plotDF3, aes(x=Depth, y=CPratio.mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge", color="black")+
        geom_errorbar(aes(ymax=CPratio.mean+CPratio.sd, ymin=CPratio.mean-CPratio.sd, 
                          color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=tmpDF, aes(x=Depth, y=CPratio, pch=Trt), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
        labs(x="", y="CP ratio")+
        theme_linedraw() +
        #ylim(0,0.15)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete(limits=c("0_10", "10_30", "transition"),
                         labels=c("microbe 0-10cm", "microbe 10-30cm",
                                  "microbe 30-60cm"))+
        scale_fill_manual(name="", values = c("Ambient" = Pastel1Palette[6], "Elevated" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("Ambient" = "black", "Elevated" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_shape_manual(name="Treatment",
                           values=c("Ambient"=22,
                                    "Elevated"=24),
                           labels=c("Ambient"=expression(aCO[2]),
                                    "Elevated"=expression(eCO[2])))
    
    
    
   
    grid.labs <- c("(a)", "(b)", "(c)")
    
    
    pdf(paste0("output/si_figures/ed_figure3.pdf"),
        width=10,height=6)
    bot_row <- plot_grid(p2, p3, ncol=2, rel_widths = c(1., 1.))
    plot_grid(p1, bot_row,  ncol = 1,
              rel_heights=c(1, 1))
    grid.text(grid.labs,
              x = c(0.1, 0.1, 0.6), y = c(0.95, 0.45, 0.45),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
 }

