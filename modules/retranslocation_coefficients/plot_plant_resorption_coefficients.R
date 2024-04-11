plot_plant_resorption_coefficients <- function(plant_p_retranslocation_coefficients) {
    
    outDF <- plant_p_retranslocation_coefficients 
    
    
    plotDF <- melt(outDF, id.vars = c("Ring"), variable.name = "component")
    
    plotDF$Ring <- as.character(plotDF$Ring)
    plotDF$Trt[plotDF$Ring%in%c("1","4","5")] <- "ele"
    plotDF$Trt[plotDF$Ring%in%c("2","3","6")] <- "amb"
    
    plotDF2 <- summaryBy(value~Trt+component, FUN=c(mean,sd),
                         data=plotDF,
                         na.rm=T, keep.names=T)
    
    plotDF2 <- plotDF2[plotDF2$component%in%c("canopy", "understorey", "sapwood"),]
    
    #p1 <- ggplot(plotDF, aes(x=component, y=value, group=Ring))+
    #    geom_bar(aes(fill=Ring), position="dodge", stat="identity") 
    
    p2 <- ggplot(plotDF2, aes(x=component, y=value.mean, group=Trt))+
        geom_bar(aes(fill=Trt), position="dodge", stat="identity", col="black")+
        geom_errorbar(aes(ymin=value.mean-value.sd, ymax=value.mean+value.sd),
                      position = position_dodge(0.9), width=0.4, size=0.4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        labs(x="", y="Resorption coefficient")+
        scale_fill_manual(name="", values = c("amb" = Pastel1Palette[6], "ele" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    
    
    pdf("output/si_figures/ed_figure6.pdf", height=4,width=6)
    plot(p2)
    dev.off()
}