plot_fineroot_p_concentration_time_sequence <- function(fineroot_p_concentration) {
    
    ### asign CO2
    fineroot_p_concentration$Trt <- "aCO2"
    fineroot_p_concentration$Trt[fineroot_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### add factors
    fineroot_p_concentration$TrtFactor <- as.factor(fineroot_p_concentration$Trt)
    fineroot_p_concentration$RingFactor <- as.factor(fineroot_p_concentration$Ring)
    fineroot_p_concentration$DateFactor <- as.factor(fineroot_p_concentration$Date)
    fineroot_p_concentration$YearFactor <- as.factor(year(fineroot_p_concentration$Date))
    fineroot_p_concentration$Year <- year(fineroot_p_concentration$Date)
    

    ### summary 
    sumDF1 <- summaryBy(PercP~DateFactor+TrtFactor+Date, FUN=c(mean,sd),
                       data=fineroot_p_concentration, na.rm=T, keep.names=T)
    sumDF2 <- summaryBy(PercP~YearFactor+TrtFactor, FUN=c(mean,sd),
                        data=fineroot_p_concentration, na.rm=T, keep.names=T)
    
    ### statistics
    #mod.result <- lmer(leaf_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=fineroot_p_concentration)
    #mod.anova <- Anova(mod.result, test="F")
    
    
    #sumDF <- summaryBy(leaf_p_pool~DateFactor+TrtFactor+Date+RingFactor, FUN=c(mean,sd),
    #                   data=fineroot_p_concentration, na.rm=T, keep.names=T)
    
    mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=fineroot_p_concentration)
    mod.anova <- Anova(mod.result, test="F")
    
    library(emmeans)
    emmip(mod.result, YearFactor~TrtFactor)
    sumDF3 <- as.data.frame(emmeans(mod.result, pairwise~YearFactor:TrtFactor)$emmeans)
    
    
    ### prppare plot
    sumDF1$Date <- as.Date(sumDF1$Date)
    
    ### plot
    p1 <- ggplot(sumDF1, aes(x=Date, y=PercP.mean, group=TrtFactor))+
        geom_ribbon(aes(x=Date, y=PercP.mean, ymin=PercP.mean-PercP.sd,
                        ymax=PercP.mean+PercP.sd,
                        fill=TrtFactor))+
        geom_point(aes(color=TrtFactor), pch=19)+
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
        labs(x="", y=expression("Fineroot P concentration (%)"))+
        scale_fill_manual(name="", values = c("aCO2" = Pastel1Palette[6], "eCO2" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_color_manual(name="", values = c("aCO2" = Pastel1Palette[2], "eCO2" = Pastel1Palette[1]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_date(date_breaks="6 month", date_labels = "%Y-%B")
    
    
    p2 <- ggplot(sumDF2, aes(x=YearFactor, y=PercP.mean, group=TrtFactor))+
        geom_bar(aes(fill=TrtFactor), position="dodge", stat="identity", col="black")+
        geom_errorbar(aes(ymin=PercP.mean-PercP.sd, ymax=PercP.mean+PercP.sd),
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
        labs(x="", y=expression("Fineroot P concentration (%)"))+
        scale_fill_manual(name="", values = c("aCO2" = Pastel1Palette[6], "eCO2" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))
    
    #p3 <- ggplot(sumDF3, aes(x=TrtFactor, y=emmean, group=YearFactor))+
    #    geom_errorbar(aes(x=TrtFactor, ymin=lower.CL, ymax=upper.CL),
    #                  position="dodge", stat="identity")+
    #    geom_line(aes(col=YearFactor))+
    #    geom_point(aes(fill=YearFactor), position="dodge", stat="identity", pch=21, col="black")+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=10), 
    #          axis.text.x = element_text(size=10),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="right")+
    #    labs(x="", y=expression("Canopy P Pool (g P " * m^-2 * ")"))#+
    #    #scale_fill_manual(name="", values = c("aCO2" = Pastel1Palette[6], "eCO2" = Pastel1Palette[8]),
    #    #                  labels=c(expression(aCO[2]), expression(eCO[2])))
    #
    #plot(p3)

    pdf("output/si_figures/fineroot_p_concentration_time_series.pdf", width = 12, height = 8)
    plot_grid(p1, p2,
              ncol=1, labels="auto")
    
    dev.off()
    
    
}