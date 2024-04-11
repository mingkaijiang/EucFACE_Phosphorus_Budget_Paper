plot_leaflitter_p_pool_time_sequence <- function(leaflitter_p_pool) {
    
    ### asign CO2
    leaflitter_p_pool$Trt <- "aCO2"
    leaflitter_p_pool$Trt[leaflitter_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### add factors
    leaflitter_p_pool$TrtFactor <- as.factor(leaflitter_p_pool$Trt)
    leaflitter_p_pool$RingFactor <- as.factor(leaflitter_p_pool$Ring)
    leaflitter_p_pool$DateFactor <- as.factor(leaflitter_p_pool$Date)
    leaflitter_p_pool$YearFactor <- as.factor(year(leaflitter_p_pool$Date))
    leaflitter_p_pool$Year <- as.numeric(as.character(leaflitter_p_pool$YearFactor))
    
    
    ### summary 
    #sumDF <- summaryBy(leaflitter_p_pool~YearFactor+TrtFactor, FUN=c(mean,sd),
    #                   data=leaflitter_p_pool, na.rm=T, keep.names=T)
    
    
    ### statistics
    #mod.result <- lmer(leaflitter_p_pool~Year * TrtFactor + (1|RingFactor),data=leaflitter_p_pool)
    #mod.anova <- Anova(mod.result, test="F")
    
    mod.result <- lmer(leaflitter_p_pool~DateFactor * TrtFactor + (1|RingFactor),data=leaflitter_p_pool)
    mod.anova <- Anova(mod.result, test="F")
    
    #library(emmeans)
    #emmip(mod.result, YearFactor~TrtFactor)
    #emmeans(mod.result, pairwise~YearFactor:TrtFactor)
    
    ### plot
    p1 <- ggplot(sumDF, aes(x=YearFactor, y=leaflitter_p_pool.mean, group=TrtFactor))+
        geom_bar(aes(fill=TrtFactor), position="dodge", stat="identity", col="black")+
        geom_errorbar(aes(ymin=leaflitter_p_pool.mean-leaflitter_p_pool.sd, ymax=leaflitter_p_pool.mean+leaflitter_p_pool.sd),
                      position = position_dodge(0.9), width=0.4, size=0.4)+
        geom_point(leaflitter_p_pool, mapping=aes(x=YearFactor, y=leaflitter_p_pool, 
                                                  fill=TrtFactor, col=TrtFactor,
                                                  group=TrtFactor), pch=21)+
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
        labs(x="", y=expression("leaflitter P Pool (g P " * m^-2 * ")"))+
        scale_fill_manual(name="", values = c("aCO2" = Pastel1Palette[6], "eCO2" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    pdf("output/si_figures/leaflitter_p_pool_time_series.pdf", width = 6, height = 3)
    plot(p1)
    
    dev.off()
    
    
}