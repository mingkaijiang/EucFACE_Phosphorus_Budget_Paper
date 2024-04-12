plot_microbial_p_pool_time_sequence <- function(microbial_p_pool) {
    
    ### asign CO2
    microbial_p_pool$Trt <- "aCO2"
    microbial_p_pool$Trt[microbial_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### add factors
    microbial_p_pool$TrtFactor <- as.factor(microbial_p_pool$Trt)
    microbial_p_pool$RingFactor <- as.factor(microbial_p_pool$Ring)
    microbial_p_pool$DateFactor <- as.factor(microbial_p_pool$Date)
    microbial_p_pool$YearFactor <- as.factor(year(microbial_p_pool$Date))
    
    microbial_p_pool <- microbial_p_pool[microbial_p_pool$Depth=="0_10",]
    
    
    ### summary 
    sumDF <- summaryBy(microbial_p_g_m2~YearFactor+TrtFactor, FUN=c(mean,sd),
                       data=microbial_p_pool, na.rm=T, keep.names=T)
    
    
    ### statistics
    mod.result <- lmer(microbial_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=microbial_p_pool)
    mod.anova <- Anova(mod.result, test="F")
    
    #library(emmeans)
    #emmip(mod.result, YearFactor~TrtFactor)
    #emmeans(mod.result, pairwise~YearFactor:TrtFactor)
    
    ### plot
    p1 <- ggplot(sumDF, aes(x=YearFactor, y=microbial_p_g_m2.mean, group=TrtFactor))+
        geom_bar(aes(fill=TrtFactor), position="dodge", stat="identity", col="black")+
        geom_errorbar(aes(ymin=microbial_p_g_m2.mean-microbial_p_g_m2.sd, ymax=microbial_p_g_m2.mean+microbial_p_g_m2.sd),
                      position = position_dodge(0.9), width=0.4, size=0.4)+
        geom_point(microbial_p_pool, mapping=aes(x=YearFactor, y=microbial_p_g_m2, group=TrtFactor, fill=TrtFactor))+
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
        labs(x="", y=expression("Microbial P Pool (g P " * m^-2 * ")"))+
        scale_fill_manual(name="", values = c("aCO2" = Pastel1Palette[6], "eCO2" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    #pdf("output/si_figures/microbial_p_pool_time_series.pdf", width = 6, height = 3)
    #plot(p1)
    
    #dev.off()
    
    
}