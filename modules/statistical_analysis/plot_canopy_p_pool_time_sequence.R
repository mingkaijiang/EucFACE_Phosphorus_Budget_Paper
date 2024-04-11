plot_canopy_p_pool_time_sequence <- function(canopy_p_pool) {
    
    ### asign CO2
    canopy_p_pool$Trt <- "aCO2"
    canopy_p_pool$Trt[canopy_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### add factors
    canopy_p_pool$TrtFactor <- as.factor(canopy_p_pool$Trt)
    canopy_p_pool$RingFactor <- as.factor(canopy_p_pool$Ring)
    canopy_p_pool$DateFactor <- as.factor(canopy_p_pool$Date)
    canopy_p_pool$YearFactor <- as.factor(year(canopy_p_pool$Date))
    canopy_p_pool$Year <- year(canopy_p_pool$Date)
    
    canopy_p_pool <- subset(canopy_p_pool, Year>2012)
    
    ### summary 
    sumDF1 <- summaryBy(leaf_p_pool~DateFactor+TrtFactor+Date, FUN=c(mean,sd),
                       data=canopy_p_pool, na.rm=T, keep.names=T)
    sumDF2 <- summaryBy(leaf_p_pool~YearFactor+TrtFactor, FUN=c(mean,sd),
                        data=canopy_p_pool, na.rm=T, keep.names=T)
    
    ### statistics
    #mod.result <- lmer(leaf_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=canopy_p_pool)
    #mod.anova <- Anova(mod.result, test="F")
    
    
    #sumDF <- summaryBy(leaf_p_pool~DateFactor+TrtFactor+Date+RingFactor, FUN=c(mean,sd),
    #                   data=canopy_p_pool, na.rm=T, keep.names=T)
    
    mod.result <- lmer(leaf_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=canopy_p_pool)
    mod.anova <- Anova(mod.result, test="F")
    
    library(emmeans)
    emmip(mod.result, YearFactor~TrtFactor)
    sumDF3 <- as.data.frame(emmeans(mod.result, pairwise~YearFactor:TrtFactor)$emmeans)
    
    
    ### prppare plot
    sumDF1$Date <- as.Date(sumDF1$Date)
    
    ### plot
    p1 <- ggplot(sumDF1, aes(x=Date, y=leaf_p_pool.mean, group=TrtFactor))+
        geom_ribbon(aes(x=Date, y=leaf_p_pool.mean, ymin=leaf_p_pool.mean-leaf_p_pool.sd,
                        ymax=leaf_p_pool.mean+leaf_p_pool.sd,
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
        labs(x="", y=expression("Canopy P Pool (g P " * m^-2 * ")"))+
        scale_fill_manual(name="", values = c("aCO2" = Pastel1Palette[6], "eCO2" = Pastel1Palette[8]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_color_manual(name="", values = c("aCO2" = Pastel1Palette[2], "eCO2" = Pastel1Palette[1]),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_date(date_breaks="6 month", date_labels = "%Y-%B")
    
    
    p2 <- ggplot(sumDF2, aes(x=YearFactor, y=leaf_p_pool.mean, group=TrtFactor))+
        geom_bar(aes(fill=TrtFactor), position="dodge", stat="identity", col="black")+
        geom_errorbar(aes(ymin=leaf_p_pool.mean-leaf_p_pool.sd, ymax=leaf_p_pool.mean+leaf_p_pool.sd),
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
        labs(x="", y=expression("Canopy P Pool (g P " * m^-2 * ")"))+
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

    pdf("output/si_figures/canopy_p_pool_time_series.pdf", width = 16, height = 8)
    plot_grid(p1, p2,
              ncol=1, labels="auto")
    
    dev.off()
    
    
}