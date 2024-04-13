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
    sumDF2i <- summaryBy(leaf_p_pool~YearFactor+RingFactor+TrtFactor, FUN=c(mean,sd),
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
        geom_point(data=sumDF2i, aes(x=YearFactor, y=leaf_p_pool.mean, pch=TrtFactor), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
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
        scale_shape_manual(name="",
                           values=c("aCO2"=22,
                                    "eCO2"=24),
                           labels=c("aCO2"=expression(aCO[2]),
                                    "eCO2"=expression(eCO[2])))+
        guides(linetype=FALSE,color=FALSE)
    
    

    pdf("output/si_figures/si_figure4.pdf", width = 16, height = 8)
    plot_grid(p1, p2,
              ncol=1, labels="auto")
    
    dev.off()
    
    
}