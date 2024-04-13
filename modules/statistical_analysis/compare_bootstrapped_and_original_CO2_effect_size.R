compare_bootstrapped_and_original_CO2_effect_size <- function(summary_table_concentration,
                                                              summary_table_concentration_bootstrap,
                                                              summary_table_pool,
                                                              summary_table_pool_bootstrap,
                                                              summary_table_flux,
                                                              summary_table_flux_bootstrap) {
    
    
    
    ### subset
    myDF1 <- summary_table_concentration[,c("conc.terms", "aCO2", "eCO2", "diff", "aCO2_sd", "eCO2_sd", "diff_se", "diff_cf")]
    myDF2 <- summary_table_pool[,c("terms", "aCO2", "eCO2", "diff", "aCO2_sd", "eCO2_sd", "diff_se", "diff_cf")]
    myDF3 <- summary_table_flux[,c("terms", "aCO2", "eCO2", "diff", "aCO2_sd", "eCO2_sd", "diff_se", "diff_cf")]

    ### recalculate 95, 85 and 75% CIs for original data
    myDF1$diff_cf_90 <- with(myDF1, diff_cf / qt(0.975, 4) * qt(0.95, 4))
    myDF1$diff_cf_85 <- with(myDF1, diff_cf / qt(0.975, 4) * qt(0.925, 4))
    myDF1$diff_cf_80 <- with(myDF1, diff_cf / qt(0.975, 4) * qt(0.90, 4))
    myDF1$diff_cf_75 <- with(myDF1, diff_cf / qt(0.975, 4) * qt(0.875, 4))
    myDF1$diff_cf_68 <- with(myDF1, diff_cf / qt(0.975, 4) * qt(0.84, 4))
    
    myDF2$diff_cf_90 <- with(myDF2, diff_cf / qt(0.975, 4) * qt(0.95, 4))
    myDF2$diff_cf_85 <- with(myDF2, diff_cf / qt(0.975, 4) * qt(0.925, 4))
    myDF2$diff_cf_80 <- with(myDF2, diff_cf / qt(0.975, 4) * qt(0.90, 4))
    myDF2$diff_cf_75 <- with(myDF2, diff_cf / qt(0.975, 4) * qt(0.875, 4))
    myDF2$diff_cf_68 <- with(myDF2, diff_cf / qt(0.975, 4) * qt(0.84, 4))
    
    myDF3$diff_cf_90 <- with(myDF3, diff_cf / qt(0.975, 4) * qt(0.95, 4))
    myDF3$diff_cf_85 <- with(myDF3, diff_cf / qt(0.975, 4) * qt(0.925, 4))
    myDF3$diff_cf_80 <- with(myDF3, diff_cf / qt(0.975, 4) * qt(0.90, 4))
    myDF3$diff_cf_75 <- with(myDF3, diff_cf / qt(0.975, 4) * qt(0.875, 4))
    myDF3$diff_cf_68 <- with(myDF3, diff_cf / qt(0.975, 4) * qt(0.84, 4))
    
    
    ### add ci range to the data
    myDF1$diff_ci_low_95 <- with(myDF1, diff-diff_cf)
    myDF1$diff_ci_high_95 <- with(myDF1, diff+diff_cf)
    myDF1$diff_ci_low_85 <- with(myDF1, diff-diff_cf_85)
    myDF1$diff_ci_high_85 <- with(myDF1, diff+diff_cf_85)
    myDF1$diff_ci_low_75 <- with(myDF1, diff-diff_cf_75)
    myDF1$diff_ci_high_75 <- with(myDF1, diff+diff_cf_75)
    
    myDF2$diff_ci_low_95 <- with(myDF2, diff-diff_cf)
    myDF2$diff_ci_high_95 <- with(myDF2, diff+diff_cf)
    myDF2$diff_ci_low_85 <- with(myDF2, diff-diff_cf_85)
    myDF2$diff_ci_high_85 <- with(myDF2, diff+diff_cf_85)
    myDF2$diff_ci_low_75 <- with(myDF2, diff-diff_cf_75)
    myDF2$diff_ci_high_75 <- with(myDF2, diff+diff_cf_75)
    
    myDF3$diff_ci_low_95 <- with(myDF3, diff-diff_cf)
    myDF3$diff_ci_high_95 <- with(myDF3, diff+diff_cf)
    myDF3$diff_ci_low_85 <- with(myDF3, diff-diff_cf_85)
    myDF3$diff_ci_high_85 <- with(myDF3, diff+diff_cf_85)
    myDF3$diff_ci_low_75 <- with(myDF3, diff-diff_cf_75)
    myDF3$diff_ci_high_75 <- with(myDF3, diff+diff_cf_75)
    
    ### selct subset
    myDF1 <- myDF1[,c("conc.terms", "aCO2", "diff", 
                      "diff_ci_low_95", "diff_ci_high_95",
                      "diff_ci_low_85", "diff_ci_high_85",
                      "diff_ci_low_75", "diff_ci_high_75")]
    
    myDF2 <- myDF2[,c("terms", "aCO2", "diff", 
                      "diff_ci_low_95", "diff_ci_high_95",
                      "diff_ci_low_85", "diff_ci_high_85",
                      "diff_ci_low_75", "diff_ci_high_75")]
    
    myDF3 <- myDF3[,c("terms", "aCO2", "diff", 
                      "diff_ci_low_95", "diff_ci_high_95",
                      "diff_ci_low_85", "diff_ci_high_85",
                      "diff_ci_low_75", "diff_ci_high_75")]
    
    
    ### bootstrapped dataset
    myDF4 <- summary_table_concentration_bootstrap[,c("conc.terms", "aCO2", "diff", 
                                                      "diff_ci_low_95", "diff_ci_high_95",
                                                      "diff_ci_low_85", "diff_ci_high_85",
                                                      "diff_ci_low_75", "diff_ci_high_75")]
    
    myDF5 <- summary_table_pool_bootstrap[,c("terms", "aCO2", "diff", 
                                             "diff_ci_low_95", "diff_ci_high_95",
                                             "diff_ci_low_85", "diff_ci_high_85",
                                             "diff_ci_low_75", "diff_ci_high_75")]
    
    myDF6 <- summary_table_flux_bootstrap[,c("terms", "aCO2", "diff", 
                                             "diff_ci_low_95", "diff_ci_high_95",
                                             "diff_ci_low_85", "diff_ci_high_85",
                                             "diff_ci_low_75", "diff_ci_high_75")]
    
    ## add information
    myDF1$Data <- "Original"
    myDF2$Data <- "Original"
    myDF3$Data <- "Original"
    
    myDF4$Data <- "Bootstrap"
    myDF5$Data <- "Bootstrap"
    myDF6$Data <- "Bootstrap"
    
    ### merge datasets
    plotDF1 <- rbind(myDF1, myDF4)
    plotDF2 <- rbind(myDF2, myDF5)
    plotDF3 <- rbind(myDF3, myDF6)
    
    ### remove NA terms
    list1 <- myDF4[complete.cases(myDF4$aCO2),]$conc.terms
    list2 <- myDF5[complete.cases(myDF5$aCO2),]$terms
    list3 <- myDF6[complete.cases(myDF6$aCO2),]$terms
    
    ### only keep small list
    plotDF1 <- plotDF1[plotDF1$conc.terms%in%list1,]
    plotDF2 <- plotDF2[plotDF2$terms%in%list2,]
    plotDF3 <- plotDF3[plotDF3$terms%in%list3,]
    
    
    ### add positive and negative colors
    plotDF1$collab <- ifelse(plotDF1$diff > 0.0, "pos", 
                             ifelse(plotDF1$diff < 0.0, "neg", "neut"))
    
    plotDF2$collab <- ifelse(plotDF2$diff > 0.0, "pos", 
                             ifelse(plotDF2$diff < 0.0, "neg", "neut"))
    
    plotDF3$collab <- ifelse(plotDF3$diff > 0.0, "pos", 
                             ifelse(plotDF3$diff < 0.0, "neg", "neut"))
    
    
    ### plot
    #p1 <- ggplot(plotDF1) +  
    #    geom_hline(yintercept=0)+
    #    geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_95, 
    #                       ymax=diff_ci_high_95, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_85, 
    #                       ymax=diff_ci_high_85, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_75, 
    #                       ymax=diff_ci_high_75, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_point(aes(x=conc.terms, 
    #                   y=diff, group=Data, fill=Data), 
    #               stat='identity', size=4, shape=21,color="black",
    #               position = position_dodge(width = 0.7))+
    #    xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
    #    ylab("") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12, family="Helvetica"), 
    #          axis.text.x = element_text(size=12, family="Helvetica"),
    #          axis.text.y=element_text(size=12, family="Helvetica"),
    #          axis.title.y=element_text(size=12, family="Helvetica"),
    #          legend.text=element_text(size=12, family="Helvetica"),
    #          legend.title=element_text(size=12, family="Helvetica"),
    #          panel.grid.major=element_blank(),
    #          #legend.box.background = element_rect(alpha("grey",0.5)),
    #          legend.position=c(0.78, 0.6),
    #          legend.text.align=0)+
    #    scale_color_manual(name="Legend",
    #                       labels=c("pos"="Positive",
    #                                "neg"="Negative",
    #                                "neut"="Zero"),
    #                       values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
    #    scale_fill_manual(name="Method",
    #                      labels=c("Original"="Original",
    #                               "Bootstrap"="Bootstrap"),
    #                      values=c("Original"="black", 
    #                               "Bootstrap"="grey"))+
    #    coord_flip()
    #
    #
    #
    #### plot
    #p2 <- ggplot(plotDF2) +  
    #    geom_hline(yintercept=0)+
    #    geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
    #                       ymax=diff_ci_high_95, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
    #                       ymax=diff_ci_high_85, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
    #                       ymax=diff_ci_high_75, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_point(aes(x=terms, 
    #                   y=diff, group=Data, fill=Data), 
    #               stat='identity', size=4, shape=21,color="black",
    #               position = position_dodge(width = 0.7))+
    #    xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
    #    ylab("") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12, family="Helvetica"), 
    #          axis.text.x = element_text(size=12, family="Helvetica"),
    #          axis.text.y=element_text(size=12, family="Helvetica"),
    #          axis.title.y=element_text(size=12, family="Helvetica"),
    #          legend.text=element_text(size=12, family="Helvetica"),
    #          legend.title=element_text(size=12, family="Helvetica"),
    #          panel.grid.major=element_blank(),
    #          #legend.box.background = element_rect(alpha("grey",0.5)),
    #          legend.position=c(0.78, 0.6),
    #          legend.text.align=0)+
    #    scale_color_manual(name="Legend",
    #                       labels=c("pos"="Positive",
    #                                "neg"="Negative",
    #                                "neut"="Zero"),
    #                       values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
    #    scale_fill_manual(name="Method",
    #                      labels=c("Original"="Original",
    #                               "Bootstrap"="Bootstrap"),
    #                      values=c("Original"="black", 
    #                               "Bootstrap"="grey"))+
    #    coord_flip()
    #
    #
    #
    #### plot
    #p3 <- ggplot(plotDF3) +  
    #    geom_hline(yintercept=0)+
    #    geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
    #                       ymax=diff_ci_high_95, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
    #                       ymax=diff_ci_high_85, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
    #                       ymax=diff_ci_high_75, color=collab,
    #                       group=Data), alpha=0.2,
    #                   size=6, position = position_dodge(width = 0.7))+
    #    geom_point(aes(x=terms, 
    #                   y=diff, group=Data, fill=Data), 
    #               stat='identity', size=4, shape=21,color="black",
    #               position = position_dodge(width = 0.7))+
    #    xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
    #    ylab("") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12, family="Helvetica"), 
    #          axis.text.x = element_text(size=12, family="Helvetica"),
    #          axis.text.y=element_text(size=12, family="Helvetica"),
    #          axis.title.y=element_text(size=12, family="Helvetica"),
    #          legend.text=element_text(size=12, family="Helvetica"),
    #          legend.title=element_text(size=12, family="Helvetica"),
    #          panel.grid.major=element_blank(),
    #          #legend.box.background = element_rect(alpha("grey",0.5)),
    #          legend.position=c(0.78, 0.6),
    #          legend.text.align=0)+
    #    scale_color_manual(name="Legend",
    #                       labels=c("pos"="Positive",
    #                                "neg"="Negative",
    #                                "neut"="Zero"),
    #                       values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
    #    scale_fill_manual(name="Method",
    #                      labels=c("Original"="Original",
    #                               "Bootstrap"="Bootstrap"),
    #                      values=c("Original"="black", 
    #                               "Bootstrap"="grey"))+
    #    coord_flip()
    #
    #
    #pdf("output/si_figures/bootstrap_comparison_concentration.pdf", width = 8, height = 10)
    #plot(p1)
    #dev.off()
    #
    #
    #pdf("output/si_figures/bootstrap_comparison_pool.pdf", width = 8, height = 14)
    #plot(p2)
    #dev.off()
    #
    #pdf("output/si_figures/bootstrap_comparison_flux.pdf", width = 8, height = 14)
    #plot(p3)
    #dev.off()
    
    
    
    
    ### make specific plotting DF
    plotDF11 <- subset(plotDF1, conc.terms%in%c("Canopy P Conc", "Sapwood P Conc",
                                           "Fine Root P Conc",
                                           "Coarse Root P Conc", "Leaflitter P Conc",
                                           "Leaflitter P Conc", "Understorey P Conc",
                                           "Frass P Conc"))
    
    plotDF12 <- subset(plotDF1, conc.terms%in%c("Microbial P Conc 0-10cm", "Microbial P Conc 10-30cm",
                                           "Microbial P Conc 30-60cm", "Soil P Conc 0-10cm",
                                           "Soil P Conc 10-30cm", "Soil P Conc 30-60cm",
                                           "Soil Phosphate P Conc 0-10cm", "Soil Phosphate P Conc 10-30cm",
                                           "Soil Phosphate P Conc 30-60cm"))
    
    
    ### plot
    p41 <- ggplot(plotDF11) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=conc.terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position=c(0.78, 0.6),
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                          labels=c("pos"="Positive",
                                   "neg"="Negative",
                                   "neut"="Zero"),
                          values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                           labels=c("Original"="Original",
                                    "Bootstrap"="Bootstrap"),
                           values=c("Original"="black", 
                                    "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Frass P Conc",
                                  "Understorey P Conc",
                                  "Leaflitter P Conc",
                                  "Fine Root P Conc",
                                  "Canopy P Conc"),
                         labels=c("Canopy P Conc" = "Canopy P",
                                  "Fine Root P Conc" = "Fine root P", 
                                  "Leaflitter P Conc" = "Leaflitter P",
                                  "Understorey P Conc" = "Understorey P",
                                  "Frass P Conc" = "Frass P"))
    

    ### plot
    p42 <- ggplot(plotDF12) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=conc.terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=conc.terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                          labels=c("Original"="Original",
                                   "Bootstrap"="Bootstrap"),
                          values=c("Original"="black", 
                                   "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Soil P Conc 10-30cm",
                                  "Soil P Conc 0-10cm",
                                  "Soil Phosphate P Conc 30-60cm",
                                  "Soil Phosphate P Conc 10-30cm",
                                  "Soil Phosphate P Conc 0-10cm",
                                  "Microbial P Conc 30-60cm",
                                  "Microbial P Conc 10-30cm",
                                  "Microbial P Conc 0-10cm"),
                         labels=c("Microbial P Conc 0-10cm" = "Microbial P (0-10cm)",
                                  "Microbial P Conc 10-30cm" = "Microbial P (10-30cm)",
                                  "Microbial P Conc 30-60cm" = "Microbial P (30-60cm)",
                                  "Soil P Conc 0-10cm" = "Soil P (0-10cm)",
                                  "Soil P Conc 10-30cm" = "Soil P (10-30cm)",
                                  "Soil Phosphate P Conc 0-10cm" = expression("Soil Labile P (0-10cm)"),
                                  "Soil Phosphate P Conc 10-30cm" = expression("Soil Labile P (10-30cm)"),
                                  "Soil Phosphate P Conc 30-60cm" = expression("Soil Labile P (30-60cm)")))
    
    
    grid.labs <- c("(a)", "(b)")
    
    pdf("output/si_figures/si_figure6.pdf", width = 16, height = 6)
    plot_grid(p41, p42, ncol=2)
    grid.text(grid.labs,x = c(0.45, 0.95), y = c(0.95, 0.95),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    
    
    ###########################################################################
    ### Pools
    plotDF21 <- subset(plotDF2, terms%in%c("Canopy P Pool", "Sapwood P Pool",
                                           "Heartwood P Pool", "Total Wood P Pool",
                                           "Fine Root P Pool",
                                           "Coarse Root P Pool", 
                                           "Forestfloor Leaf Litter P Pool",
                                           "Understorey P Pool",
                                           "Understorey Litter P Pool"))
    
    plotDF22 <- subset(plotDF2, terms%in%c("Microbial P Pool 0-10cm", "Microbial P Pool 10-30cm",
                                           "Microbial P Pool 30-60cm", "Soil P Pool 0-10cm",
                                           "Soil P Pool 10-30cm", "Soil P Pool 30-60cm",
                                           "Soil Phosphate P Pool 0-10cm", "Soil Phosphate P Pool 10-30cm",
                                           "Soil Phosphate P Pool 30-60cm"))
    
    plotDF23 <- subset(plotDF2, terms%in%c("Exchangeable Pi Pool",
                                           "Exchangeable Po Pool",
                                           "Moderately labile Po Pool",
                                           "Occluded P Pool"))
    
    
    
    
    ### plot
    p51 <- ggplot(plotDF21) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                          labels=c("Original"="Original",
                                   "Bootstrap"="Bootstrap"),
                          values=c("Original"="black", 
                                   "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Understorey Litter P Pool",
                                  "Understorey P Pool",
                                  "Forestfloor Leaf Litter P Pool",
                                  "Coarse Root P Pool",
                                  "Fine Root P Pool",
                                  "Total Wood P Pool",
                                  "Heartwood P Pool",
                                  "Sapwood P Pool",
                                  "Canopy P Pool"),
                         labels=c("Canopy P Pool" = "Canopy P",
                                  "Sapwood P Pool" = "Sapwood P",
                                  "Heartwood P Pool" = "Heartwood P",
                                  "Total Wood P Pool" = "Total Wood P",
                                  "Fine Root P Pool" = "Fine root P", 
                                  "Coarse Root P Pool" = "Coarse root P",
                                  "Forestfloor Leaf Litter P Pool" = "Forestfloor leaf P",
                                  "Understorey P Pool" = "Understorey P",
                                  "Understorey Litter P Pool" = "Understorey litter P"))
    
    p52 <- ggplot(plotDF22) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position=c(0.78, 0.6),
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                          labels=c("Original"="Original",
                                   "Bootstrap"="Bootstrap"),
                          values=c("Original"="black", 
                                   "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Soil P Pool 10-30cm",
                                  "Soil P Pool 0-10cm",
                                  "Soil Phosphate P Pool 30-60cm",
                                  "Soil Phosphate P Pool 10-30cm",
                                  "Soil Phosphate P Pool 0-10cm",
                                  "Microbial P Pool 30-60cm",
                                  "Microbial P Pool 10-30cm",
                                  "Microbial P Pool 0-10cm"),
                         labels=c("Microbial P Pool 0-10cm" = "Microbial P (0-10cm)",
                                  "Microbial P Pool 10-30cm" = "Microbial P (10-30cm)",
                                  "Microbial P Pool 30-60cm" = "Microbial P (30-60cm)",
                                  "Soil P Pool 0-10cm" = "Soil P (0-10cm)",
                                  "Soil P Pool 10-30cm" = "Soil P (10-30cm)",
                                  "Soil Phosphate P Pool 0-10cm" = expression("Soil Labile P (0-10cm)"),
                                  "Soil Phosphate P Pool 10-30cm" = expression("Soil Labile P (10-30cm)"),
                                  "Soil Phosphate P Pool 30-60cm" = expression("Soil Labile P (30-60cm)")))
    
    
    
    p53 <- ggplot(plotDF23) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                          labels=c("Original"="Original",
                                   "Bootstrap"="Bootstrap"),
                          values=c("Original"="black", 
                                   "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Occluded P Pool",
                                  #"Primary Ca bound Pi Pool",
                                  #"Secondary Fe bound Pi Pool",
                                  "Moderately labile Po Pool",
                                  "Exchangeable Po Pool",
                                  "Exchangeable Pi Pool"),
                         labels=c("Exchangeable Pi Pool" = expression("Exchangeable " * P[i]),
                                  "Exchangeable Po Pool" = expression("Exchangeable " * P[o]),
                                  "Moderately labile Po Pool" = expression("Moderately Labile " * P[o]),
                                  #"Secondary Fe bound Pi Pool" = expression("Secondary " * F[e]-bound * " " * P[i]),
                                  #"Primary Ca bound Pi Pool" = expression("Primary " * C[a]-bound * " " * P[i]),
                                  "Occluded P Pool" = expression("Occluded P")))
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)")
    
    pdf("output/si_figures/si_figure7.pdf", width = 14, height = 10)
    left_col <- plot_grid(p51, p53, ncol=1, rel_heights=c(1.2, 0.8))
    plot_grid(left_col, p52,  ncol = 2, rel_widths = c(1, 1),
              rel_heights=c(1, 1))
    
    grid.text(grid.labs,x = c(0.47, 0.96, 0.47), 
              y = c(0.96, 0.96, 0.37),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    
    
    dev.off()
    
    
    
    
    #############################################
    plotDF31 <- subset(plotDF3, terms%in%c("Canopy P flux", 
                                           "Wood P flux",
                                           "Fine Root P flux",
                                           "Coarse Root P flux", 
                                           "Understorey P flux"))
    
    plotDF32 <- subset(plotDF3, terms%in%c("Fineroot Litter P flux",
                                           "Leaflitter P flux",
                                           "Bark litter P flux",
                                           "Twig litter P flux",
                                           "Seed litter P flux",
                                           "Frass P flux",
                                           "Understorey Litter P flux"))
    
    plotDF33 <- subset(plotDF3, terms%in%c("Canopy retrans P flux", 
                                           "Sapwood retrans P flux",
                                           "Understorey retrans P flux"))
    
    plotDF34 <- subset(plotDF3, terms%in%c("Total vegetation production P flux", 
                                           "Total vegetation retranslocation P flux",
                                           "Total vegetation uptake P flux", 
                                           "Mineralization P flux 0-10cm",
                                           "Mineralization P flux 10-30cm", 
                                           "Mineralization P flux 30-60cm"))
    
    
    p31 <- ggplot(plotDF31) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                          labels=c("Original"="Original",
                                   "Bootstrap"="Bootstrap"),
                          values=c("Original"="black", 
                                   "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Understorey P flux",
                                  "Coarse Root P flux",
                                  "Fine Root P flux",
                                  "Wood P flux",
                                  "Canopy P flux"),
                         labels=c("Canopy P flux" = "Canopy P",
                                  "Wood P flux" = "Wood P",
                                  "Fine Root P flux" = "Fine root P", 
                                  "Coarse Root P flux" = "Coarse root P",
                                  "Understorey P flux" = "Understorey P"))
    
    
    p32 <- ggplot(plotDF32) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                          labels=c("Original"="Original",
                                   "Bootstrap"="Bootstrap"),
                          values=c("Original"="black", 
                                   "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Frass P flux",
                                  "Understorey Litter P flux",
                                  "Fineroot Litter P flux",
                                  "Seed litter P flux",
                                  "Bark litter P flux",
                                  "Twig litter P flux",
                                  "Leaflitter P flux"),
                         labels=c("Leaflitter P flux" = "Leaf litter P",
                                  "Twig litter P flux" = "Twig litter P",
                                  "Bark litter P flux" = "Bark litter P",
                                  "Seed litter P flux" = "Seed litter P",
                                  "Fineroot Litter P flux" = "Fine root litter P", 
                                  "Understorey Litter P flux" = "Understorey litter P",
                                  "Frass P flux" = "Frass P"))
    
    
    p33 <- ggplot(plotDF33) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position=c(0.78, 0.6),
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                          labels=c("Original"="Original",
                                   "Bootstrap"="Bootstrap"),
                          values=c("Original"="black", 
                                   "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Understorey retrans P flux",
                                  "Sapwood retrans P flux",
                                  "Canopy retrans P flux"),
                         labels=c("Canopy retrans P flux" = "Canopy retrans P",
                                  "Sapwood retrans P flux" = "Sapwood retrans P",
                                  "Understorey retrans P flux" = "Understorey retrans P"))
    
    
    p34 <- ggplot(plotDF34) +  
        geom_hline(yintercept=0)+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_95, 
                           ymax=diff_ci_high_95, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_85, 
                           ymax=diff_ci_high_85, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_linerange(aes(x=terms, ymin=diff_ci_low_75, 
                           ymax=diff_ci_high_75, color=collab,
                           group=Data), alpha=0.2,
                       size=6, position = position_dodge(width = 0.7))+
        geom_point(aes(x=terms, 
                       y=diff, group=Data, fill=Data), 
                   stat='identity', size=4, shape=21,color="black",
                   position = position_dodge(width = 0.7))+
        xlab(expression(paste(CO[2], " effect (ele - amb, %)"))) + 
        ylab("") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_color_manual(name="Legend",
                           labels=c("pos"="Positive",
                                    "neg"="Negative",
                                    "neut"="Zero"),
                           values=c("pos"=cbPalette[4], "neg"=cbPalette[7], "neut"="black"))+
        scale_fill_manual(name="Method",
                          labels=c("Original"="Original",
                                   "Bootstrap"="Bootstrap"),
                          values=c("Original"="black", 
                                   "Bootstrap"="grey"))+
        coord_flip()+
        scale_x_discrete(limits=c("Mineralization P flux 30-60cm",
                                  "Mineralization P flux 10-30cm",
                                  "Mineralization P flux 0-10cm",
                                  "Total vegetation uptake P flux",
                                  "Total vegetation retranslocation P flux",
                                  "Total vegetation production P flux"),
                         labels=c("Mineralization P flux 30-60cm" = expression("Net " * P[min] * " 30-60cm"),
                                  "Mineralization P flux 10-30cm" = expression("Net " * P[min] * " 10-30cm"),
                                  "Mineralization P flux 0-10cm" = expression("Net " * P[min] * " 0-10cm"),
                                  "Total vegetation uptake P flux" = "Plant P uptake",
                                  "Total vegetation retranslocation P flux" = "Plant P resorption",
                                  "Total vegetation production P flux" = "Plant P demand"))
        
    
    
    
    ### 
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    pdf("output/si_figures/si_figure8.pdf", width = 12, height = 10)
    
    plot_grid(p34, p31, p32, p33, ncol=2, nrow=2)
    
    grid.text(grid.labs,
              x = c(0.47, 0.95, 0.47, 0.95), 
              y = c(0.96, 0.96, 0.47, 0.47),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    
    dev.off()
    
    
    
    
    
}