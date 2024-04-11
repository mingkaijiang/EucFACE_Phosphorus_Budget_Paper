
analyze_time_effect_on_pool <- function (soil_p_pool,
                                         soil_inorganic_p_pool,
                                         soil_organic_p_pool,
                                         soil_phosphate_pool,
                                         soil_p_pool_hedley,
                                         microbial_p_pool,
                                         canopy_p_pool,
                                         leaflitter_p_pool,
                                         wood_p_pool,
                                         sapwood_p_pool,
                                         heartwood_p_pool,
                                         standing_dead_p_pool,
                                         fineroot_p_pool,
                                         understorey_p_pool,
                                         coarse_root_p_pool) {
    
    ##########################################################################
    ###
    ### to perform statistical analysis of the time-sequenced datasets
    ###
    ##########################################################################
    
    ### Define pool variable names
    terms <- c("Canopy P Pool", 
               "Total Wood P Pool", 
               #"Sapwood P Pool",
               #"Heartwood P Pool",
               "Forestfloor Leaf Litter P Pool",
               "Fine Root P Pool",
               "Coarse Root P Pool", 
               "Understorey P Pool", 
               #"Understorey Litter P Pool",
               "Standing Dead Wood P Pool",
               "Microbial P Pool 0-10cm", 
               "Microbial P Pool 10-30cm", 
               "Microbial P Pool 30-60cm", 
               "Soil Phosphate P Pool 0-10cm",
               "Soil Phosphate P Pool 10-30cm",
               "Soil Phosphate P Pool 30-60cm",
               "Soil P Pool 0-10cm",
               "Soil P Pool 10-30cm", 
               "Soil P Pool 30-60cm")
    
    
    ### add treatment variable
    soil_p_pool$Trt <- "aCO2"
    soil_p_pool$Trt[soil_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_inorganic_p_pool$Trt <- "aCO2"
    soil_inorganic_p_pool$Trt[soil_inorganic_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_organic_p_pool$Trt <- "aCO2"
    soil_organic_p_pool$Trt[soil_organic_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_phosphate_pool$Trt <- "aCO2"
    soil_phosphate_pool$Trt[soil_phosphate_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_p_pool_hedley$Trt <- "aCO2"
    soil_p_pool_hedley$Trt[soil_p_pool_hedley$Ring%in%c(1,4,5)] <- "eCO2"
    
    microbial_p_pool$Trt <- "aCO2"
    microbial_p_pool$Trt[microbial_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    canopy_p_pool$Trt <- "aCO2"
    canopy_p_pool$Trt[canopy_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    leaflitter_p_pool$Trt <- "aCO2"
    leaflitter_p_pool$Trt[leaflitter_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    wood_p_pool$Trt <- "aCO2"
    wood_p_pool$Trt[wood_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    sapwood_p_pool$Trt <- "aCO2"
    sapwood_p_pool$Trt[sapwood_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    heartwood_p_pool$Trt <- "aCO2"
    heartwood_p_pool$Trt[heartwood_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    standing_dead_p_pool$Trt <- "aCO2"
    standing_dead_p_pool$Trt[standing_dead_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_p_pool$Trt <- "aCO2"
    fineroot_p_pool$Trt[fineroot_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_p_pool$Trt <- "aCO2"
    understorey_p_pool$Trt[understorey_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    coarse_root_p_pool$Trt <- "aCO2"
    coarse_root_p_pool$Trt[coarse_root_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### add factors
    canopy_p_pool$TrtFactor <- as.factor(canopy_p_pool$Trt)
    canopy_p_pool$RingFactor <- as.factor(canopy_p_pool$Ring)
    canopy_p_pool$DateFactor <- as.factor(canopy_p_pool$Date)
    canopy_p_pool$YearFactor <- as.factor(year(canopy_p_pool$Date))
    
    wood_p_pool$TrtFactor <- as.factor(wood_p_pool$Trt)
    wood_p_pool$RingFactor <- as.factor(wood_p_pool$Ring)
    wood_p_pool$DateFactor <- as.factor(wood_p_pool$Date)
    wood_p_pool$YearFactor <- as.factor(year(wood_p_pool$Date))
    
    fineroot_p_pool$TrtFactor <- as.factor(fineroot_p_pool$Trt)
    fineroot_p_pool$RingFactor <- as.factor(fineroot_p_pool$Ring)
    fineroot_p_pool$DateFactor <- as.factor(fineroot_p_pool$Date)
    fineroot_p_pool$YearFactor <- as.factor(year(fineroot_p_pool$Date))
    
    leaflitter_p_pool$TrtFactor <- as.factor(leaflitter_p_pool$Trt)
    leaflitter_p_pool$RingFactor <- as.factor(leaflitter_p_pool$Ring)
    leaflitter_p_pool$DateFactor <- as.factor(leaflitter_p_pool$Date)
    leaflitter_p_pool$YearFactor <- as.factor(year(leaflitter_p_pool$Date))
    
    coarse_root_p_pool$TrtFactor <- as.factor(coarse_root_p_pool$Trt)
    coarse_root_p_pool$RingFactor <- as.factor(coarse_root_p_pool$Ring)
    coarse_root_p_pool$DateFactor <- as.factor(coarse_root_p_pool$Date)
    coarse_root_p_pool$YearFactor <- as.factor(year(coarse_root_p_pool$Date))
    
    understorey_p_pool$TrtFactor <- as.factor(understorey_p_pool$Trt)
    understorey_p_pool$RingFactor <- as.factor(understorey_p_pool$Ring)
    understorey_p_pool$DateFactor <- as.factor(understorey_p_pool$Date)
    understorey_p_pool$YearFactor <- as.factor(year(understorey_p_pool$Date))
    
    standing_dead_p_pool$TrtFactor <- as.factor(standing_dead_p_pool$Trt)
    standing_dead_p_pool$RingFactor <- as.factor(standing_dead_p_pool$Ring)
    standing_dead_p_pool$DateFactor <- as.factor(standing_dead_p_pool$Date)
    standing_dead_p_pool$YearFactor <- as.factor(year(standing_dead_p_pool$Date))
    
    microbial_p_pool$TrtFactor <- as.factor(microbial_p_pool$Trt)
    microbial_p_pool$RingFactor <- as.factor(microbial_p_pool$Ring)
    microbial_p_pool$DateFactor <- as.factor(microbial_p_pool$Date)
    microbial_p_pool$YearFactor <- as.factor(year(microbial_p_pool$Date))
    
    soil_p_pool$TrtFactor <- as.factor(soil_p_pool$Trt)
    soil_p_pool$RingFactor <- as.factor(soil_p_pool$Ring)
    soil_p_pool$DateFactor <- as.factor(soil_p_pool$Date)
    soil_p_pool$YearFactor <- as.factor(year(soil_p_pool$Date))
    
    soil_phosphate_pool$TrtFactor <- as.factor(soil_phosphate_pool$Trt)
    soil_phosphate_pool$RingFactor <- as.factor(soil_phosphate_pool$Ring)
    soil_phosphate_pool$DateFactor <- as.factor(soil_phosphate_pool$Date)
    soil_phosphate_pool$YearFactor <- as.factor(year(soil_phosphate_pool$Date))
    
    
    ### prepare storage dataframe to store output statitics
    outDF <- data.frame("variable"=terms,
                        "F_statistic_YearFactor"=NA,
                        "F_statistic_TrtFactor"=NA,
                        "F_statistic_Year_TrtFactor"=NA,
                        "Df_YearFactor"=NA,
                        "Df_TrtFactor"=NA,
                        "Df_Year_TrtFactor"=NA,
                        "Df.res_YearFactor"=NA,
                        "Df.res_TrtFactor"=NA,
                        "Df.res_Year_TrtFactor"=NA,
                        "p_value_YearFactor"=NA,
                        "p_value_TrtFactor"=NA,
                        "p_value_Year_TrtFactor"=NA)
    
    
    
    
    ###############################################################
    ### canopy p pool
    ### linear mixed effect model
    mod.result <- lmer(leaf_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=canopy_p_pool)
    mod.result <- lmer(leaf_p_pool~DateFactor * TrtFactor + (1|RingFactor),data=canopy_p_pool)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ## Check ele - amb diff
    #summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    #eff.size <- coef(modelt1)[[1]][1,2]
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Canopy P Pool"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### fineroot p pool
    ### linear mixed effect model
    mod.result <- lmer(fineroot_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=fineroot_p_pool)
    mod.result <- lmer(fineroot_p_pool~DateFactor * TrtFactor + (1|RingFactor),data=fineroot_p_pool)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Fine Root P Pool"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### wood p pool
    ### linear mixed effect model
    mod.result <- lmer(wood_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=wood_p_pool)
    mod.result <- lmer(wood_p_pool~DateFactor * TrtFactor + (1|RingFactor),data=wood_p_pool)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Total Wood P Pool"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### standing dead wood p pool
    ### linear mixed effect model
    #mod.result <- lmer(wood_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=standing_dead_p_pool)
    #mod.result <- lmer(wood_p_pool~DateFactor * TrtFactor + (1|RingFactor),data=standing_dead_p_pool)
    
    ## anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Standing Dead Wood P Pool"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### coarse root p pool
    ### linear mixed effect model
    mod.result <- lmer(coarse_root_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=coarse_root_p_pool)
    mod.result <- lmer(coarse_root_p_pool~DateFactor * TrtFactor + (1|RingFactor),data=coarse_root_p_pool)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Coarse Root P Pool"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### leaflitter p pool
    ### linear mixed effect model
    mod.result <- lmer(leaflitter_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=leaflitter_p_pool)
    mod.result <- lmer(leaflitter_p_pool~DateFactor * TrtFactor + (1|RingFactor),data=leaflitter_p_pool)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Forestfloor Leaf Litter P Pool"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### understorey p pool
    ### linear mixed effect model
    mod.result <- lmer(understorey_p_pool~YearFactor * TrtFactor + (1|RingFactor),data=understorey_p_pool)
    mod.result <- lmer(understorey_p_pool~DateFactor * TrtFactor + (1|RingFactor),data=understorey_p_pool)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Understorey P Pool"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### microbial p pool
    ### linear mixed effect model
    mod.result <- lmer(microbial_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=microbial_p_pool[microbial_p_pool$Depth=="0_10",])
    mod.result <- lmer(microbial_p_g_m2~DateFactor * TrtFactor + (1|RingFactor),data=microbial_p_pool[microbial_p_pool$Depth=="0_10",])
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Microbial P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### microbial p pool
    #### linear mixed effect model
    #mod.result <- lmer(microbial_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=microbial_p_pool[microbial_p_pool$Depth=="10_30",])
    #mod.result <- lmer(microbial_p_g_m2~DateFactor * TrtFactor + (1|RingFactor),data=microbial_p_pool[microbial_p_pool$Depth=="10_30",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Microbial P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[3]
    #
    #
    ################################################################
    #### microbial p pool
    #### linear mixed effect model
    #mod.result <- lmer(microbial_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=microbial_p_pool[microbial_p_pool$Depth=="transition",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Microbial P Pool 30-60cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### soil p pool
    ### linear mixed effect model
    mod.result <- lmer(soil_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=soil_p_pool[soil_p_pool$Depth=="0_10",])
    mod.result <- lmer(soil_p_g_m2~DateFactor * TrtFactor + (1|RingFactor),data=soil_p_pool[soil_p_pool$Depth=="0_10",])
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Soil P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### soil p pool
    ### linear mixed effect model
    #mod.result <- lmer(soil_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=soil_p_pool[soil_p_pool$Depth=="10_30",])
    #mod.result <- lmer(soil_p_g_m2~DateFactor * TrtFactor + (1|RingFactor),data=soil_p_pool[soil_p_pool$Depth=="10_30",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Soil P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    
    ###############################################################
    ### soil phosphate p pool
    ### linear mixed effect model
    mod.result <- lmer(soil_phosphate_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=soil_phosphate_pool[soil_phosphate_pool$Depth=="0_10",])
    mod.result <- lmer(soil_phosphate_p_g_m2~DateFactor * TrtFactor + (1|RingFactor),data=soil_phosphate_pool[soil_phosphate_pool$Depth=="0_10",])
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 0-10cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### soil phosphate p pool
    #### linear mixed effect model
    #mod.result <- lmer(soil_phosphate_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=soil_phosphate_pool[soil_phosphate_pool$Depth=="10_30",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 10-30cm"] <- mod.anova$`Pr(>F)`[3]
    #
    #
    ################################################################
    #### Soil phosphate p pool
    #### linear mixed effect model
    #mod.result <- lmer(soil_phosphate_p_g_m2~YearFactor * TrtFactor + (1|RingFactor),data=soil_phosphate_pool[soil_phosphate_pool$Depth=="transition",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Soil Phosphate P Pool 30-60cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ### save
    write.csv(outDF, paste0("output/summary_tables/statistics_P_pool.csv"), row.names=F)
    
    

    
}