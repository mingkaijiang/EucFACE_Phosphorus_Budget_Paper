
analyze_time_effect_on_concentration <- function (canopy_p_concentration,
                                                  sapwood_p_concentration,
                                                  fineroot_p_concentration,
                                                  leaflitter_p_concentration,
                                                  understorey_p_concentration,
                                                  understorey_litter_p_concentration,
                                                  frass_p_concentration,
                                                  microbial_p_concentration,
                                                  soil_p_concentration,
                                                  soil_inorganic_p_concentration,
                                                  soil_organic_p_concentration,
                                                  soil_phosphate_concentration,
                                                  soil_hedley_p_concentration) {
    
    ##########################################################################
    ###
    ### to perform statistical analysis of the time-sequenced datasets
    ###
    ##########################################################################
    
    
    ### Define concentration variable names
    conc.terms <- c("Canopy P Conc", 
                    "Sapwood P Conc", 
                    "Heartwood P Conc",
                    "Fine Root P Conc", 
                    "Coarse Root P Conc",
                    "Leaflitter P Conc",
                    "Understorey P Conc", 
                    "Understorey Litter P Conc", 
                    "Frass P Conc",
                    "Microbial P Conc 0-10cm", 
                    "Microbial P Conc 10-30cm", 
                    "Microbial P Conc 30-60cm", 
                    "Soil P Conc 0-10cm", 
                    "Soil P Conc 10-30cm", 
                    "Soil P Conc 30-60cm", 
                    "Soil Inorg P Conc 0-10cm", 
                    "Soil Inorg P Conc 10-30cm", 
                    "Soil Inorg P Conc 30-60cm", 
                    "Soil Org P Conc 0-10cm", 
                    "Soil Org P Conc 10-30cm", 
                    "Soil Org P Conc 30-60cm", 
                    "Soil Phosphate P Conc 0-10cm",
                    "Soil Phosphate P Conc 10-30cm",
                    "Soil Phosphate P Conc 30-60cm",
                    "Exchangeable Pi Conc 0-10cm", 
                    "Exchangeable Po Conc 0-10cm",
                    "Moderately labile Po Conc 0-10cm", 
                    "Secondary Fe bound Pi Conc 0-10cm", 
                    "Primary Ca bound Pi Conc 0-10cm",
                    "Occluded P Conc 0-10cm")
    
    
    ### add treatment variable
    canopy_p_concentration$Trt <- "aCO2"
    canopy_p_concentration$Trt[canopy_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    sapwood_p_concentration$Trt <- "aCO2"
    sapwood_p_concentration$Trt[sapwood_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_p_concentration$Trt <- "aCO2"
    fineroot_p_concentration$Trt[fineroot_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    leaflitter_p_concentration$Trt <- "aCO2"
    leaflitter_p_concentration$Trt[leaflitter_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_p_concentration$Trt <- "aCO2"
    understorey_p_concentration$Trt[understorey_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_litter_p_concentration$Trt <- "aCO2"
    understorey_litter_p_concentration$Trt[understorey_litter_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    frass_p_concentration$Trt <- "aCO2"
    frass_p_concentration$Trt[frass_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    microbial_p_concentration$Trt <- "aCO2"
    microbial_p_concentration$Trt[microbial_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_p_concentration$Trt <- "aCO2"
    soil_p_concentration$Trt[soil_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_inorganic_p_concentration$Trt <- "aCO2"
    soil_inorganic_p_concentration$Trt[soil_inorganic_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_organic_p_concentration$Trt <- "aCO2"
    soil_organic_p_concentration$Trt[soil_organic_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_phosphate_concentration$Trt <- "aCO2"
    soil_phosphate_concentration$Trt[soil_phosphate_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_hedley_p_concentration$Trt <- "aCO2"
    soil_hedley_p_concentration$Trt[soil_hedley_p_concentration$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### add factors
    canopy_p_concentration$TrtFactor <- as.factor(canopy_p_concentration$Trt)
    canopy_p_concentration$RingFactor <- as.factor(canopy_p_concentration$Ring)
    canopy_p_concentration$DateFactor <- as.factor(canopy_p_concentration$Date)
    canopy_p_concentration$YearFactor <- as.factor(year(canopy_p_concentration$Date))
    
    fineroot_p_concentration$TrtFactor <- as.factor(fineroot_p_concentration$Trt)
    fineroot_p_concentration$RingFactor <- as.factor(fineroot_p_concentration$Ring)
    fineroot_p_concentration$DateFactor <- as.factor(fineroot_p_concentration$Date)
    fineroot_p_concentration$YearFactor <- as.factor(year(fineroot_p_concentration$Date))
    
    leaflitter_p_concentration$TrtFactor <- as.factor(leaflitter_p_concentration$Trt)
    leaflitter_p_concentration$RingFactor <- as.factor(leaflitter_p_concentration$Ring)
    leaflitter_p_concentration$DateFactor <- as.factor(leaflitter_p_concentration$Date)
    leaflitter_p_concentration$YearFactor <- as.factor(year(leaflitter_p_concentration$Date))
    
    frass_p_concentration$TrtFactor <- as.factor(frass_p_concentration$Trt)
    frass_p_concentration$RingFactor <- as.factor(frass_p_concentration$Ring)
    frass_p_concentration$DateFactor <- as.factor(frass_p_concentration$Date)
    frass_p_concentration$YearFactor <- as.factor(year(frass_p_concentration$Date))
    
    understorey_p_concentration$TrtFactor <- as.factor(understorey_p_concentration$Trt)
    understorey_p_concentration$RingFactor <- as.factor(understorey_p_concentration$Ring)
    understorey_p_concentration$DateFactor <- as.factor(understorey_p_concentration$Date)
    understorey_p_concentration$YearFactor <- as.factor(year(understorey_p_concentration$Date))
    
    microbial_p_concentration$TrtFactor <- as.factor(microbial_p_concentration$Trt)
    microbial_p_concentration$RingFactor <- as.factor(microbial_p_concentration$Ring)
    microbial_p_concentration$DateFactor <- as.factor(microbial_p_concentration$Date)
    microbial_p_concentration$YearFactor <- as.factor(year(microbial_p_concentration$Date))
    
    soil_p_concentration$TrtFactor <- as.factor(soil_p_concentration$Trt)
    soil_p_concentration$RingFactor <- as.factor(soil_p_concentration$Ring)
    soil_p_concentration$DateFactor <- as.factor(soil_p_concentration$Date)
    soil_p_concentration$YearFactor <- as.factor(year(soil_p_concentration$Date))
    
    soil_phosphate_concentration$TrtFactor <- as.factor(soil_phosphate_concentration$Trt)
    soil_phosphate_concentration$RingFactor <- as.factor(soil_phosphate_concentration$Ring)
    soil_phosphate_concentration$DateFactor <- as.factor(soil_phosphate_concentration$Date)
    soil_phosphate_concentration$YearFactor <- as.factor(year(soil_phosphate_concentration$Date))
    
    
    
    ### prepare storage dataframe to store output statitics
    outDF <- data.frame("variable"=conc.terms,
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
    ### canopy p concentration
    ### linear mixed effect model
    mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=canopy_p_concentration)
    #mod.result <- lmer(PercP~DateFactor * TrtFactor + (1|RingFactor),data=canopy_p_concentration)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ## Check ele - amb diff
    #summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    #eff.size <- coef(modelt1)[[1]][1,2]
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Canopy P Conc"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### fineroot p concentration
    ### linear mixed effect model
    mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=fineroot_p_concentration)
    mod.result <- lmer(PercP~DateFactor * TrtFactor + (1|RingFactor),data=fineroot_p_concentration)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Fine Root P Conc"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### leaflitter p concentration
    ### linear mixed effect model
    mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=leaflitter_p_concentration)
    mod.result <- lmer(PercP~DateFactor * TrtFactor + (1|RingFactor),data=leaflitter_p_concentration)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Leaflitter P Conc"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### understorey p concentration
    ### linear mixed effect model
    #mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=understorey_p_concentration)
    mod.result <- lmer(PercP~DateFactor * TrtFactor + (1|RingFactor),data=understorey_p_concentration)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Understorey P Conc"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### frass p concentration
    ### linear mixed effect model
    mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=frass_p_concentration)
    mod.result <- lmer(PercP~DateFactor * TrtFactor + (1|RingFactor),data=frass_p_concentration)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Frass P Conc"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Frass P Conc"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Frass P Conc"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Frass P Conc"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Frass P Conc"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Frass P Conc"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Frass P Conc"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Frass P Conc"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Frass P Conc"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Frass P Conc"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Frass P Conc"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Frass P Conc"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### microbial p concentration
    ### linear mixed effect model
    mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=microbial_p_concentration[microbial_p_concentration$Depth=="0_10",])
    mod.result <- lmer(PercP~DateFactor * TrtFactor + (1|RingFactor),data=microbial_p_concentration[microbial_p_concentration$Depth=="0_10",])
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Microbial P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### microbial p concentration
    ### linear mixed effect model
    #mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=microbial_p_concentration[microbial_p_concentration$Depth=="10_30",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Microbial P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[3]
    #
    #
    ################################################################
    #### microbial p concentration
    #### linear mixed effect model
    #mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=microbial_p_concentration[microbial_p_concentration$Depth=="transition",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Microbial P Conc 30-60cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### soil p concentration
    ### linear mixed effect model
    mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=soil_p_concentration[soil_p_concentration$Depth=="0_10",])
    mod.result <- lmer(PercP~DateFactor * TrtFactor + (1|RingFactor),data=soil_p_concentration[soil_p_concentration$Depth=="0_10",])
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Soil P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### soil p concentration
    ### linear mixed effect model
    #mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=soil_p_concentration[soil_p_concentration$Depth=="10_30",])
#
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Soil P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    
    ###############################################################
    ### soil phosphate p concentration
    ### linear mixed effect model
    mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=soil_phosphate_concentration[soil_phosphate_concentration$Depth=="0_10",])
    mod.result <- lmer(PercP~DateFactor * TrtFactor + (1|RingFactor),data=soil_phosphate_concentration[soil_phosphate_concentration$Depth=="0_10",])
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 0-10cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### soil phosphate p concentration
    ### linear mixed effect model
    #mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=soil_phosphate_concentration[soil_phosphate_concentration$Depth=="10_30",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 10-30cm"] <- mod.anova$`Pr(>F)`[3]
    #
    #
    ################################################################
    #### Soil phosphate p concentration
    #### linear mixed effect model
    #mod.result <- lmer(PercP~YearFactor * TrtFactor + (1|RingFactor),data=soil_phosphate_concentration[soil_phosphate_concentration$Depth=="transition",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Soil Phosphate P Conc 30-60cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ### save
    write.csv(outDF, paste0("output/summary_tables/statistics_P_concentration.csv"), row.names=F)
    
    

    
}