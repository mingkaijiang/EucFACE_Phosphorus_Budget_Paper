
analyze_time_effect_on_flux <- function (soil_p_mineralization,
                                         soil_p_leaching,
                                         canopy_p_flux,
                                         frass_p_production,
                                         leaflitter_p_flux,
                                         fineroot_p_production,
                                         fineroot_litter_p_flux,
                                         twig_litter_p_flux,
                                         bark_litter_p_flux,
                                         seed_litter_p_flux,
                                         wood_p_flux,
                                         coarse_root_p_flux,
                                         understorey_p_flux,
                                         understorey_litter_p_flux,
                                         canopy_P_retranslocation_flux,
                                         sapwood_P_retranslocation_flux,
                                         understorey_P_retranslocation_flux,
                                         fineroot_P_retranslocation_flux,
                                         coarseroot_P_retranslocation_flux) {
    
    ##########################################################################
    ###
    ### to perform statistical analysis of the time-sequenced datasets
    ###
    ##########################################################################
    
    ### Define flux variable names
    terms <- c("Canopy P Flux",              # include retranslocated and new uptake flux
               #"Herbivory P Flux",
               "Wood P Flux",                # include retranslocated and new uptake flux
               "Fine Root P Flux",           # include retranslocated and new uptake flux
               "Coarse Root P Flux",         # include retranslocated and new uptake flux
               "Leaflitter P Flux",          # Proxy for new uptake
               "Fineroot Litter P Flux",     # Proxy for new uptake
               "Twig Litter P Flux",         # to be included in the new uptake
               "Bark Litter P Flux",         # to be included in the new uptake
               "Seed Litter P Flux",         # to be included in the new uptake
               "Frass P Flux",               # to be included in the new uptake
               "Understorey P Flux",         # include retranslocated and new uptake flux
               "Understorey Litter P Flux",  # Proxy for new uptake 
               "Canopy Retrans P Flux",     
               "Sapwood Retrans P Flux",
               "Fineroot Retrans P Flux",
               "Coarseroot Retrans P Flux",
               "Understorey Retrans P Flux",
               #"Total vegetation production P Flux",
               #"Total vegetation retranslocation P Flux",
               #"Total vegetation uptake P Flux",
               "Mineralization P Flux 0-10cm",
               "Mineralization P Flux 10-30cm",
               "Mineralization P Flux 30-60cm",
               "Leaching P Flux")
    
    
    
    ### add treatment variable
    soil_p_mineralization$Trt <- "aCO2"
    soil_p_mineralization$Trt[soil_p_mineralization$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_p_leaching$Trt <- "aCO2"
    soil_p_leaching$Trt[soil_p_leaching$Ring%in%c(1,4,5)] <- "eCO2"
    
    canopy_p_flux$Trt <- "aCO2"
    canopy_p_flux$Trt[canopy_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    frass_p_production$Trt <- "aCO2"
    frass_p_production$Trt[frass_p_production$Ring%in%c(1,4,5)] <- "eCO2"
    
    leaflitter_p_flux$Trt <- "aCO2"
    leaflitter_p_flux$Trt[leaflitter_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_p_production$Trt <- "aCO2"
    fineroot_p_production$Trt[fineroot_p_production$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_litter_p_flux$Trt <- "aCO2"
    fineroot_litter_p_flux$Trt[fineroot_litter_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    twig_litter_p_flux$Trt <- "aCO2"
    twig_litter_p_flux$Trt[twig_litter_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    bark_litter_p_flux$Trt <- "aCO2"
    bark_litter_p_flux$Trt[bark_litter_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    seed_litter_p_flux$Trt <- "aCO2"
    seed_litter_p_flux$Trt[seed_litter_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    wood_p_flux$Trt <- "aCO2"
    wood_p_flux$Trt[wood_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    coarse_root_p_flux$Trt <- "aCO2"
    coarse_root_p_flux$Trt[coarse_root_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_p_flux$Trt <- "aCO2"
    understorey_p_flux$Trt[understorey_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_litter_p_flux$Trt <- "aCO2"
    understorey_litter_p_flux$Trt[understorey_litter_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    canopy_P_retranslocation_flux$Trt <- "aCO2"
    canopy_P_retranslocation_flux$Trt[canopy_P_retranslocation_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    sapwood_P_retranslocation_flux$Trt <- "aCO2"
    sapwood_P_retranslocation_flux$Trt[sapwood_P_retranslocation_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_P_retranslocation_flux$Trt <- "aCO2"
    understorey_P_retranslocation_flux$Trt[understorey_P_retranslocation_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_P_retranslocation_flux$Trt <- "aCO2"
    fineroot_P_retranslocation_flux$Trt[fineroot_P_retranslocation_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    coarseroot_P_retranslocation_flux$Trt <- "aCO2"
    coarseroot_P_retranslocation_flux$Trt[coarseroot_P_retranslocation_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    
    ### add factors
    canopy_p_flux$TrtFactor <- as.factor(canopy_p_flux$Trt)
    canopy_p_flux$RingFactor <- as.factor(canopy_p_flux$Ring)
    canopy_p_flux$DateFactor <- as.factor(canopy_p_flux$Date)
    canopy_p_flux$YearFactor <- as.factor(year(canopy_p_flux$Date))
    
    wood_p_flux$TrtFactor <- as.factor(wood_p_flux$Trt)
    wood_p_flux$RingFactor <- as.factor(wood_p_flux$Ring)
    wood_p_flux$DateFactor <- as.factor(wood_p_flux$Date)
    wood_p_flux$YearFactor <- as.factor(year(wood_p_flux$Date))
    
    fineroot_p_production$TrtFactor <- as.factor(fineroot_p_production$Trt)
    fineroot_p_production$RingFactor <- as.factor(fineroot_p_production$Ring)
    fineroot_p_production$DateFactor <- as.factor(fineroot_p_production$Date)
    fineroot_p_production$YearFactor <- as.factor(year(fineroot_p_production$Date))
    
    leaflitter_p_flux$TrtFactor <- as.factor(leaflitter_p_flux$Trt)
    leaflitter_p_flux$RingFactor <- as.factor(leaflitter_p_flux$Ring)
    leaflitter_p_flux$DateFactor <- as.factor(leaflitter_p_flux$Date)
    leaflitter_p_flux$YearFactor <- as.factor(year(leaflitter_p_flux$Date))
    
    twig_litter_p_flux$TrtFactor <- as.factor(twig_litter_p_flux$Trt)
    twig_litter_p_flux$RingFactor <- as.factor(twig_litter_p_flux$Ring)
    twig_litter_p_flux$DateFactor <- as.factor(twig_litter_p_flux$Date)
    twig_litter_p_flux$YearFactor <- as.factor(year(twig_litter_p_flux$Date))
    
    bark_litter_p_flux$TrtFactor <- as.factor(bark_litter_p_flux$Trt)
    bark_litter_p_flux$RingFactor <- as.factor(bark_litter_p_flux$Ring)
    bark_litter_p_flux$DateFactor <- as.factor(bark_litter_p_flux$Date)
    bark_litter_p_flux$YearFactor <- as.factor(year(bark_litter_p_flux$Date))
    
    seed_litter_p_flux$TrtFactor <- as.factor(seed_litter_p_flux$Trt)
    seed_litter_p_flux$RingFactor <- as.factor(seed_litter_p_flux$Ring)
    seed_litter_p_flux$DateFactor <- as.factor(seed_litter_p_flux$Date)
    seed_litter_p_flux$YearFactor <- as.factor(year(seed_litter_p_flux$Date))
    
    coarse_root_p_flux$TrtFactor <- as.factor(coarse_root_p_flux$Trt)
    coarse_root_p_flux$RingFactor <- as.factor(coarse_root_p_flux$Ring)
    coarse_root_p_flux$DateFactor <- as.factor(coarse_root_p_flux$Date)
    coarse_root_p_flux$YearFactor <- as.factor(year(coarse_root_p_flux$Date))
    
    understorey_p_flux$TrtFactor <- as.factor(understorey_p_flux$Trt)
    understorey_p_flux$RingFactor <- as.factor(understorey_p_flux$Ring)
    understorey_p_flux$DateFactor <- as.factor(understorey_p_flux$Date)
    understorey_p_flux$YearFactor <- as.factor(year(understorey_p_flux$Date))
    
    understorey_litter_p_flux$TrtFactor <- as.factor(understorey_litter_p_flux$Trt)
    understorey_litter_p_flux$RingFactor <- as.factor(understorey_litter_p_flux$Ring)
    understorey_litter_p_flux$DateFactor <- as.factor(understorey_litter_p_flux$Date)
    understorey_litter_p_flux$YearFactor <- as.factor(year(understorey_litter_p_flux$Date))
    
    fineroot_litter_p_flux$TrtFactor <- as.factor(fineroot_litter_p_flux$Trt)
    fineroot_litter_p_flux$RingFactor <- as.factor(fineroot_litter_p_flux$Ring)
    fineroot_litter_p_flux$DateFactor <- as.factor(fineroot_litter_p_flux$Date)
    fineroot_litter_p_flux$YearFactor <- as.factor(year(fineroot_litter_p_flux$Date))
    
    frass_p_production$TrtFactor <- as.factor(frass_p_production$Trt)
    frass_p_production$RingFactor <- as.factor(frass_p_production$Ring)
    frass_p_production$DateFactor <- as.factor(frass_p_production$Date)
    frass_p_production$YearFactor <- as.factor(year(frass_p_production$Date))
    
    canopy_P_retranslocation_flux$TrtFactor <- as.factor(canopy_P_retranslocation_flux$Trt)
    canopy_P_retranslocation_flux$RingFactor <- as.factor(canopy_P_retranslocation_flux$Ring)
    canopy_P_retranslocation_flux$DateFactor <- as.factor(canopy_P_retranslocation_flux$Date)
    canopy_P_retranslocation_flux$YearFactor <- as.factor(year(canopy_P_retranslocation_flux$Date))
    
    sapwood_P_retranslocation_flux$TrtFactor <- as.factor(sapwood_P_retranslocation_flux$Trt)
    sapwood_P_retranslocation_flux$RingFactor <- as.factor(sapwood_P_retranslocation_flux$Ring)
    sapwood_P_retranslocation_flux$DateFactor <- as.factor(sapwood_P_retranslocation_flux$Date)
    sapwood_P_retranslocation_flux$YearFactor <- as.factor(year(sapwood_P_retranslocation_flux$Date))
    
    understorey_P_retranslocation_flux$TrtFactor <- as.factor(understorey_P_retranslocation_flux$Trt)
    understorey_P_retranslocation_flux$RingFactor <- as.factor(understorey_P_retranslocation_flux$Ring)
    understorey_P_retranslocation_flux$DateFactor <- as.factor(understorey_P_retranslocation_flux$Date)
    understorey_P_retranslocation_flux$YearFactor <- as.factor(year(understorey_P_retranslocation_flux$Date))
    
    fineroot_P_retranslocation_flux$TrtFactor <- as.factor(fineroot_P_retranslocation_flux$Trt)
    fineroot_P_retranslocation_flux$RingFactor <- as.factor(fineroot_P_retranslocation_flux$Ring)
    fineroot_P_retranslocation_flux$DateFactor <- as.factor(fineroot_P_retranslocation_flux$Date)
    fineroot_P_retranslocation_flux$YearFactor <- as.factor(year(fineroot_P_retranslocation_flux$Date))
    
    coarseroot_P_retranslocation_flux$TrtFactor <- as.factor(coarseroot_P_retranslocation_flux$Trt)
    coarseroot_P_retranslocation_flux$RingFactor <- as.factor(coarseroot_P_retranslocation_flux$Ring)
    coarseroot_P_retranslocation_flux$DateFactor <- as.factor(coarseroot_P_retranslocation_flux$Date)
    coarseroot_P_retranslocation_flux$YearFactor <- as.factor(year(coarseroot_P_retranslocation_flux$Date))
    
    
    soil_p_mineralization$TrtFactor <- as.factor(soil_p_mineralization$Trt)
    soil_p_mineralization$RingFactor <- as.factor(soil_p_mineralization$Ring)
    soil_p_mineralization$DateFactor <- as.factor(soil_p_mineralization$Date)
    soil_p_mineralization$YearFactor <- as.factor(year(soil_p_mineralization$Date))
    
    soil_p_leaching$TrtFactor <- as.factor(soil_p_leaching$Trt)
    soil_p_leaching$RingFactor <- as.factor(soil_p_leaching$Ring)
    soil_p_leaching$DateFactor <- as.factor(soil_p_leaching$Date)
    soil_p_leaching$YearFactor <- as.factor(year(soil_p_leaching$Date))
    

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
    ### canopy p flux
    ### linear mixed effect model
    mod.result <- lmer(canopy_p_flux~YearFactor * TrtFactor + (1|RingFactor),data=canopy_p_flux)
    mod.result <- lmer(canopy_p_flux~DateFactor * TrtFactor + (1|RingFactor),data=canopy_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ## Check ele - amb diff
    #summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    #eff.size <- coef(modelt1)[[1]][1,2]
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Canopy P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### fineroot P Flux
    ### linear mixed effect model
    mod.result <- lmer(fineroot_p_flux_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=fineroot_p_production)
    mod.result <- lmer(fineroot_p_flux_mg_m2_d~DateFactor * TrtFactor + (1|RingFactor),data=fineroot_p_production)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Fine Root P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### fineroot litter P Flux
    ### linear mixed effect model
    mod.result <- lmer(fineroot_litter_p_flux~YearFactor * TrtFactor + (1|RingFactor),data=fineroot_litter_p_flux)
    mod.result <- lmer(fineroot_litter_p_flux~DateFactor * TrtFactor + (1|RingFactor),data=fineroot_litter_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Fineroot Litter P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### wood P Flux
    ### linear mixed effect model
    mod.result <- lmer(wood_p_flux~YearFactor * TrtFactor + (1|RingFactor),data=wood_p_flux)
    mod.result <- lmer(wood_p_flux~DateFactor * TrtFactor + (1|RingFactor),data=wood_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Wood P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Wood P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Wood P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Wood P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Wood P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Wood P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Wood P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Wood P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Wood P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Wood P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Wood P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Wood P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### coarse root P Flux
    ### linear mixed effect model
    mod.result <- lmer(coarse_root_p_flux~YearFactor * TrtFactor + (1|RingFactor),data=coarse_root_p_flux)
    mod.result <- lmer(coarse_root_p_flux~DateFactor * TrtFactor + (1|RingFactor),data=coarse_root_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Coarse Root P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### leaflitter P Flux
    ### linear mixed effect model
    mod.result <- lmer(leaflitter_p_flux_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=leaflitter_p_flux)
    mod.result <- lmer(leaflitter_p_flux_mg_m2_d~DateFactor * TrtFactor + (1|RingFactor),data=leaflitter_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Leaflitter P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### twiglitter P Flux
    ### linear mixed effect model
    mod.result <- lmer(twiglitter_p_flux_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=twig_litter_p_flux)
    mod.result <- lmer(twiglitter_p_flux_mg_m2_d~DateFactor * TrtFactor + (1|RingFactor),data=twig_litter_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Twig Litter P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    ###############################################################
    ### bark litter P Flux
    ### linear mixed effect model
    mod.result <- lmer(barklitter_p_flux_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=bark_litter_p_flux)
    mod.result <- lmer(barklitter_p_flux_mg_m2_d~DateFactor * TrtFactor + (1|RingFactor),data=bark_litter_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Bark Litter P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### seed litter P Flux
    ### linear mixed effect model
    mod.result <- lmer(seedlitter_p_flux_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=seed_litter_p_flux)
    mod.result <- lmer(seedlitter_p_flux_mg_m2_d~DateFactor * TrtFactor + (1|RingFactor),data=seed_litter_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Seed Litter P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### understorey P Flux
    ### linear mixed effect model
    mod.result <- lmer(understorey_p_flux~YearFactor * TrtFactor + (1|RingFactor),data=understorey_p_flux)
    mod.result <- lmer(understorey_p_flux~DateFactor * TrtFactor + (1|RingFactor),data=understorey_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Understorey P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    ###############################################################
    ### understorey litter P Flux
    ### linear mixed effect model
    mod.result <- lmer(understorey_litter_p_flux~YearFactor * TrtFactor + (1|RingFactor),data=understorey_litter_p_flux)
    mod.result <- lmer(understorey_litter_p_flux~DateFactor * TrtFactor + (1|RingFactor),data=understorey_litter_p_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Understorey Litter P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### frass P Flux
    ### linear mixed effect model
    mod.result <- lmer(frass_p_flux_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=frass_p_production)
    mod.result <- lmer(frass_p_flux_mg_m2_d~DateFactor * TrtFactor + (1|RingFactor),data=frass_p_production)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Frass P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Frass P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Frass P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Frass P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Frass P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Frass P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Frass P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Frass P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Frass P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Frass P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Frass P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Frass P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### canopy retrans P Flux
    ### linear mixed effect model
    mod.result <- lmer(canopy_p_retrans_flux~YearFactor * TrtFactor + (1|RingFactor),data=canopy_P_retranslocation_flux)
    mod.result <- lmer(canopy_p_retrans_flux~DateFactor * TrtFactor + (1|RingFactor),data=canopy_P_retranslocation_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Canopy Retrans P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### sapwood retrans P Flux
    ### linear mixed effect model
    mod.result <- lmer(sapwood_p_retrans_flux~YearFactor * TrtFactor + (1|RingFactor),data=sapwood_P_retranslocation_flux)
    mod.result <- lmer(sapwood_p_retrans_flux~DateFactor * TrtFactor + (1|RingFactor),data=sapwood_P_retranslocation_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Sapwood Retrans P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### fineroot retrans P Flux
    ### linear mixed effect model
    mod.result <- lmer(fineroot_p_retrans_flux~YearFactor * TrtFactor + (1|RingFactor),data=fineroot_P_retranslocation_flux)
    mod.result <- lmer(fineroot_p_retrans_flux~DateFactor * TrtFactor + (1|RingFactor),data=fineroot_P_retranslocation_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Fineroot Retrans P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### coarseroot retrans P Flux
    ### linear mixed effect model
    mod.result <- lmer(coarseroot_p_retrans_flux~YearFactor * TrtFactor + (1|RingFactor),data=coarseroot_P_retranslocation_flux)
    mod.result <- lmer(coarseroot_p_retrans_flux~DateFactor * TrtFactor + (1|RingFactor),data=coarseroot_P_retranslocation_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Coarseroot Retrans P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### understorey retrans P Flux
    ### linear mixed effect model
    mod.result <- lmer(understorey_p_retrans_flux~YearFactor * TrtFactor + (1|RingFactor),data=understorey_P_retranslocation_flux)
    mod.result <- lmer(understorey_p_retrans_flux~DateFactor * TrtFactor + (1|RingFactor),data=understorey_P_retranslocation_flux)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Understorey Retrans P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### Mineralization P Flux 0-10cm
    ### linear mixed effect model
    mod.result <- lmer(p_mineralization_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=soil_p_mineralization[soil_p_mineralization$Depth=="0_10",])
    mod.result <- lmer(p_mineralization_mg_m2_d~DateFactor * TrtFactor + (1|RingFactor),data=soil_p_mineralization[soil_p_mineralization$Depth=="0_10",])
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Mineralization P Flux 0-10cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    ###############################################################
    ### Mineralization P Flux
    ### linear mixed effect model
    #mod.result <- lmer(p_mineralization_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=soil_p_mineralization[soil_p_mineralization$Depth=="10_30",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Mineralization P Flux 10-30cm"] <- mod.anova$`Pr(>F)`[3]
    #
    #
    ################################################################
    #### Mineralization P Flux
    #### linear mixed effect model
    #mod.result <- lmer(p_mineralization_mg_m2_d~YearFactor * TrtFactor + (1|RingFactor),data=soil_p_mineralization[soil_p_mineralization$Depth=="transition",])
    #
    ### anova
    #mod.anova <- Anova(mod.result, test="F")
    #
    #### assign value to output DF
    #outDF$F_statistic_YearFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$F[1]
    #outDF$F_statistic_TrtFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$F[2]
    #outDF$F_statistic_Year_TrtFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$F[3]
    #
    #outDF$Df_YearFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$Df[1]
    #outDF$Df_TrtFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$Df[2]
    #outDF$Df_Year_TrtFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$Df[3]
    #
    #outDF$Df.res_YearFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$Df.res[1]
    #outDF$Df.res_TrtFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$Df.res[2]
    #outDF$Df.res_Year_TrtFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$Df.res[3]
    #
    #outDF$p_value_YearFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$`Pr(>F)`[1]
    #outDF$p_value_TrtFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$`Pr(>F)`[2]
    #outDF$p_value_Year_TrtFactor[outDF$variable=="Mineralization P Flux 30-60cm"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ###############################################################
    ### soil leaching P Flux
    ### linear mixed effect model
    mod.result <- lmer(phosphate_leaching_flux~YearFactor * TrtFactor + (1|RingFactor),data=soil_p_leaching)
    mod.result <- lmer(phosphate_leaching_flux~DateFactor * TrtFactor + (1|RingFactor),data=soil_p_leaching)
    
    ## anova
    mod.anova <- Anova(mod.result, test="F")
    
    ### assign value to output DF
    outDF$F_statistic_YearFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$F[1]
    outDF$F_statistic_TrtFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$F[2]
    outDF$F_statistic_Year_TrtFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$F[3]
    
    outDF$Df_YearFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$Df[1]
    outDF$Df_TrtFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$Df[2]
    outDF$Df_Year_TrtFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$Df[3]
    
    outDF$Df.res_YearFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$Df.res[1]
    outDF$Df.res_TrtFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$Df.res[2]
    outDF$Df.res_Year_TrtFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$Df.res[3]
    
    outDF$p_value_YearFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$`Pr(>F)`[1]
    outDF$p_value_TrtFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$`Pr(>F)`[2]
    outDF$p_value_Year_TrtFactor[outDF$variable=="Leaching P Flux"] <- mod.anova$`Pr(>F)`[3]
    
    
    
    ### save
    write.csv(outDF, paste0("output/summary_tables/statistics_P_flux.csv"), row.names=F)
    
    

    
}