
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information

make_conc_summary_table_bootstrap <- function(norm,
                                              canopy_p_concentration,
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
    
    treatDF <- data.frame(conc.terms)
    
    treatDF$aCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$aCO2_ci_low <- rep(NA, length(treatDF$conc.terms))
    treatDF$aCO2_ci_high <- rep(NA, length(treatDF$conc.terms))
    
    
    treatDF$diff <- rep(NA, length(treatDF$conc.terms))
    treatDF$diff_ci_low_95 <- rep(NA, length(treatDF$conc.terms))
    treatDF$diff_ci_high_95 <- rep(NA, length(treatDF$conc.terms))
    
    treatDF$diff_ci_low_85 <- rep(NA, length(treatDF$conc.terms))
    treatDF$diff_ci_high_85 <- rep(NA, length(treatDF$conc.terms))
    
    treatDF$diff_ci_low_75 <- rep(NA, length(treatDF$conc.terms))
    treatDF$diff_ci_high_75 <- rep(NA, length(treatDF$conc.terms))
    
    
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
    
    
    ### prepare temporary holding dataframe to hold the bootstrapped data
    #tmpDF <- data.frame("aCO2"=rep(NA, 1000),
    #                    "eCO2"=rep(NA, 1000))
    #
    #### fix random
    #set.seed(123)
    #
    #tmpDF$aCO2 <- resample(canopy_p_concentration$PercP[canopy_p_concentration$Ring%in%c(2,3,6)], 1000, replace=T)
    #tmpDF$eCO2 <- resample(canopy_p_concentration$PercP[canopy_p_concentration$Ring%in%c(1,4,5)], 1000, replace=T)
    
    ### basics
    library(boot)
    set.seed(123)
    
    ### prepare the relationship
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    
    ################################################################################
    ### Canopy P concentration
    
    # bootstrapping with 1000 replications 
    results <- boot(data=canopy_p_concentration, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Canopy P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Fine root P concentration
    # bootstrapping with 1000 replications 
    results <- boot(data=fineroot_p_concentration, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Fine Root P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    
    ### Leaf litter P concentration
    # bootstrapping with 1000 replications 
    results <- boot(data=leaflitter_p_concentration, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Leaflitter P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])

    
    ### Understorey P concentration
    # bootstrapping with 1000 replications 
    results <- boot(data=understorey_p_concentration, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Understorey P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    

    ### Frass P concentration
    # bootstrapping with 1000 replications 
    results <- boot(data=frass_p_concentration, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Frass P Conc"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Microbial P concentration
    # bootstrapping with 1000 replications 
    subDF <- microbial_p_concentration[microbial_p_concentration$Depth=="0_10",]
    results <- boot(data=subDF, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Microbial P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    
    ### Microbial P concentration
    # bootstrapping with 1000 replications 
    subDF <- microbial_p_concentration[microbial_p_concentration$Depth=="10_30",]
    results <- boot(data=subDF, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Microbial P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Microbial P concentration
    # bootstrapping with 1000 replications 
    subDF <- microbial_p_concentration[microbial_p_concentration$Depth=="transition",]
    results <- boot(data=subDF, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Microbial P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Soil P concentration
    # bootstrapping with 1000 replications 
    subDF <- soil_p_concentration[soil_p_concentration$Depth=="0_10",]
    results <- boot(data=subDF, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Soil P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Soil P concentration
    # bootstrapping with 1000 replications 
    subDF <- soil_p_concentration[soil_p_concentration$Depth=="10_30",]
    results <- boot(data=subDF, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Soil P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    
    
    
    
    
    
    ### Soil Phosphate P concentration
    # bootstrapping with 1000 replications 
    subDF <- soil_phosphate_concentration[soil_phosphate_concentration$Depth=="0_10",]
    results <- boot(data=subDF, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Soil Phosphate P Conc 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Soil Phosphate P concentration
    # bootstrapping with 1000 replications 
    subDF <- soil_phosphate_concentration[soil_phosphate_concentration$Depth=="10_30",]
    results <- boot(data=subDF, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Soil Phosphate P Conc 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Soil Phosphate P concentration
    # bootstrapping with 1000 replications 
    subDF <- soil_phosphate_concentration[soil_phosphate_concentration$Depth=="transition",]
    results <- boot(data=subDF, statistic=bs, 
                    R=1000, formula=PercP~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)[2])
    treatDF$aCO2_ci_low[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$conc.terms == "Soil Phosphate P Conc 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### save
    write.csv(treatDF, paste0("output/summary_tables/summary_table_P_concentration_", norm, 
                              ".csv"), row.names=F)
    
    
   
    
    ##### output tables
    return(treatDF)
      
}

