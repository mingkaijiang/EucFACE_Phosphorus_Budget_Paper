make_cp_ratios_bootstrap <- function(norm, 
                           soil_p_pool,
                           microbial_p_pool,
                           canopy_p_pool,
                           leaflitter_p_pool,
                           wood_p_pool,
                           sapwood_p_pool,
                           heartwood_p_pool,
                           standing_dead_p_pool,
                           fineroot_p_pool,
                           understorey_p_pool,
                           coarse_root_p_pool,
                           canopy_c_pool,
                           wood_c_pool,
                           standing_dead_c_pool,
                           fineroot_c_pool,
                           coarse_root_c_pool,
                           understorey_c_pool,
                           soil_c_pool,
                           microbial_c_pool,
                           mycorrhizal_c_pool,
                           leaflitter_c_pool,
                           understorey_litter_c_flux,
                           understorey_litter_p_flux,
                           frass_c_production_flux,
                           frass_p_production) {
    
    
    ### Compute CP ratio for major pools
    terms <- c("canopy", 
               "leaflitter", 
               "wood",
               "sapwood", 
               "heartwood",
               "fineroot", 
               "understorey", 
               "understorey_litter", 
               "frass", 
               "soil",
               "soil_0_10",
               "soil_10_30",
               "soil_30_60",
               "microbe",
               "microbe_0_10",
               "microbe_10_30",
               "microbe_30_60")
    
    
    ### prepare dataframe
    treatDF <- data.frame(terms)
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$aCO2_ci_low <- rep(NA, length(treatDF$terms))
    treatDF$aCO2_ci_high <- rep(NA, length(treatDF$terms))
    
    
    treatDF$diff <- rep(NA, length(treatDF$terms))
    treatDF$diff_ci_low_95 <- rep(NA, length(treatDF$terms))
    treatDF$diff_ci_high_95 <- rep(NA, length(treatDF$terms))
    
    treatDF$diff_ci_low_85 <- rep(NA, length(treatDF$terms))
    treatDF$diff_ci_high_85 <- rep(NA, length(treatDF$terms))
    
    treatDF$diff_ci_low_75 <- rep(NA, length(treatDF$terms))
    treatDF$diff_ci_high_75 <- rep(NA, length(treatDF$terms))
    
    
    ### add treatment variable
    soil_p_pool$Trt <- "aCO2"
    soil_p_pool$Trt[soil_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    microbial_p_pool$Trt <- "aCO2"
    microbial_p_pool$Trt[microbial_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    canopy_p_pool$Trt <- "aCO2"
    canopy_p_pool$Trt[canopy_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    leaflitter_p_pool$Trt <- "aCO2"
    leaflitter_p_pool$Trt[leaflitter_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    wood_p_pool$Trt <- "aCO2"
    wood_p_pool$Trt[wood_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_p_pool$Trt <- "aCO2"
    fineroot_p_pool$Trt[fineroot_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_p_pool$Trt <- "aCO2"
    understorey_p_pool$Trt[understorey_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    coarse_root_p_pool$Trt <- "aCO2"
    coarse_root_p_pool$Trt[coarse_root_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    canopy_c_pool$Trt <- "aCO2"
    canopy_c_pool$Trt[canopy_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    wood_c_pool$Trt <- "aCO2"
    wood_c_pool$Trt[wood_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_c_pool$Trt <- "aCO2"
    fineroot_c_pool$Trt[fineroot_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    coarse_root_c_pool$Trt <- "aCO2"
    coarse_root_c_pool$Trt[coarse_root_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_c_pool$Trt <- "aCO2"
    understorey_c_pool$Trt[understorey_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    soil_c_pool$Trt <- "aCO2"
    soil_c_pool$Trt[soil_c_pool$Ring%in%c(1,4,5)] <- "eCO2"

    microbial_c_pool$Trt <- "aCO2"
    microbial_c_pool$Trt[microbial_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    leaflitter_c_pool$Trt <- "aCO2"
    leaflitter_c_pool$Trt[leaflitter_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_litter_c_flux$Trt <- "aCO2"
    understorey_litter_c_flux$Trt[understorey_litter_c_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_litter_p_flux$Trt <- "aCO2"
    understorey_litter_p_flux$Trt[understorey_litter_p_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    frass_c_production_flux$Trt <- "aCO2"
    frass_c_production_flux$Trt[frass_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    frass_p_production$Trt <- "aCO2"
    frass_p_production$Trt[frass_p_production$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### basics
    library(boot)
    set.seed(123)
    
    ### prepare the relationship
    bs <- function(formula, data, indices) {
        d <- data[indices,] # allows boot to select sample 
        fit <- lm(formula, data=d)
        return(coef(fit)) 
    } 
    
    
    ###  Canopy CP
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(canopy_c_pool$leaf_pool[canopy_c_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(canopy_c_pool$leaf_pool[canopy_c_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(canopy_p_pool$leaf_p_pool[canopy_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(canopy_p_pool$leaf_p_pool[canopy_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)

    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)

    # assign values 
    treatDF$aCO2[treatDF$terms == "canopy"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "canopy"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "canopy"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "canopy"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "canopy"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "canopy"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "canopy"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "canopy"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "canopy"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "canopy"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
 
    ###  Wood CP
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(wood_c_pool$wood_pool[wood_c_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(wood_c_pool$wood_pool[wood_c_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(wood_p_pool$wood_p_pool[wood_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(wood_p_pool$wood_p_pool[wood_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "wood"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "wood"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "wood"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "wood"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "wood"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "wood"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "wood"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "wood"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "wood"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "wood"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Heartwood CP
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(wood_c_pool$sap_pool[wood_c_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(wood_c_pool$sap_pool[wood_c_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(sapwood_p_pool$wood_p_pool[sapwood_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(sapwood_p_pool$wood_p_pool[sapwood_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "sapwood"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "sapwood"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "sapwood"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "sapwood"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "sapwood"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "sapwood"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "sapwood"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "sapwood"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "sapwood"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "sapwood"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Heartwood CP
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(wood_c_pool$heart_pool[wood_c_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(wood_c_pool$heart_pool[wood_c_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(heartwood_p_pool$wood_p_pool[heartwood_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(heartwood_p_pool$wood_p_pool[heartwood_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "heartwood"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "heartwood"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "heartwood"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "heartwood"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "heartwood"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "heartwood"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "heartwood"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "heartwood"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "heartwood"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "heartwood"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Fineroot CP
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(fineroot_c_pool$fineroot_pool[fineroot_c_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(fineroot_c_pool$fineroot_pool[fineroot_c_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(fineroot_p_pool$fineroot_p_pool[fineroot_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(fineroot_p_pool$fineroot_p_pool[fineroot_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "fineroot"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "fineroot"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "fineroot"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "fineroot"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "fineroot"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "fineroot"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "fineroot"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "fineroot"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "fineroot"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "fineroot"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Coarseroot CP
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(coarse_root_c_pool$coarse_root_pool[coarse_root_c_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(coarse_root_c_pool$coarse_root_pool[coarse_root_c_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(coarse_root_p_pool$coarse_root_p_pool[coarse_root_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(coarse_root_p_pool$coarse_root_p_pool[coarse_root_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "coarseroot"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "coarseroot"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "coarseroot"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "coarseroot"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "coarseroot"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "coarseroot"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "coarseroot"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "coarseroot"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "coarseroot"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "coarseroot"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Understorey CP
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(understorey_c_pool$Total_g_C_m2[understorey_c_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(understorey_c_pool$Total_g_C_m2[understorey_c_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(understorey_p_pool$understorey_p_pool[understorey_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(understorey_p_pool$understorey_p_pool[understorey_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "understorey"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "understorey"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "understorey"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "understorey"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "understorey"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "understorey"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "understorey"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "understorey"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "understorey"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "understorey"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Leaflitter CP
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(leaflitter_c_pool$leaflitter_pool[leaflitter_c_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(leaflitter_c_pool$leaflitter_pool[leaflitter_c_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(leaflitter_p_pool$leaflitter_p_pool[leaflitter_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(leaflitter_p_pool$leaflitter_p_pool[leaflitter_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "leaflitter"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "leaflitter"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "leaflitter"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "leaflitter"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "leaflitter"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "leaflitter"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "leaflitter"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "leaflitter"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "leaflitter"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "leaflitter"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    
    ###  Soil CP 
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    totDF1 <- summaryBy(soil_carbon_pool~Date+Ring+Trt, FUN=sum, data=soil_c_pool,
                        na.rm=T, keep.names=T)
    totDF2 <- summaryBy(soil_p_g_m2~Date+Ring+Trt, FUN=sum, data=soil_p_pool,
                        na.rm=T, keep.names=T)
    
    tmpDF1$Cpool <- resample(totDF1$soil_carbon_pool[totDF1$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(totDF1$soil_carbon_pool[totDF1$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(totDF2$soil_p_g_m2[totDF2$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(totDF2$soil_p_g_m2[totDF2$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "soil"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "soil"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "soil"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "soil"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "soil"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "soil"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "soil"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "soil"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "soil"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "soil"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    
    ###  Soil CP 0-10cm
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(soil_c_pool$soil_carbon_pool[soil_c_pool$Ring%in%c(2,3,6)&soil_c_pool$Depth=="0_10"], 1000, replace=T)
    tmpDF2$Cpool <- resample(soil_c_pool$soil_carbon_pool[soil_c_pool$Ring%in%c(1,4,5)&soil_c_pool$Depth=="0_10"], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(soil_p_pool$soil_p_g_m2[soil_p_pool$Ring%in%c(2,3,6)&soil_p_pool$Depth=="0_10"], 1000, replace=T)
    tmpDF2$Ppool <- resample(soil_p_pool$soil_p_g_m2[soil_p_pool$Ring%in%c(1,4,5)&soil_p_pool$Depth=="0_10"], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "soil_0_10"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "soil_0_10"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "soil_0_10"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "soil_0_10"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "soil_0_10"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "soil_0_10"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "soil_0_10"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "soil_0_10"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "soil_0_10"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "soil_0_10"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    
    ###  Soil CP 10-30
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(soil_c_pool$soil_carbon_pool[soil_c_pool$Ring%in%c(2,3,6)&soil_c_pool$Depth=="10_30"], 1000, replace=T)
    tmpDF2$Cpool <- resample(soil_c_pool$soil_carbon_pool[soil_c_pool$Ring%in%c(1,4,5)&soil_c_pool$Depth=="10_30"], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(soil_p_pool$soil_p_g_m2[soil_p_pool$Ring%in%c(2,3,6)&soil_p_pool$Depth=="10_30"], 1000, replace=T)
    tmpDF2$Ppool <- resample(soil_p_pool$soil_p_g_m2[soil_p_pool$Ring%in%c(1,4,5)&soil_p_pool$Depth=="10_30"], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "soil_10_30"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "soil_10_30"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "soil_10_30"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "soil_10_30"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "soil_10_30"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "soil_10_30"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "soil_10_30"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "soil_10_30"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "soil_10_30"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "soil_10_30"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Soil CP 30-60
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(soil_c_pool$soil_carbon_pool[soil_c_pool$Ring%in%c(2,3,6)&soil_c_pool$Depth=="transition"], 1000, replace=T)
    tmpDF2$Cpool <- resample(soil_c_pool$soil_carbon_pool[soil_c_pool$Ring%in%c(1,4,5)&soil_c_pool$Depth=="transition"], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(soil_p_pool$soil_p_g_m2[soil_p_pool$Ring%in%c(2,3,6)&soil_p_pool$Depth=="transition"], 1000, replace=T)
    tmpDF2$Ppool <- resample(soil_p_pool$soil_p_g_m2[soil_p_pool$Ring%in%c(1,4,5)&soil_p_pool$Depth=="transition"], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "soil_30_60"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "soil_30_60"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "soil_30_60"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "soil_30_60"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "soil_30_60"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "soil_30_60"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "soil_30_60"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "soil_30_60"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "soil_30_60"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "soil_30_60"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Microbial CP 
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    totDF1 <- summaryBy(Cmic_g_m2~Date+Ring+Trt, FUN=sum, data=microbial_c_pool,
                        na.rm=T, keep.names=T)
    totDF2 <- summaryBy(microbial_p_g_m2~Date+Ring+Trt, FUN=sum, data=microbial_p_pool,
                        na.rm=T, keep.names=T)
    
    tmpDF1$Cpool <- resample(totDF1$Cmic_g_m2[totDF1$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(totDF1$Cmic_g_m2[totDF1$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(totDF2$microbial_p_g_m2[totDF2$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(totDF2$microbial_p_g_m2[totDF2$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "microbe"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "microbe"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "microbe"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "microbe"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "microbe"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "microbe"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "microbe"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "microbe"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "microbe"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "microbe"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    
    ###  Microbial CP 0-10
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(microbial_c_pool$Cmic_g_m2[microbial_c_pool$Ring%in%c(2,3,6)&microbial_c_pool$Depth=="0_10"], 1000, replace=T)
    tmpDF2$Cpool <- resample(microbial_c_pool$Cmic_g_m2[microbial_c_pool$Ring%in%c(1,4,5)&microbial_c_pool$Depth=="0_10"], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(microbial_p_pool$microbial_p_g_m2[microbial_p_pool$Ring%in%c(2,3,6)&microbial_p_pool$Depth=="0_10"], 1000, replace=T)
    tmpDF2$Ppool <- resample(microbial_p_pool$microbial_p_g_m2[microbial_p_pool$Ring%in%c(1,4,5)&microbial_p_pool$Depth=="0_10"], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "microbe_0_10"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "microbe_0_10"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "microbe_0_10"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "microbe_0_10"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "microbe_0_10"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "microbe_0_10"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "microbe_0_10"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "microbe_0_10"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "microbe_0_10"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "microbe_0_10"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Microbial CP 10-30
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(microbial_c_pool$Cmic_g_m2[microbial_c_pool$Ring%in%c(2,3,6)&microbial_c_pool$Depth=="10_30"], 1000, replace=T)
    tmpDF2$Cpool <- resample(microbial_c_pool$Cmic_g_m2[microbial_c_pool$Ring%in%c(1,4,5)&microbial_c_pool$Depth=="10_30"], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(microbial_p_pool$microbial_p_g_m2[microbial_p_pool$Ring%in%c(2,3,6)&microbial_p_pool$Depth=="10_30"], 1000, replace=T)
    tmpDF2$Ppool <- resample(microbial_p_pool$microbial_p_g_m2[microbial_p_pool$Ring%in%c(1,4,5)&microbial_p_pool$Depth=="10_30"], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "microbe_10_30"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "microbe_10_30"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "microbe_10_30"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "microbe_10_30"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "microbe_10_30"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "microbe_10_30"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "microbe_10_30"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "microbe_10_30"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "microbe_10_30"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "microbe_10_30"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  Microbial CP 30-60
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(microbial_c_pool$Cmic_g_m2[microbial_c_pool$Ring%in%c(2,3,6)&microbial_c_pool$Depth=="transition"], 1000, replace=T)
    tmpDF2$Cpool <- resample(microbial_c_pool$Cmic_g_m2[microbial_c_pool$Ring%in%c(1,4,5)&microbial_c_pool$Depth=="transition"], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(microbial_p_pool$microbial_p_g_m2[microbial_p_pool$Ring%in%c(2,3,6)&microbial_p_pool$Depth=="transition"], 1000, replace=T)
    tmpDF2$Ppool <- resample(microbial_p_pool$microbial_p_g_m2[microbial_p_pool$Ring%in%c(1,4,5)&microbial_p_pool$Depth=="transition"], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "microbe_30_60"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "microbe_30_60"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "microbe_30_60"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "microbe_30_60"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "microbe_30_60"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "microbe_30_60"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "microbe_30_60"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "microbe_30_60"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "microbe_30_60"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "microbe_30_60"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  understorey litter
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(understorey_litter_c_flux$understorey_litter_flux[understorey_litter_c_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(understorey_litter_c_flux$understorey_litter_flux[understorey_litter_c_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(understorey_litter_p_flux$understorey_litter_p_flux[understorey_litter_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(understorey_litter_p_flux$understorey_litter_p_flux[understorey_litter_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "understorey_litter"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    
    treatDF$aCO2_ci_low[treatDF$terms == "understorey_litter"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "understorey_litter"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "understorey_litter"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "understorey_litter"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "understorey_litter"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "understorey_litter"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "understorey_litter"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "understorey_litter"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "understorey_litter"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ###  frass
    tmpDF1 <- data.frame("Cpool"=rep(NA, 1000),
                         "Ppool"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Cpool <- resample(frass_c_production_flux$frass_production_flux[frass_c_production_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Cpool <- resample(frass_c_production_flux$frass_production_flux[frass_c_production_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Ppool <- resample(frass_p_production$frass_p_flux_mg_m2_d[frass_p_production$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Ppool <- resample(frass_p_production$frass_p_flux_mg_m2_d[frass_p_production$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$CPratio <- with(tmpDF1, Cpool/Ppool)
    tmpDF2$CPratio <- with(tmpDF2, Cpool/Ppool)
    
    tmpDF3 <- data.frame("aCO2"=tmpDF1$CPratio,
                         "eCO2"=tmpDF2$CPratio)
    tmpDF3$diff <- with(tmpDF3, eCO2-aCO2)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "frass"] <- mean(c(tmpDF3$aCO2, tmpDF3$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "frass"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "frass"] <- t.test(c(tmpDF3$aCO2, tmpDF3$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "frass"] <- mean(tmpDF3$diff)
    
    treatDF$diff_ci_low_95[treatDF$terms == "frass"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "frass"] <- t.test(tmpDF3$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "frass"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "frass"] <- t.test(tmpDF3$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "frass"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "frass"] <- t.test(tmpDF3$diff, conf.level=0.75)$conf.int[2]
    
    
    ### output
    write.csv(treatDF, paste0("output/summary_tables/summary_cp_ratios_bootstrap.csv"),
              row.names=F)
    
    
    return(treatDF)
    
}
