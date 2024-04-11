make_total_p_budget_bootstrap <- function(norm, 
                                          canopy_p_pool,
                                          wood_p_pool,
                                          sapwood_p_pool,
                                          fineroot_p_pool,
                                          coarse_root_p_pool,
                                          understorey_p_pool,
                                          standing_dead_p_pool,
                                          leaflitter_p_pool,
                                          soil_p_mineralization,
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
                                          coarseroot_P_retranslocation_flux,
                                          canopy_c_production_flux,
                                          wood_c_production,
                                          fineroot_c_production_flux,
                                          coarse_root_c_flux,
                                          understorey_c_flux_clipping,
                                          frass_c_production_flux,
                                          twiglitter_c_production_flux,
                                          barklitter_c_production_flux,
                                          seedlitter_c_production_flux) {
    
    
    ### Compute major terms
    terms <- c("Total plant P stock", 
               "Total plant P requirement flux", 
               "Total plant P retranslocation flux", 
               "Plant P uptake flux", 
               "Plant P MRT", 
               "Plant PUE",
               "plant_GPP_efficiency", 
               "Overstorey GPP efficiency",
               "Understorey GPP efficiency")
    
    
    ### prepare dataframe
    treatDF <- data.frame("terms"=terms,
                          "aCO2"=NA,
                          "aCO2_ci_low"=NA,
                          "aCO2_ci_high"=NA,
                          "diff"=NA,
                          "diff_ci_low_95"=NA,
                          "diff_ci_high_95"=NA,
                          "diff_ci_low_85"=NA,
                          "diff_ci_high_85"=NA,
                          "diff_ci_low_75"=NA,
                          "diff_ci_high_75"=NA)
    
    
    ### add treatment variable
    canopy_p_pool$Trt <- "aCO2"
    canopy_p_pool$Trt[canopy_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    wood_p_pool$Trt <- "aCO2"
    wood_p_pool$Trt[wood_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    sapwood_p_pool$Trt <- "aCO2"
    sapwood_p_pool$Trt[sapwood_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_p_pool$Trt <- "aCO2"
    fineroot_p_pool$Trt[fineroot_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    coarse_root_p_pool$Trt <- "aCO2"
    coarse_root_p_pool$Trt[coarse_root_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_p_pool$Trt <- "aCO2"
    understorey_p_pool$Trt[understorey_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    standing_dead_p_pool$Trt <- "aCO2"
    standing_dead_p_pool$Trt[standing_dead_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    leaflitter_p_pool$Trt <- "aCO2"
    leaflitter_p_pool$Trt[leaflitter_p_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
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
    
    
    canopy_c_production_flux$Trt <- "aCO2"
    canopy_c_production_flux$Trt[canopy_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    wood_c_production$Trt <- "aCO2"
    wood_c_production$Trt[wood_c_production$Ring%in%c(1,4,5)] <- "eCO2"
    
    fineroot_c_production_flux$Trt <- "aCO2"
    fineroot_c_production_flux$Trt[fineroot_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    coarse_root_c_flux$Trt <- "aCO2"
    coarse_root_c_flux$Trt[coarse_root_c_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    understorey_c_flux_clipping$Trt <- "aCO2"
    understorey_c_flux_clipping$Trt[understorey_c_flux_clipping$Ring%in%c(1,4,5)] <- "eCO2"
    
    frass_c_production_flux$Trt <- "aCO2"
    frass_c_production_flux$Trt[frass_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    twiglitter_c_production_flux$Trt <- "aCO2"
    twiglitter_c_production_flux$Trt[twiglitter_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    barklitter_c_production_flux$Trt <- "aCO2"
    barklitter_c_production_flux$Trt[barklitter_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    seedlitter_c_production_flux$Trt <- "aCO2"
    seedlitter_c_production_flux$Trt[seedlitter_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### basics
    library(boot)
    set.seed(123)
    
    ### prepare the relationship
    bs <- function(formula, data, indices) {
        d <- data[indices,] # allows boot to select sample 
        fit <- lm(formula, data=d)
        return(coef(fit)) 
    } 
    
    conv <- 365 / 1000
    
    ###  Total vegetation production P flux
    tmpDF1 <- data.frame("Canopy"=rep(NA, 1000),
                         "Wood"=rep(NA, 1000),
                         "Fineroot"=rep(NA, 1000),
                         "Coarseroot"=rep(NA, 1000),
                         "Twig"=rep(NA, 1000),
                         "Bark"=rep(NA, 1000),
                         "Seed"=rep(NA, 1000),
                         "Understorey"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Canopy <- resample(canopy_p_flux$canopy_p_flux[canopy_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Canopy <- resample(canopy_p_flux$canopy_p_flux[canopy_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Wood <- resample(wood_p_flux$wood_p_flux[wood_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Wood <- resample(wood_p_flux$wood_p_flux[wood_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Fineroot <- resample(fineroot_p_production$fineroot_p_flux_mg_m2_d[fineroot_p_production$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Fineroot <- resample(fineroot_p_production$fineroot_p_flux_mg_m2_d[fineroot_p_production$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Coarseroot <- resample(coarse_root_p_flux$coarse_root_p_flux[coarse_root_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Coarseroot <- resample(coarse_root_p_flux$coarse_root_p_flux[coarse_root_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Twig <- resample(twig_litter_p_flux$twiglitter_p_flux_mg_m2_d[twig_litter_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Twig <- resample(twig_litter_p_flux$twiglitter_p_flux_mg_m2_d[twig_litter_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Bark <- resample(bark_litter_p_flux$barklitter_p_flux_mg_m2_d[bark_litter_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Bark <- resample(bark_litter_p_flux$barklitter_p_flux_mg_m2_d[bark_litter_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Seed <- resample(seed_litter_p_flux$seedlitter_p_flux_mg_m2_d[seed_litter_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Seed <- resample(seed_litter_p_flux$seedlitter_p_flux_mg_m2_d[seed_litter_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Understorey <- resample(understorey_p_flux$understorey_p_flux[understorey_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Understorey <- resample(understorey_p_flux$understorey_p_flux[understorey_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF3 <- rowSums(tmpDF1, na.rm=T)
    tmpDF4 <- rowSums(tmpDF2, na.rm=T)
    tmpDF5 <- data.frame("aCO2"=tmpDF3,
                         "eCO2"=tmpDF4)
    tmpDF5$diff <- with(tmpDF5, eCO2-aCO2)    
    prodDF <- tmpDF5
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Total plant P requirement flux"] <- mean(c(tmpDF3, tmpDF4)) * conv
    
    t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1]
    t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2]
    
    treatDF$aCO2_ci_low[treatDF$terms == "Total plant P requirement flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1] * conv
    treatDF$aCO2_ci_high[treatDF$terms == "Total plant P requirement flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2] * conv 
    
    treatDF$diff[treatDF$terms == "Total plant P requirement flux"] <- mean(tmpDF5$diff) * conv
    
    
    treatDF$diff_ci_low_95[treatDF$terms == "Total plant P requirement flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[1] * conv
    treatDF$diff_ci_high_95[treatDF$terms == "Total plant P requirement flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[2] * conv
    
    treatDF$diff_ci_low_85[treatDF$terms == "Total plant P requirement flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[1] * conv
    treatDF$diff_ci_high_85[treatDF$terms == "Total plant P requirement flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[2] * conv
    
    treatDF$diff_ci_low_75[treatDF$terms == "Total plant P requirement flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[1] * conv
    treatDF$diff_ci_high_75[treatDF$terms == "Total plant P requirement flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[2] * conv
    
    
    ###  Total retranslocation flux
    tmpDF1 <- data.frame("Canopy"=rep(NA, 1000),
                         "Wood"=rep(NA, 1000),
                         "Fineroot"=rep(NA, 1000),
                         "Coarseroot"=rep(NA, 1000),
                         "Understorey"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$Canopy <- resample(canopy_P_retranslocation_flux$canopy_p_retrans_flux[canopy_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Canopy <- resample(canopy_P_retranslocation_flux$canopy_p_retrans_flux[canopy_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Wood <- resample(sapwood_P_retranslocation_flux$sapwood_p_retrans_flux[sapwood_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Wood <- resample(sapwood_P_retranslocation_flux$sapwood_p_retrans_flux[sapwood_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Fineroot <- resample(fineroot_P_retranslocation_flux$fineroot_p_retrans_flux[fineroot_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Fineroot <- resample(fineroot_P_retranslocation_flux$fineroot_p_retrans_flux[fineroot_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Coarseroot <- resample(coarseroot_P_retranslocation_flux$coarseroot_p_retrans_flux[coarseroot_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Coarseroot <- resample(coarseroot_P_retranslocation_flux$coarseroot_p_retrans_flux[coarseroot_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$Understorey <- resample(understorey_P_retranslocation_flux$understorey_p_retrans_flux[understorey_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$Understorey <- resample(understorey_P_retranslocation_flux$understorey_p_retrans_flux[understorey_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF3 <- rowSums(tmpDF1, na.rm=T)
    tmpDF4 <- rowSums(tmpDF2, na.rm=T)
    tmpDF5 <- data.frame("aCO2"=tmpDF3,
                         "eCO2"=tmpDF4)
    tmpDF5$diff <- with(tmpDF5, eCO2-aCO2)
    retranDF <- tmpDF5
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Total plant P retranslocation flux"] <- mean(c(tmpDF3, tmpDF4)) * conv
    
    treatDF$aCO2_ci_low[treatDF$terms == "Total plant P retranslocation flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1] * conv
    treatDF$aCO2_ci_high[treatDF$terms == "Total plant P retranslocation flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2] * conv 
    
    treatDF$diff[treatDF$terms == "Total plant P retranslocation flux"] <- mean(tmpDF5$diff) * conv
    
    
    treatDF$diff_ci_low_95[treatDF$terms == "Total plant P retranslocation flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[1] * conv
    treatDF$diff_ci_high_95[treatDF$terms == "Total plant P retranslocation flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[2] * conv
    
    treatDF$diff_ci_low_85[treatDF$terms == "Total plant P retranslocation flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[1] * conv
    treatDF$diff_ci_high_85[treatDF$terms == "Total plant P retranslocation flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[2] * conv
    
    treatDF$diff_ci_low_75[treatDF$terms == "Total plant P retranslocation flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[1] * conv
    treatDF$diff_ci_high_75[treatDF$terms == "Total plant P retranslocation flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[2] * conv
    
    
    
    ###  Total vegetation uptake P flux
    tmpDF3 <- prodDF$aCO2 - retranDF$aCO2
    tmpDF4 <- prodDF$eCO2 - retranDF$eCO2
    
    tmpDF5 <- data.frame("aCO2"=tmpDF3,
                         "eCO2"=tmpDF4)
    tmpDF5$diff <- with(tmpDF5, eCO2-aCO2) 
    puptDF <- tmpDF5
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Plant P uptake flux"] <- mean(c(tmpDF3, tmpDF4)) * conv
    
    t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1]
    t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2]
    
    treatDF$aCO2_ci_low[treatDF$terms == "Plant P uptake flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1] * conv
    treatDF$aCO2_ci_high[treatDF$terms == "Plant P uptake flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2] * conv 
    
    treatDF$diff[treatDF$terms == "Plant P uptake flux"] <- mean(tmpDF5$diff) * conv
    
    
    treatDF$diff_ci_low_95[treatDF$terms == "Plant P uptake flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[1] * conv
    treatDF$diff_ci_high_95[treatDF$terms == "Plant P uptake flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[2] * conv
    
    treatDF$diff_ci_low_85[treatDF$terms == "Plant P uptake flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[1] * conv
    treatDF$diff_ci_high_85[treatDF$terms == "Plant P uptake flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[2] * conv
    
    treatDF$diff_ci_low_75[treatDF$terms == "Plant P uptake flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[1] * conv
    treatDF$diff_ci_high_75[treatDF$terms == "Plant P uptake flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[2] * conv
    
    
    ### Total vegetation P stock
    ###  prepare storage df
    tmpDF1 <- data.frame("canopy"=rep(NA, 1000),
                         "wood"=rep(NA, 1000),
                         "fineroot"=rep(NA, 1000),
                         "coarseroot"=rep(NA, 1000),
                         "understorey"=rep(NA, 1000),
                         "sapwood"=rep(NA, 1000),
                         "leaflitter"=rep(NA, 1000),
                         "standing_dead"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$canopy <- resample(canopy_p_pool$leaf_p_pool[canopy_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$canopy <- resample(canopy_p_pool$leaf_p_pool[canopy_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$wood <- resample(wood_p_pool$wood_p_pool[wood_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$wood <- resample(wood_p_pool$wood_p_pool[wood_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$fineroot <- resample(fineroot_p_pool$fineroot_p_pool[fineroot_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$fineroot <- resample(fineroot_p_pool$fineroot_p_pool[fineroot_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$coarseroot <- resample(coarse_root_p_pool$coarse_root_p_pool[coarse_root_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$coarseroot <- resample(coarse_root_p_pool$coarse_root_p_pool[coarse_root_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$understorey <- resample(understorey_p_pool$understorey_p_pool[understorey_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$understorey <- resample(understorey_p_pool$understorey_p_pool[understorey_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$leaflitter <- resample(leaflitter_p_pool$leaflitter_p_pool[leaflitter_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$leaflitter <- resample(leaflitter_p_pool$leaflitter_p_pool[leaflitter_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$standing_dead <- resample(standing_dead_p_pool$wood_p_pool[standing_dead_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$standing_dead <- resample(standing_dead_p_pool$wood_p_pool[standing_dead_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    

    tmpDF3 <- rowSums(tmpDF1, na.rm=T)
    tmpDF4 <- rowSums(tmpDF2, na.rm=T)
    tmpDF5 <- data.frame("aCO2"=tmpDF3,
                         "eCO2"=tmpDF4)
    tmpDF5$diff <- with(tmpDF5, eCO2-aCO2)
    
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Total plant P stock"]  <- mean(c(tmpDF3, tmpDF4))
    
    treatDF$aCO2_ci_low[treatDF$terms == "Total plant P stock"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "Total plant P stock"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "Total plant P stock"] <- mean(tmpDF5$diff) 
    
    treatDF$diff_ci_low_95[treatDF$terms == "Total plant P stock"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "Total plant P stock"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "Total plant P stock"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "Total plant P stock"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "Total plant P stock"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "Total plant P stock"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[2]
    
    
    
    ### plant_P_mean_residence_time
    ###  prepare storage df
    tmpDF1 <- data.frame("canopy"=rep(NA, 1000),
                         "fineroot"=rep(NA, 1000),
                         "understorey"=rep(NA, 1000),
                         "sapwood"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$canopy <- resample(canopy_p_pool$leaf_p_pool[canopy_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$canopy <- resample(canopy_p_pool$leaf_p_pool[canopy_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$sapwood <- resample(sapwood_p_pool$wood_p_pool[sapwood_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$sapwood <- resample(sapwood_p_pool$wood_p_pool[sapwood_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$fineroot <- resample(fineroot_p_pool$fineroot_p_pool[fineroot_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$fineroot <- resample(fineroot_p_pool$fineroot_p_pool[fineroot_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$understorey <- resample(understorey_p_pool$understorey_p_pool[understorey_p_pool$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$understorey <- resample(understorey_p_pool$understorey_p_pool[understorey_p_pool$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF3 <- rowSums(tmpDF1, na.rm=T) 
    tmpDF4 <- rowSums(tmpDF2, na.rm=T)
    
    tmpDF5 <- data.frame("veg_stock"=tmpDF3,
                         "veg_p_uptake"=puptDF$aCO2 * conv)
    
    tmpDF6 <- data.frame("veg_stock"=tmpDF4,
                         "veg_p_uptake"=puptDF$eCO2 * conv)
    
    tmpDF5$P_MRT <- with(tmpDF5, veg_stock/veg_p_uptake)
    tmpDF6$P_MRT <- with(tmpDF6, veg_stock/veg_p_uptake)
    
    
    tmpDF7 <- data.frame("aCO2"=tmpDF5$P_MRT,
                         "eCO2"=tmpDF6$P_MRT)
    tmpDF7$diff <- with(tmpDF7, eCO2-aCO2)
    
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Plant P MRT"] <- mean(c(tmpDF7$aCO2, tmpDF7$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "Plant P MRT"] <- t.test(c(tmpDF7$aCO2, tmpDF7$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "Plant P MRT"] <- t.test(c(tmpDF7$aCO2, tmpDF7$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "Plant P MRT"] <- mean(tmpDF7$diff) 
    
    treatDF$diff_ci_low_95[treatDF$terms == "Plant P MRT"] <- t.test(tmpDF7$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "Plant P MRT"] <- t.test(tmpDF7$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "Plant P MRT"] <- t.test(tmpDF7$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "Plant P MRT"] <- t.test(tmpDF7$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "Plant P MRT"] <- t.test(tmpDF7$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "Plant P MRT"] <- t.test(tmpDF7$diff, conf.level=0.75)$conf.int[2]
    
    
    
    #### Plant PUE
    
    ###  prepare storage df
    tmpDF1 <- data.frame("canopy"=rep(NA, 1000),
                         "fineroot"=rep(NA, 1000),
                         "coarseroot"=rep(NA, 1000),
                         "frass"=rep(NA, 1000),
                         "twig"=rep(NA, 1000),
                         "bark"=rep(NA, 1000),
                         "seed"=rep(NA, 1000),
                         "understorey"=rep(NA, 1000),
                         "wood"=rep(NA, 1000))
    tmpDF2 <- tmpDF1
    
    
    ### change of method to estimate aggregated fluxes
    tmpDF1$canopy <- resample(canopy_c_production_flux$leaf_flux[canopy_c_production_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$canopy <- resample(canopy_c_production_flux$leaf_flux[canopy_c_production_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$sapwood <- resample(wood_c_production$wood_production_flux[wood_c_production$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$sapwood <- resample(wood_c_production$wood_production_flux[wood_c_production$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$fineroot <- resample(fineroot_c_production_flux$fineroot_production_flux[fineroot_c_production_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$fineroot <- resample(fineroot_c_production_flux$fineroot_production_flux[fineroot_c_production_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$understorey <- resample(understorey_c_flux_clipping$understorey_production_flux[understorey_c_flux_clipping$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$understorey <- resample(understorey_c_flux_clipping$understorey_production_flux[understorey_c_flux_clipping$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$twig <- resample(twiglitter_c_production_flux$twig_flux[twiglitter_c_production_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$twig <- resample(twiglitter_c_production_flux$twig_flux[twiglitter_c_production_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$seed <- resample(seedlitter_c_production_flux$seed_flux[seedlitter_c_production_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$seed <- resample(seedlitter_c_production_flux$seed_flux[seedlitter_c_production_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$bark <- resample(barklitter_c_production_flux$bark_flux[barklitter_c_production_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$bark <- resample(barklitter_c_production_flux$bark_flux[barklitter_c_production_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$frass <- resample(frass_c_production_flux$frass_production_flux[frass_c_production_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$frass <- resample(frass_c_production_flux$frass_production_flux[frass_c_production_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    tmpDF1$coarseroot <- resample(coarse_root_c_flux$coarse_root_production_flux[coarse_root_c_flux$Ring%in%c(2,3,6)], 1000, replace=T)
    tmpDF2$coarseroot <- resample(coarse_root_c_flux$coarse_root_production_flux[coarse_root_c_flux$Ring%in%c(1,4,5)], 1000, replace=T)
    
    
    tmpDF3 <- rowSums(tmpDF1, na.rm=T) 
    tmpDF4 <- rowSums(tmpDF2, na.rm=T)
    
    tmpDF5 <- data.frame("npp"=tmpDF3 * conv,
                         "veg_p_uptake"=puptDF$aCO2 * conv)
    
    tmpDF6 <- data.frame("npp"=tmpDF4 * conv,
                         "veg_p_uptake"=puptDF$eCO2 * conv)
    
    tmpDF5$P_PUE <- with(tmpDF5, npp/veg_p_uptake)
    tmpDF6$P_PUE <- with(tmpDF6, npp/veg_p_uptake)
    
    
    tmpDF7 <- data.frame("aCO2"=tmpDF5$P_PUE,
                         "eCO2"=tmpDF6$P_PUE)
    tmpDF7$diff <- with(tmpDF7, eCO2-aCO2)
    
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Plant PUE"] <- mean(c(tmpDF7$aCO2, tmpDF7$eCO2))
    
    treatDF$aCO2_ci_low[treatDF$terms == "Plant PUE"] <- t.test(c(tmpDF7$aCO2, tmpDF7$eCO2), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high[treatDF$terms == "Plant PUE"] <- t.test(c(tmpDF7$aCO2, tmpDF7$eCO2), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff[treatDF$terms == "Plant PUE"] <- mean(tmpDF7$diff) 
    
    treatDF$diff_ci_low_95[treatDF$terms == "Plant PUE"] <- t.test(tmpDF7$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95[treatDF$terms == "Plant PUE"] <- t.test(tmpDF7$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85[treatDF$terms == "Plant PUE"] <- t.test(tmpDF7$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85[treatDF$terms == "Plant PUE"] <- t.test(tmpDF7$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75[treatDF$terms == "Plant PUE"] <- t.test(tmpDF7$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75[treatDF$terms == "Plant PUE"] <- t.test(tmpDF7$diff, conf.level=0.75)$conf.int[2]
    
    
    ### output
    write.csv(treatDF, paste0("output/summary_tables/summary_table_total_P_budget_", norm, ".csv"), 
              row.names=F)
    
    return(treatDF)
}