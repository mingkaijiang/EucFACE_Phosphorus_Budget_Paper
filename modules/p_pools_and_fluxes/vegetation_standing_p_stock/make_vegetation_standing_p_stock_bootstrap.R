make_vegetation_standing_p_stock_bootstrap <- function(norm, 
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
                                                       coarseroot_P_retranslocation_flux) {
    
    
    ### Compute major terms
    terms <- c("Total plant P stock", 
               "Total plant P requirement flux", 
               "Total plant P retranslocation flux", 
               "Plant P uptake flux", 
               "Soil P mineralization flux",
               "Labile Pi stock",
               "Plant P uptake over requirement",
               "Plant P MRT", 
               "Plant PUE",
               "Microbe P MRT",
               "Microbe P MRT 0-10cm",
               "Microbe P MRT 10-30cm",
               "Microbe P MRT 30-60cm",
               "plant_GPP_efficiency", 
               "Overstorey GPP efficiency",
               "Understorey GPP efficiency",
               "Plant P uptake over P mineralization",
               "Leaflitter P over P mineralization",
               "Fineroot litter P over P mineralization",
               "Twig litter P over P mineralization",
               "Bark litter P over P mineralization",
               "Seed litter P over P mineralization",
               "Frass litter P over P mineralization",
               "Understorey litter P over P mineralization",
               "Leaching P over P mineralization",
               "Overstorey aboveground P stock",
               "Understorey aboveground P stock",
               "Belowground P stock",
               "Dead P stock")
    
    
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
    
    
    ### basics
    library(boot)
    set.seed(123)
    
    ### prepare the relationship
    bs <- function(formula, data, indices) {
        d <- data[indices,] # allows boot to select sample 
        fit <- lm(formula, data=d)
        return(coef(fit)) 
    } 
    
    
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
    treatDF$aCO2<- mean(c(tmpDF3, tmpDF4))
    
    treatDF$aCO2_ci_low <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1] 
    treatDF$aCO2_ci_high <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2] 
    
    treatDF$diff <- mean(tmpDF5$diff) 
    
    
    treatDF$diff_ci_low_95 <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[1]
    treatDF$diff_ci_high_95 <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[2]
    
    treatDF$diff_ci_low_85 <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[1]
    treatDF$diff_ci_high_85 <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[2]
    
    treatDF$diff_ci_low_75 <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[1]
    treatDF$diff_ci_high_75 <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[2]
    
    
    write.csv(treatDF, paste0("output/summary_tables/summary_table_veg_standing_p_stock_", norm, ".csv"), 
              row.names=F)
    
    return(treatDF)
}