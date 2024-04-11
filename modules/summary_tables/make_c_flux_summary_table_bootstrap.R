
#### To make EucFACE C summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_c_flux_summary_table_bootstrap <- function(norm,
                                                leaflitter_c_production_flux,
                                                twiglitter_c_production_flux,
                                                barklitter_c_production_flux,
                                                seedlitter_c_production_flux,
                                                canopy_c_production_flux,
                                                wood_c_production,
                                                fineroot_c_production_flux,
                                                coarse_root_c_flux,
                                                understorey_c_flux_clipping,
                                                understorey_litter_c_flux,
                                                frass_c_production_flux) {
  
  ### Define production variable names
  terms <- c("Canopy C flux", 
             "Wood C flux", 
             "Fine Root C flux",
             "Coarse Root C flux",
             "Leaflitter C flux", 
             "Twig litter C flux", 
             "Bark litter C flux", 
             "Seed litter C flux", 
             "Fineroot Litter C flux",
             "Frass C flux",
             "Understorey C flux", 
             "Understorey Litter C flux")
  
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
  leaflitter_c_production_flux$Trt <- "aCO2"
  leaflitter_c_production_flux$Trt[leaflitter_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
  
  twiglitter_c_production_flux$Trt <- "aCO2"
  twiglitter_c_production_flux$Trt[twiglitter_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
  
  barklitter_c_production_flux$Trt <- "aCO2"
  barklitter_c_production_flux$Trt[barklitter_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
  
  seedlitter_c_production_flux$Trt <- "aCO2"
  seedlitter_c_production_flux$Trt[seedlitter_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
  
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
  
  understorey_litter_c_flux$Trt <- "aCO2"
  understorey_litter_c_flux$Trt[understorey_litter_c_flux$Ring%in%c(1,4,5)] <- "eCO2"
  
  frass_c_production_flux$Trt <- "aCO2"
  frass_c_production_flux$Trt[frass_c_production_flux$Ring%in%c(1,4,5)] <- "eCO2"
  
  
  ## basics
  library(boot)
  set.seed(123)
  
  ### prepare the relationship
  bs <- function(formula, data, indices) {
    d <- data[indices,] # allows boot to select sample 
    fit <- lm(formula, data=d)
    return(coef(fit)) 
  } 
  
  
  ### convert daily flux in mg P m2 d-1 to g P m-2 yr-1
  conv <- 365 / 1000
  
  
  ### Canopy C flux
  # bootstrapping with 1000 replications 
  results <- boot(data=canopy_c_production_flux, statistic=bs, 
                  R=1000, formula=leaf_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Canopy C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Canopy C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Canopy C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Canopy C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Canopy C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Canopy C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Canopy C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Canopy C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Canopy C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Canopy C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    ### Wood C 
  # bootstrapping with 1000 replications 
  results <- boot(data=wood_c_production, statistic=bs, 
                  R=1000, formula=wood_production_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Wood C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Wood C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Wood C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Wood C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Wood C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Wood C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Wood C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Wood C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Wood C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Wood C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    ### Fine root C flux
  # bootstrapping with 1000 replications 
  results <- boot(data=fineroot_c_production_flux, statistic=bs, 
                  R=1000, formula=fineroot_production_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Fine Root C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Fine Root C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Fine Root C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Fine Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Fine Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Fine Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Fine Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Fine Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Fine Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Fine Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    ### Coarse root C flux
  # bootstrapping with 1000 replications 
  results <- boot(data=coarse_root_c_flux, statistic=bs, 
                  R=1000, formula=coarse_root_production_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Coarse Root C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Coarse Root C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Coarse Root C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Coarse Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Coarse Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Coarse Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Coarse Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Coarse Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Coarse Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Coarse Root C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    ### Understorey C flux
  # bootstrapping with 1000 replications 
  results <- boot(data=understorey_c_flux_clipping, statistic=bs, 
                  R=1000, formula=understorey_production_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Understorey C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Understorey C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Understorey C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Understorey C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Understorey C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Understorey C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Understorey C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Understorey C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Understorey C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Understorey C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    ### Understorey Litter C flux
  # bootstrapping with 1000 replications 
  results <- boot(data=understorey_litter_c_flux, statistic=bs, 
                  R=1000, formula=understorey_litter_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Understorey Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
    ### Frass production flux
  # bootstrapping with 1000 replications 
  results <- boot(data=frass_c_production_flux, statistic=bs, 
                  R=1000, formula=frass_production_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Frass C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Frass C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Frass C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Frass C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Frass C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Frass C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Frass C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Frass C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Frass C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Frass C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    
    ### Leaf litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=leaflitter_c_production_flux, statistic=bs, 
                  R=1000, formula=leaf_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Leaflitter C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Leaflitter C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Leaflitter C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Leaflitter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Leaflitter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Leaflitter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Leaflitter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Leaflitter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Leaflitter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Leaflitter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    
    ### Twig litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=twiglitter_c_production_flux, statistic=bs, 
                  R=1000, formula=twig_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Twig litter C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Twig litter C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Twig litter C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Twig litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Twig litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Twig litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Twig litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Twig litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Twig litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Twig litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    ### Bark litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=barklitter_c_production_flux, statistic=bs, 
                  R=1000, formula=bark_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Bark litter C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Bark litter C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Bark litter C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Bark litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Bark litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Bark litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Bark litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Bark litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Bark litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Bark litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
    
    
    ### Seed litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=seedlitter_c_production_flux, statistic=bs, 
                  R=1000, formula=seed_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Seed litter C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Seed litter C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Seed litter C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Seed litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Seed litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Seed litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Seed litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Seed litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Seed litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Seed litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
    ### Fine Root litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=fineroot_c_production_flux, statistic=bs, 
                  R=1000, formula=fineroot_production_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Fineroot Litter C flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
    
  ### csv
  write.csv(treatDF, 
            paste0("output/summary_tables/summary_table_C_flux_", norm, ".csv"), row.names=F)
  
  
  
  
  ##### output tables
  return(treatDF)
  
}
