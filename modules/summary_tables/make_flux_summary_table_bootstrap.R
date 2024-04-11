
#### To make EucFACE P summary table by CO2 treatment
make_flux_summary_table_bootstrap <- function(norm,
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

  
    #### Ignore time but produce time coverage information
    #### This is for fluxes
  
  ### Define production variable names
  terms <- c("Canopy P flux",              # include retranslocated and new uptake flux
             "Herbivory P flux",
             "Wood P flux",                # include retranslocated and new uptake flux
             "Fine Root P flux",           # include retranslocated and new uptake flux
             "Coarse Root P flux",         # include retranslocated and new uptake flux
             "Leaflitter P flux",          # Proxy for new uptake
             "Fineroot Litter P flux",     # Proxy for new uptake
             "Twig litter P flux",         # to be included in the new uptake
             "Bark litter P flux",         # to be included in the new uptake
             "Seed litter P flux",         # to be included in the new uptake
             "Frass P flux",               # to be included in the new uptake
             "Understorey P flux",         # include retranslocated and new uptake flux
             "Understorey Litter P flux",  # Proxy for new uptake 
             "Canopy retrans P flux",     
             "Sapwood retrans P flux",
             "Fineroot retrans P flux",
             "Coarseroot retrans P flux",
             "Understorey retrans P flux",
             "Total vegetation production P flux",
             "Total vegetation retranslocation P flux",
             "Total vegetation uptake P flux",
             "Mineralization P flux 0-10cm",
             "Mineralization P flux 10-30cm",
             "Mineralization P flux 30-60cm",
             "Leaching P flux")
  
 
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
  
  
  ### basics
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
  
  
  
  # bootstrapping with 1000 replications 
  results <- boot(data=canopy_p_flux, statistic=bs, 
                  R=1000, formula=canopy_p_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Canopy P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Canopy P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Canopy P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Canopy P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Canopy P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Canopy P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Canopy P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Canopy P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Canopy P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Canopy P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
    
  ### Herbivory P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=canopy_p_flux, statistic=bs, 
                  R=1000, formula=herbivory_p_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Herbivory P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Herbivory P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Herbivory P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Herbivory P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Herbivory P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Herbivory P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Herbivory P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Herbivory P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Herbivory P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Herbivory P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
      
  
      ### Wood P 
  # bootstrapping with 1000 replications 
  results <- boot(data=wood_p_flux, statistic=bs, 
                  R=1000, formula=wood_p_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Wood P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Wood P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Wood P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Wood P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Wood P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Wood P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Wood P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Wood P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Wood P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Wood P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
  
      
      ### Fine root P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=fineroot_p_production, statistic=bs, 
                  R=1000, formula=fineroot_p_flux_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Fine Root P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Fine Root P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Fine Root P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Fine Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Fine Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Fine Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Fine Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Fine Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Fine Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Fine Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
      
      
      ### Coarse root P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=coarse_root_p_flux, statistic=bs, 
                  R=1000, formula=coarse_root_p_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Coarse Root P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Coarse Root P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Coarse Root P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Coarse Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Coarse Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Coarse Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Coarse Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Coarse Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Coarse Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Coarse Root P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
      
      ### Understorey P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=understorey_p_flux, statistic=bs, 
                  R=1000, formula=understorey_p_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Understorey P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Understorey P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Understorey P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Understorey P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Understorey P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Understorey P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Understorey P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Understorey P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Understorey P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Understorey P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
  
      ### Understorey Litter P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=understorey_litter_p_flux, statistic=bs, 
                  R=1000, formula=understorey_litter_p_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Understorey Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
  
      ### Frass production flux
  # bootstrapping with 1000 replications 
  results <- boot(data=frass_p_production, statistic=bs, 
                  R=1000, formula=frass_p_flux_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Frass P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Frass P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Frass P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Frass P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Frass P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Frass P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Frass P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Frass P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Frass P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Frass P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
      
      ### Leaf litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=leaflitter_p_flux, statistic=bs, 
                  R=1000, formula=leaflitter_p_flux_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Leaflitter P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Leaflitter P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Leaflitter P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Leaflitter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Leaflitter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Leaflitter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Leaflitter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Leaflitter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Leaflitter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Leaflitter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
      ### Fine Root litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=fineroot_litter_p_flux, statistic=bs, 
                  R=1000, formula=fineroot_litter_p_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Fineroot Litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
      
      ### twig litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=twig_litter_p_flux, statistic=bs, 
                  R=1000, formula=twiglitter_p_flux_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Twig litter P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Twig litter P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Twig litter P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Twig litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Twig litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Twig litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Twig litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Twig litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Twig litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Twig litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
      ### bark litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=bark_litter_p_flux, statistic=bs, 
                  R=1000, formula=barklitter_p_flux_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Bark litter P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Bark litter P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Bark litter P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Bark litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Bark litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Bark litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Bark litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Bark litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Bark litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Bark litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
      ### seed litter flux
  # bootstrapping with 1000 replications 
  results <- boot(data=seed_litter_p_flux, statistic=bs, 
                  R=1000, formula=seedlitter_p_flux_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Seed litter P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Seed litter P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Seed litter P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Seed litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Seed litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Seed litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Seed litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Seed litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Seed litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Seed litter P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
      
      ###  P mineralization flux
  # bootstrapping with 1000 replications 
  results <- boot(data=soil_p_mineralization[soil_p_mineralization$Depth =="0_10",], statistic=bs, 
                  R=1000, formula=p_mineralization_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Mineralization P flux 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
      
  ###  P mineralization flux
  # bootstrapping with 1000 replications 
  results <- boot(data=soil_p_mineralization[soil_p_mineralization$Depth =="10_30",], statistic=bs, 
                  R=1000, formula=p_mineralization_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Mineralization P flux 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
      
      ###  P mineralization flux
  # bootstrapping with 1000 replications 
  results <- boot(data=soil_p_mineralization[soil_p_mineralization$Depth =="transition",], statistic=bs, 
                  R=1000, formula=p_mineralization_mg_m2_d~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Mineralization P flux 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
      
      ###  P leaching flux
  # bootstrapping with 1000 replications 
  results <- boot(data=soil_p_leaching, statistic=bs, 
                  R=1000, formula=phosphate_leaching_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Leaching P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Leaching P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Leaching P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Leaching P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Leaching P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Leaching P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Leaching P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Leaching P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Leaching P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Leaching P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
      
      
      ###  Canopy retrans P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=canopy_P_retranslocation_flux, statistic=bs, 
                  R=1000, formula=canopy_p_retrans_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Canopy retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
      ###  Sapwood retrans P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=sapwood_P_retranslocation_flux, statistic=bs, 
                  R=1000, formula=sapwood_p_retrans_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Sapwood retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
      
  
  
      ###  Coarseroot retrans P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=coarseroot_P_retranslocation_flux, statistic=bs, 
                  R=1000, formula=coarseroot_p_retrans_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Coarseroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
      
      ###  Fineroot retrans P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=fineroot_P_retranslocation_flux, statistic=bs, 
                  R=1000, formula=fineroot_p_retrans_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Fineroot retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
      
      
  ###  Understorey retrans P flux
  # bootstrapping with 1000 replications 
  results <- boot(data=understorey_P_retranslocation_flux, statistic=bs, 
                  R=1000, formula=understorey_p_retrans_flux~Trt)
  
  # assign values 
  treatDF$aCO2[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Understorey retrans P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
  ### calculate CO2 effect for total vegetation production P flux
  #tmpDF1 <- treatDF[treatDF$terms%in%c("Canopy P flux",
  #                                    "Wood P flux",
  #                                    "Fine Root P flux",
  #                                    "Coarse Root P flux",
  #                                    "Twig litter P flux",
  #                                    "Bark litter P flux",
  #                                    "Seed litter P flux",
  #                                    "Understorey P flux"),]
  #
  #tmpDF1$ci_range <- with(tmpDF1, (diff_ci_high_95-diff_ci_low_95)/2)
  #tmpDF1$t95 <- 1.96
  #tmpDF1$se <- with(tmpDF1, ci_range/t95)
  #tmpDF1$sd <- with(tmpDF1, se * sqrt(1000))
  #
  ### pooled sd and confidence intervals
  #pooled_sd_1 <- sqrt(sum(tmpDF1$sd^2)/8)
  #pooled_diff_1 <- sum(tmpDF1$diff)
  #
  #### from pooled_sd to 
  #pooled_se_1 <- pooled_sd_1 / sqrt(8)
  #pooled_ci_95_1 <- qt(0.975, 6) * pooled_se_1
  #pooled_ci_85_1 <- qt(0.925, 6) * pooled_se_1
  #pooled_ci_75_1 <- qt(0.875, 6) * pooled_se_1
  #
  #
  ### assign values 
  #treatDF$aCO2[treatDF$terms == "Total vegetation production P flux"] <- mean(tmpDF1$aCO2)
  #
  #treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation production P flux"] <- NA
  #treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation production P flux"] <- NA
  #
  #treatDF$diff[treatDF$terms == "Total vegetation production P flux"] <- pooled_diff_1
  #
  #treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation production P flux"] <- pooled_diff_1 - pooled_ci_95_1
  #treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation production P flux"] <- pooled_diff_1 + pooled_ci_95_1
  #
  #treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation production P flux"] <- pooled_diff_1 - pooled_ci_85_1
  #treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation production P flux"] <- pooled_diff_1 + pooled_ci_85_1
  #
  #treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation production P flux"] <- pooled_diff_1 - pooled_ci_75_1
  #treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation production P flux"] <- pooled_diff_1 + pooled_ci_75_1
     
  
  
   
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
  tmpDF1$Canopy <- sample(canopy_p_flux$canopy_p_flux[canopy_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Canopy <- sample(canopy_p_flux$canopy_p_flux[canopy_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Wood <- sample(wood_p_flux$wood_p_flux[wood_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Wood <- sample(wood_p_flux$wood_p_flux[wood_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Fineroot <- sample(fineroot_p_production$fineroot_p_flux_mg_m2_d[fineroot_p_production$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Fineroot <- sample(fineroot_p_production$fineroot_p_flux_mg_m2_d[fineroot_p_production$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Coarseroot <- sample(coarse_root_p_flux$coarse_root_p_flux[coarse_root_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Coarseroot <- sample(coarse_root_p_flux$coarse_root_p_flux[coarse_root_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Twig <- sample(twig_litter_p_flux$twiglitter_p_flux_mg_m2_d[twig_litter_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Twig <- sample(twig_litter_p_flux$twiglitter_p_flux_mg_m2_d[twig_litter_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Bark <- sample(bark_litter_p_flux$barklitter_p_flux_mg_m2_d[bark_litter_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Bark <- sample(bark_litter_p_flux$barklitter_p_flux_mg_m2_d[bark_litter_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Seed <- sample(seed_litter_p_flux$seedlitter_p_flux_mg_m2_d[seed_litter_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Seed <- sample(seed_litter_p_flux$seedlitter_p_flux_mg_m2_d[seed_litter_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)

  tmpDF1$Understorey <- sample(understorey_p_flux$understorey_p_flux[understorey_p_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Understorey <- sample(understorey_p_flux$understorey_p_flux[understorey_p_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF3 <- rowSums(tmpDF1, na.rm=T)
  tmpDF4 <- rowSums(tmpDF2, na.rm=T)
  tmpDF5 <- data.frame("aCO2"=tmpDF3,
                       "eCO2"=tmpDF4)
  tmpDF5$diff <- with(tmpDF5, eCO2-aCO2)    
  prodDF <- tmpDF5
  
  tmpDF6 <- data.frame("value"=tmpDF3, "Trt"="aCO2")
  tmpDF7 <- data.frame("value"=tmpDF4, "Trt"="eCO2")
  
  tmpDF8 <- rbind(tmpDF6, tmpDF7)
  
  ### statistics
  results <- boot(data=tmpDF8, statistic=bs, 
                  R=2000, formula=value~Trt)
  
  treatDF$aCO2[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation production P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
  
  
  # assign values 
  #treatDF$aCO2[treatDF$terms == "Total vegetation production P flux"] <- mean(c(tmpDF3, tmpDF4)) * conv
  #
  #treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation production P flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1] * conv
  #treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation production P flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2] * conv 
  #
  #treatDF$diff[treatDF$terms == "Total vegetation production P flux"] <- mean(tmpDF5$diff) * conv
  #
  #
  #treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation production P flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[1] * conv
  #treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation production P flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[2] * conv
  #
  #treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation production P flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[1] * conv
  #treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation production P flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[2] * conv
  #
  #treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation production P flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[1] * conv
  #treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation production P flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[2] * conv
  
  
  ###  Total retranslocation flux
  #tmpDF2 <- treatDF[treatDF$terms%in%c("Canopy retrans P flux",
  #                                    "Sapwood retrans P flux",
  #                                    "Fineroot retrans P flux",
  #                                    "Coarseroot retrans P flux",
  #                                    "Understorey retrans P flux"),]
  #
  #tmpDF2$ci_range <- with(tmpDF2, (diff_ci_high_95-diff_ci_low_95)/2)
  #tmpDF2$t95 <- 1.96
  #tmpDF2$se <- with(tmpDF2, ci_range/t95)
  #tmpDF2$sd <- with(tmpDF2, se * sqrt(1000))
  #
  ### pooled sd and confidence intervals
  #pooled_sd_2 <- sqrt(sum(tmpDF2$sd^2)/8)
  #pooled_diff_2 <- sum(tmpDF2$diff)
  #
  #### from pooled_sd to 
  #pooled_se_2 <- pooled_sd_2 / sqrt(8)
  #pooled_ci_95_2 <- qt(0.975, 6) * pooled_se_2
  #pooled_ci_85_2 <- qt(0.925, 6) * pooled_se_2
  #pooled_ci_75_2 <- qt(0.875, 6) * pooled_se_2
  #
  #
  ### assign values 
  #treatDF$aCO2[treatDF$terms == "Total vegetation retranslocation P flux"] <- mean(tmpDF2$aCO2)
  #
  #treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation retranslocation P flux"] <- NA
  #treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation retranslocation P flux"] <- NA
  #
  #treatDF$diff[treatDF$terms == "Total vegetation retranslocation P flux"] <- pooled_diff_2
  #
  #treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation retranslocation P flux"] <- pooled_diff_2 - pooled_ci_95_2
  #treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation retranslocation P flux"] <- pooled_diff_2 + pooled_ci_95_2
  #
  #treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation retranslocation P flux"] <- pooled_diff_2 - pooled_ci_85_2
  #treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation retranslocation P flux"] <- pooled_diff_2 + pooled_ci_85_2
  #
  #treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation retranslocation P flux"] <- pooled_diff_2 - pooled_ci_75_2
  #treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation retranslocation P flux"] <- pooled_diff_2 + pooled_ci_75_2
  
  
  
  
  tmpDF1 <- data.frame("Canopy"=rep(NA, 1000),
                       "Wood"=rep(NA, 1000),
                       "Fineroot"=rep(NA, 1000),
                       "Coarseroot"=rep(NA, 1000),
                       "Understorey"=rep(NA, 1000))
  tmpDF2 <- tmpDF1
  
  ### change of method to estimate aggregated fluxes
  tmpDF1$Canopy <- sample(canopy_P_retranslocation_flux$canopy_p_retrans_flux[canopy_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Canopy <- sample(canopy_P_retranslocation_flux$canopy_p_retrans_flux[canopy_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Wood <- sample(sapwood_P_retranslocation_flux$sapwood_p_retrans_flux[sapwood_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Wood <- sample(sapwood_P_retranslocation_flux$sapwood_p_retrans_flux[sapwood_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Fineroot <- sample(fineroot_P_retranslocation_flux$fineroot_p_retrans_flux[fineroot_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Fineroot <- sample(fineroot_P_retranslocation_flux$fineroot_p_retrans_flux[fineroot_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF1$Coarseroot <- sample(coarseroot_P_retranslocation_flux$coarseroot_p_retrans_flux[coarseroot_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Coarseroot <- sample(coarseroot_P_retranslocation_flux$coarseroot_p_retrans_flux[coarseroot_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)

  tmpDF1$Understorey <- sample(understorey_P_retranslocation_flux$understorey_p_retrans_flux[understorey_P_retranslocation_flux$Ring%in%c(2,3,6)], 1000, replace=T)
  tmpDF2$Understorey <- sample(understorey_P_retranslocation_flux$understorey_p_retrans_flux[understorey_P_retranslocation_flux$Ring%in%c(1,4,5)], 1000, replace=T)
  
  tmpDF3 <- rowSums(tmpDF1, na.rm=T)
  tmpDF4 <- rowSums(tmpDF2, na.rm=T)
  tmpDF5 <- data.frame("aCO2"=tmpDF3,
                       "eCO2"=tmpDF4)
  tmpDF5$diff <- with(tmpDF5, eCO2-aCO2)
  retranDF <- tmpDF5
  
  tmpDF6 <- data.frame("value"=tmpDF3, "Trt"="aCO2")
  tmpDF7 <- data.frame("value"=tmpDF4, "Trt"="eCO2")
  
  tmpDF8 <- rbind(tmpDF6, tmpDF7)
  
  ### statistics
  results <- boot(data=tmpDF8, statistic=bs, 
                  R=2000, formula=value~Trt)
  
  treatDF$aCO2[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation retranslocation P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
  
  # assign values 
  #treatDF$aCO2[treatDF$terms == "Total vegetation retranslocation P flux"] <- mean(c(tmpDF3, tmpDF4)) * conv
  #
  #treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation retranslocation P flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1] * conv
  #treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation retranslocation P flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2] * conv 
  #
  #treatDF$diff[treatDF$terms == "Total vegetation retranslocation P flux"] <- mean(tmpDF5$diff) * conv
  #
  #
  #treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation retranslocation P flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[1] * conv
  #treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation retranslocation P flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[2] * conv
  #
  #treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation retranslocation P flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[1] * conv
  #treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation retranslocation P flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[2] * conv
  #
  #treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation retranslocation P flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[1] * conv
  #treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation retranslocation P flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[2] * conv
  
  
      
  ###  Total vegetation uptake P flux
  #pooled_diff <- pooled_diff_1 - pooled_diff_2
  #
  ### pooled sd and confidence intervals
  #pooled_sd <- sqrt((pooled_sd_1^2+pooled_sd_2^2)/2)
#
  #### from pooled_sd to 
  #pooled_se <- pooled_sd / sqrt(2)
  #pooled_ci_95 <- qt(0.975, 6) * pooled_se
  #pooled_ci_85 <- qt(0.925, 6) * pooled_se
  #pooled_ci_75 <- qt(0.875, 6) * pooled_se
  #
  #
  ### assign values 
  #treatDF$aCO2[treatDF$terms == "Total vegetation uptake P flux"] <- sum(tmpDF1$aCO2)-sum(tmpDF2$aCO2)
  #
  #treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation uptake P flux"] <- NA
  #treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation uptake P flux"] <- NA
  #
  #treatDF$diff[treatDF$terms == "Total vegetation uptake P flux"] <- pooled_diff
  #
  #treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation uptake P flux"] <- pooled_diff - pooled_ci_95
  #treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation uptake P flux"] <- pooled_diff + pooled_ci_95
  #
  #treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation uptake P flux"] <- pooled_diff - pooled_ci_85
  #treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation uptake P flux"] <- pooled_diff + pooled_ci_85
  #
  #treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation uptake P flux"] <- pooled_diff - pooled_ci_75
  #treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation uptake P flux"] <- pooled_diff + pooled_ci_75
  
  
  
  tmpDF3 <- prodDF$aCO2 - retranDF$aCO2
  tmpDF4 <- prodDF$eCO2 - retranDF$eCO2
  
  tmpDF5 <- data.frame("aCO2"=tmpDF3,
                       "eCO2"=tmpDF4)
  tmpDF5$diff <- with(tmpDF5, eCO2-aCO2)
  
  tmpDF6 <- data.frame("value"=tmpDF3, "Trt"="aCO2")
  tmpDF7 <- data.frame("value"=tmpDF4, "Trt"="eCO2")
  
  tmpDF8 <- rbind(tmpDF6, tmpDF7)
  
  ### statistics
  results <- boot(data=tmpDF8, statistic=bs, 
                  R=2000, formula=value~Trt)
  
  treatDF$aCO2[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(results$t0[1]) * conv
  v1 <- (as.numeric(boot.ci(results, type="bca", index=1)$bca[5])-as.numeric(boot.ci(results, type="bca", index=1)$bca[4]))/2*conv
  
  treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(results$t0[1]) * conv - v1
  treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(results$t0[1]) * conv + v1
  
  treatDF$diff[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv
  
  v2 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4]))/2*conv
  v3 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4]))/2*conv
  v4 <- (as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])-as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4]))/2*conv
  
  treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v2
  treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v2
  
  treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v3
  treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v3
  
  treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv - v4
  treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation uptake P flux"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2]) * conv + v4
  
  
  # assign values 
  #treatDF$aCO2[treatDF$terms == "Total vegetation uptake P flux"] <- mean(c(tmpDF3, tmpDF4)) * conv
  #
  #t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1]
  #t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2]
  #
  #treatDF$aCO2_ci_low[treatDF$terms == "Total vegetation uptake P flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[1] * conv
  #treatDF$aCO2_ci_high[treatDF$terms == "Total vegetation uptake P flux"] <- t.test(c(tmpDF3, tmpDF4), conf.level=0.95)$conf.int[2] * conv 
  #
  #treatDF$diff[treatDF$terms == "Total vegetation uptake P flux"] <- mean(tmpDF5$diff) * conv
  #
  #
  #treatDF$diff_ci_low_95[treatDF$terms == "Total vegetation uptake P flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[1] * conv
  #treatDF$diff_ci_high_95[treatDF$terms == "Total vegetation uptake P flux"] <- t.test(tmpDF5$diff, conf.level=0.95)$conf.int[2] * conv
  #
  #treatDF$diff_ci_low_85[treatDF$terms == "Total vegetation uptake P flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[1] * conv
  #treatDF$diff_ci_high_85[treatDF$terms == "Total vegetation uptake P flux"] <- t.test(tmpDF5$diff, conf.level=0.85)$conf.int[2] * conv
  #
  #treatDF$diff_ci_low_75[treatDF$terms == "Total vegetation uptake P flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[1] * conv
  #treatDF$diff_ci_high_75[treatDF$terms == "Total vegetation uptake P flux"] <- t.test(tmpDF5$diff, conf.level=0.75)$conf.int[2] * conv
  

    
  ### save output
    write.csv(treatDF, paste0("output/summary_tables/summary_table_P_flux_", norm, ".csv"), 
              row.names=F)
    
    
    ##### output tables
    return(treatDF)
      
}

