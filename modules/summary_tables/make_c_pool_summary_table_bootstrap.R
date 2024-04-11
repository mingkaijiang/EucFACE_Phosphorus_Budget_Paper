
#### To make EucFACE C summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_c_pool_summary_table_bootstrap <- function(norm,
                                                canopy_c_pool,
                                                wood_c_pool,
                                                standing_dead_c_pool,
                                                fineroot_c_pool,
                                                coarse_root_c_pool,
                                                understorey_c_pool,
                                                soil_c_pool,
                                                microbial_c_pool,
                                                mycorrhizal_c_pool,
                                                leaflitter_c_pool) {
  
    ### Define pool variable names
    terms <- c("Canopy C Pool", 
               "Wood C Pool", 
               "Sapwood C Pool", 
               "Heartwood C Pool",
               "Standing Dead Wood C Pool",
               "Fine Root C Pool",
               "Coarse Root C Pool", 
               "Understorey C Pool", 
               "Microbial C Pool",
               "Microbial C Pool 0-10cm",
               "Microbial C Pool 10-30cm",
               "Microbial C Pool 30-60cm",
               "Leaflitter C Pool",
               "Soil C Pool",
               "Soil C Pool 0-10cm",
               "Soil C Pool 10-30cm",
               "Soil C Pool 30-60cm",
               "Mycorrhizal C Pool")
    
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
    canopy_c_pool$Trt <- "aCO2"
    canopy_c_pool$Trt[canopy_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    wood_c_pool$Trt <- "aCO2"
    wood_c_pool$Trt[wood_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    standing_dead_c_pool$Trt <- "aCO2"
    standing_dead_c_pool$Trt[standing_dead_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
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
    
    mycorrhizal_c_pool$Trt <- "aCO2"
    mycorrhizal_c_pool$Trt[mycorrhizal_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    leaflitter_c_pool$Trt <- "aCO2"
    leaflitter_c_pool$Trt[leaflitter_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    
    
    ### basics
    library(boot)
    set.seed(123)
    
    ### prepare the relationship
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    
    
    ### Canopy C pool
    # bootstrapping with 1000 replications 
    results <- boot(data=canopy_c_pool, statistic=bs, 
                    R=5000, formula=leaf_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Canopy C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Canopy C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Leaflitter C 
    # bootstrapping with 1000 replications 
    results <- boot(data=leaflitter_c_pool, statistic=bs, 
                    R=1000, formula=leaflitter_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Leaflitter C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Wood C 
    # bootstrapping with 1000 replications 
    results <- boot(data=wood_c_pool, statistic=bs, 
                    R=1000, formula=wood_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Wood C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Wood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Sapwood C 
    # bootstrapping with 1000 replications 
    results <- boot(data=wood_c_pool, statistic=bs, 
                    R=1000, formula=sap_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Sapwood C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Sapwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Heartwood C
    # bootstrapping with 1000 replications 
    results <- boot(data=wood_c_pool, statistic=bs, 
                    R=1000, formula=heart_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Heartwood C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Heartwood C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Fine root C pool
    # bootstrapping with 1000 replications 
    results <- boot(data=fineroot_c_pool, statistic=bs, 
                    R=1000, formula=fineroot_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Fine Root C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Fine Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Coarse root C pool
    # bootstrapping with 1000 replications 
    results <- boot(data=coarse_root_c_pool, statistic=bs, 
                    R=1000, formula=coarse_root_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Coarse Root C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Understorey C pool
    # bootstrapping with 1000 replications 
    results <- boot(data=understorey_c_pool, statistic=bs, 
                    R=1000, formula=Total_g_C_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Understorey C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Understorey C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Microbial C pool
    # bootstrapping with 1000 replications 
    results <- boot(data=microbial_c_pool[microbial_c_pool$Depth=="0_10",], statistic=bs, 
                    R=1000, formula=Cmic_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Microbial C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Microbial C pool
    # bootstrapping with 1000 replications 
    results <- boot(data=microbial_c_pool[microbial_c_pool$Depth=="10_30",], statistic=bs, 
                    R=1000, formula=Cmic_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Microbial C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Microbial C pool
    # bootstrapping with 1000 replications 
    results <- boot(data=microbial_c_pool[microbial_c_pool$Depth=="transition",], statistic=bs, 
                    R=1000, formula=Cmic_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Microbial C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Microbial C pool
    out <- summaryBy(Cmic_g_m2~Ring+Depth+Trt,data=microbial_c_pool,FUN=sum, keep.names=T,na.rm=T)
    
    # bootstrapping with 1000 replications 
    results <- boot(data=out, statistic=bs, 
                    R=1000, formula=Cmic_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Microbial C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Microbial C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])

    
    
    ### Soil C pool
    out <- summaryBy(soil_carbon_pool~Ring+Depth+Trt,data=soil_c_pool,FUN=sum, keep.names=T,na.rm=T)
    
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_c_pool, statistic=bs, 
                    R=1000, formula=soil_carbon_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil C Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil C Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Soil C pool 0 - 10 cm
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_c_pool[soil_c_pool$Depth=="0_10",], statistic=bs, 
                    R=1000, formula=soil_carbon_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil C Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Soil C pool 10 - 30 cm
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_c_pool[soil_c_pool$Depth=="10_30",], statistic=bs, 
                    R=1000, formula=soil_carbon_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil C Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Soil C pool 30 - 60 cm
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_c_pool[soil_c_pool$Depth=="transition",], statistic=bs, 
                    R=1000, formula=soil_carbon_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil C Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    
    ### save output
    write.csv(treatDF, 
              paste0("output/summary_tables/summary_table_C_pool_", norm, ".csv"), row.names=F)
    
    
    
    ##### output tables
    return(treatDF)
      
}

