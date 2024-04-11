
make_pool_summary_table_bootstrap <- function(norm,
                                              soil_p_pool,
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
    
  #### To make EucFACE P summary table by CO2 treatment
  #### Ignore time but produce time coverage information
  #### This is for pools
  
    ### Define pool variable names
    terms <- c("Canopy P Pool", 
               "Total Wood P Pool", 
               "Sapwood P Pool",
               "Heartwood P Pool",
               "Forestfloor Leaf Litter P Pool",
               "Fine Root P Pool",
               "Coarse Root P Pool", 
               "Understorey P Pool", 
               "Understorey Litter P Pool",
               "Standing Dead Wood P Pool",
               "Microbial P Pool 0-10cm", 
               "Microbial P Pool 10-30cm", 
               "Microbial P Pool 30-60cm", 
               #"Mycorrhizal P Pool 0-10cm", 
               #"Mycorrhizal P Pool 10-30cm", 
               #"Mycorrhizal P Pool 30-60cm", 
               "Soil Phosphate P Pool 0-10cm",
               "Soil Phosphate P Pool 10-30cm",
               "Soil Phosphate P Pool 30-60cm",
               "Soil P Pool 0-10cm",
               "Soil P Pool 10-30cm", 
               "Soil P Pool 30-60cm", 
               "Soil Inorg P Pool 0-10cm",
               "Soil Inorg P Pool 10-30cm", 
               "Soil Inorg P Pool 30-60cm", 
               "Soil Org P Pool 0-10cm",
               "Soil Org P Pool 10-30cm", 
               "Soil Org P Pool 30-60cm", 
               "Exchangeable Pi Pool", 
               "Exchangeable Po Pool",
               "Moderately labile Po Pool", 
               "Secondary Fe bound Pi Pool", 
               "Primary Ca bound Pi Pool",
               "Occluded P Pool")
    
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
    
    
    
    ### basics
    library(boot)
    set.seed(123)
    
    ### prepare the relationship
    bs <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample 
      fit <- lm(formula, data=d)
      return(coef(fit)) 
    } 
    
    
    
    
    
    ### Canopy P 
    # bootstrapping with 1000 replications 
    results <- boot(data=canopy_p_pool, statistic=bs, 
                    R=5000, formula=leaf_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Canopy P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Canopy P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    

    ### Forestfloor Leaf litter
    # bootstrapping with 1000 replications 
    results <- boot(data=leaflitter_p_pool, statistic=bs, 
                    R=1000, formula=leaflitter_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Wood P 
    # bootstrapping with 1000 replications 
    results <- boot(data=wood_p_pool, statistic=bs, 
                    R=1000, formula=wood_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Total Wood P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Total Wood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Sapwood P 
    # bootstrapping with 1000 replications 
    results <- boot(data=sapwood_p_pool, statistic=bs, 
                    R=1000, formula=wood_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Sapwood P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Sapwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Heartwood P 
    # bootstrapping with 1000 replications 
    results <- boot(data=heartwood_p_pool, statistic=bs, 
                    R=1000, formula=wood_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Heartwood P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Heartwood P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Fine root P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=fineroot_p_pool, statistic=bs, 
                    R=1000, formula=fineroot_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Fine Root P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Fine Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### coarse root
    # bootstrapping with 1000 replications 
    results <- boot(data=coarse_root_p_pool, statistic=bs, 
                    R=1000, formula=coarse_root_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Coarse Root P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Understorey P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=understorey_p_pool, statistic=bs, 
                    R=1000, formula=understorey_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Understorey P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Understorey P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    
    ### Understorey Litter P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=understorey_p_pool, statistic=bs, 
                    R=1000, formula=dead_p_pool~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Understorey Litter P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Microbial P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=microbial_p_pool[microbial_p_pool$Depth=="0_10",], statistic=bs, 
                    R=1000, formula=microbial_p_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Microbial P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Microbial P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=microbial_p_pool[microbial_p_pool$Depth=="10_30",], statistic=bs, 
                    R=1000, formula=microbial_p_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Microbial P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Microbial P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=microbial_p_pool[microbial_p_pool$Depth=="transition",], statistic=bs, 
                    R=1000, formula=microbial_p_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Microbial P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    
    ### Soil Phosphate P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_phosphate_pool[soil_phosphate_pool$Depth=="0_10",], statistic=bs, 
                    R=1000, formula=soil_phosphate_p_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Soil Phosphate P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_phosphate_pool[soil_phosphate_pool$Depth=="10_30",], statistic=bs, 
                    R=1000, formula=soil_phosphate_p_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Soil Phosphate P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_phosphate_pool[soil_phosphate_pool$Depth=="transition",], statistic=bs, 
                    R=1000, formula=soil_phosphate_p_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil Phosphate P Pool 30-60cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### Soil P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_p_pool[soil_p_pool$Depth=="0_10",], statistic=bs, 
                    R=1000, formula=soil_p_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil P Pool 0-10cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Soil P pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_p_pool[soil_p_pool$Depth=="10_30",], statistic=bs, 
                    R=500, formula=soil_p_g_m2~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Soil P Pool 10-30cm"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Exchangeable Pi Pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_p_pool_hedley, statistic=bs, 
                    R=1000, formula=F1_2_Pi_Exchangeable~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Exchangeable Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Exchangeable Po Pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_p_pool_hedley, statistic=bs, 
                    R=1000, formula=F1_2_Po_Exchangeable~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Exchangeable Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Moderately labile Po Pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_p_pool_hedley, statistic=bs, 
                    R=1000, formula=F3_Po_Moderately_labile~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Moderately labile Po Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    

    ### Secondary Fe bound Pi Pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_p_pool_hedley, statistic=bs, 
                    R=1000, formula=F3_Fe_bound_P_Secondary_mineral~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Secondary Fe bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Primary Ca bound Pi Pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_p_pool_hedley, statistic=bs, 
                    R=1000, formula=F4_Ca_bound_Primary_Mineral~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Primary Ca bound Pi Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    
    ### Occluded P Pool
    # bootstrapping with 1000 replications 
    results <- boot(data=soil_p_pool_hedley, statistic=bs, 
                    R=1000, formula=F5_6_Occluded~Trt)
    
    # assign values 
    treatDF$aCO2[treatDF$terms == "Occluded P Pool"] <- as.numeric(results$t0[1])
    treatDF$aCO2_ci_low[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[4])
    treatDF$aCO2_ci_high[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=1)$bca[5])
    
    treatDF$diff[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)[2])
    treatDF$diff_ci_low_95[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[4])
    treatDF$diff_ci_high_95[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.95)$bca[5])
    
    treatDF$diff_ci_low_85[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[4])
    treatDF$diff_ci_high_85[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.85)$bca[5])
    
    treatDF$diff_ci_low_75[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[4])
    treatDF$diff_ci_high_75[treatDF$terms == "Occluded P Pool"] <- as.numeric(boot.ci(results, type="bca", index=2, conf=0.75)$bca[5])
    
    
    ### save csv
    write.csv(treatDF, paste0("output/summary_tables/summary_table_P_pool_", norm, 
                              ".csv"), 
              row.names=F)
    
    
    
    ##### output tables
    return(treatDF)
      
}

