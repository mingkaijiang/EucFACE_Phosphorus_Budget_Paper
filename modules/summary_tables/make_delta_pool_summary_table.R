
make_delta_pool_summary_table <- function(norm,
                                          soil_p_pool,
                                          #soil_inorganic_p_pool,
                                          #soil_organic_p_pool,
                                          soil_phosphate_pool,
                                          #soil_p_pool_hedley,
                                          microbial_p_pool,
                                          canopy_p_pool,
                                          leaflitter_p_pool,
                                          wood_p_pool,
                                          sapwood_p_pool,
                                          heartwood_p_pool,
                                          #standing_dead_p_pool,
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
               "Microbial P Pool",
               "Microbial P Pool 0-10cm", 
               "Microbial P Pool 10-30cm", 
               "Microbial P Pool 30-60cm", 
               "Soil Phosphate P Pool",
               "Soil Phosphate P Pool 0-10cm",
               "Soil Phosphate P Pool 10-30cm",
               "Soil P Pool 0-10cm",
               "Soil P Pool 10-30cm")
    
    treatDF <- data.frame(terms)
    treatDF$R1 <- rep(NA, length(treatDF$terms))
    treatDF$R2 <- rep(NA, length(treatDF$terms))
    treatDF$R3 <- rep(NA, length(treatDF$terms))
    treatDF$R4 <- rep(NA, length(treatDF$terms))
    treatDF$R5 <- rep(NA, length(treatDF$terms))
    treatDF$R6 <- rep(NA, length(treatDF$terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$diff <- rep(NA, length(treatDF$terms))
    treatDF$percent_diff <- rep(NA, length(treatDF$terms))
    
    treatDF$year_start <- rep(NA, length(treatDF$terms))
    treatDF$year_end <- rep(NA, length(treatDF$terms))
    treatDF$timepoint <- rep(NA, length(treatDF$terms))
    treatDF$notes <- rep(NA, length(treatDF$terms))
    
    ### Canopy P 
    out <- summaryBy(delta~Ring,data=canopy_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy P Pool", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Canopy P Pool"] <- min(canopy_p_pool$Date)  
    treatDF$year_end[treatDF$terms == "Canopy P Pool"] <- max(canopy_p_pool$Date)   
    treatDF$timepoint[treatDF$terms == "Canopy P Pool"] <- length(unique(canopy_p_pool$Date))  

    ### Forestfloor Leaf litter
    out <- summaryBy(delta~Ring,data=leaflitter_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Forestfloor Leaf Litter P Pool", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- min(leaflitter_p_pool$Date)    
    treatDF$year_end[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- max(leaflitter_p_pool$Date)  
    treatDF$timepoint[treatDF$terms == "Forestfloor Leaf Litter P Pool"] <- length(unique(leaflitter_p_pool$Date))  

    ### Wood P 
    out <- summaryBy(delta~Ring,data=wood_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Total Wood P Pool", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Total Wood P Pool"] <- min(wood_p_pool$Date)   
    treatDF$year_end[treatDF$terms == "Total Wood P Pool"] <- max(wood_p_pool$Date)    
    treatDF$timepoint[treatDF$terms == "Total Wood P Pool"] <- length(unique(wood_p_pool$Date)) 

    ### Sapwood P 
    out <- summaryBy(delta~Ring,data=sapwood_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Sapwood P Pool", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Sapwood P Pool"] <- min(wood_p_pool$Date)    
    treatDF$year_end[treatDF$terms == "Sapwood P Pool"] <- max(wood_p_pool$Date)    
    treatDF$timepoint[treatDF$terms == "Sapwood P Pool"] <- length(unique(wood_p_pool$Date)) 

    ### Heartwood P 
    out <- summaryBy(delta~Ring,data=heartwood_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Heartwood P Pool", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Heartwood P Pool"] <- min(wood_p_pool$Date)    
    treatDF$year_end[treatDF$terms == "Heartwood P Pool"] <- max(wood_p_pool$Date)    
    treatDF$timepoint[treatDF$terms == "Heartwood P Pool"] <- length(unique(wood_p_pool$Date)) 

    
    ### Fine root P pool
    out <- summaryBy(delta~Ring,data=fineroot_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root P Pool", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Fine Root P Pool"] <- min(fineroot_p_pool$Date)    
    treatDF$year_end[treatDF$terms == "Fine Root P Pool"] <- max(fineroot_p_pool$Date)    
    treatDF$timepoint[treatDF$terms == "Fine Root P Pool"] <- length(unique(fineroot_p_pool$Date))  

    ### Coarse root P pool
    out <- summaryBy(delta~Ring,data=coarse_root_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Coarse Root P Pool", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Coarse Root P Pool"] <- min(coarse_root_p_pool$Date)   
    treatDF$year_end[treatDF$terms == "Coarse Root P Pool"] <- max(coarse_root_p_pool$Date)    
    treatDF$timepoint[treatDF$terms == "Coarse Root P Pool"] <- length(unique(coarse_root_p_pool$Date))  

    ### Understorey P pool
    out <- summaryBy(delta~Ring,data=understorey_p_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey P Pool", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Understorey P Pool"] <- min(understorey_p_pool$Date)    
    treatDF$year_end[treatDF$terms == "Understorey P Pool"] <- max(understorey_p_pool$Date)    
    treatDF$timepoint[treatDF$terms == "Understorey P Pool"] <- length(unique(understorey_p_pool$Date))  

    
    ### Microbial P pool
    out <- summaryBy(delta~Ring+Date,data=microbial_p_pool,FUN=sum,keep.names=T,na.rm=T)
    out2 <- summaryBy(delta~Ring,data=out,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool", 2:7] <- out2$delta
    treatDF$year_start[treatDF$terms == "Microbial P Pool"] <- NA   
    treatDF$year_end[treatDF$terms == "Microbial P Pool"] <- NA
    treatDF$timepoint[treatDF$terms == "Microbial P Pool"] <- NA 

    
    out <- summaryBy(delta~Ring,data=microbial_p_pool[microbial_p_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool 0-10cm", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Microbial P Pool 0-10cm"] <- min(microbial_p_pool$Date[microbial_p_pool$Depth=="0_10"])    
    treatDF$year_end[treatDF$terms == "Microbial P Pool 0-10cm"] <- max(microbial_p_pool$Date[microbial_p_pool$Depth=="0_10"])   
    treatDF$timepoint[treatDF$terms == "Microbial P Pool 0-10cm"] <- length(unique(microbial_p_pool$Date[microbial_p_pool$Depth=="0_10"]))  

    
    ### Microbial P pool
    out <- summaryBy(delta~Ring,data=microbial_p_pool[microbial_p_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool 10-30cm", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Microbial P Pool 10-30cm"] <- min(microbial_p_pool$Date[microbial_p_pool$Depth=="10_30"])    
    treatDF$year_end[treatDF$terms == "Microbial P Pool 10-30cm"] <- max(microbial_p_pool$Date[microbial_p_pool$Depth=="10_30"])    
    treatDF$timepoint[treatDF$terms == "Microbial P Pool 10-30cm"] <- length(unique(microbial_p_pool$Date[microbial_p_pool$Depth=="10_30"]))  

    
    ### Microbial P pool
    out <- summaryBy(delta~Ring,data=microbial_p_pool[microbial_p_pool$Depth=="transition",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial P Pool 30-60cm", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Microbial P Pool 30-60cm"] <- min(microbial_p_pool$Date[microbial_p_pool$Depth=="transition"])    
    treatDF$year_end[treatDF$terms == "Microbial P Pool 30-60cm"] <- max(microbial_p_pool$Date[microbial_p_pool$Depth=="transition"])    
    treatDF$timepoint[treatDF$terms == "Microbial P Pool 30-60cm"] <- length(unique(microbial_p_pool$Date[microbial_p_pool$Depth=="transition"]))  

    
    ### Soil Phosphate P pool
    out <- summaryBy(delta~Ring+Date,data=soil_phosphate_pool,FUN=sum,keep.names=T,na.rm=T)
    out2 <- summaryBy(delta~Ring,data=out,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Phosphate P Pool", 2:7] <- out2$delta
    treatDF$year_start[treatDF$terms == "Soil Phosphate P Pool"] <- NA   
    treatDF$year_end[treatDF$terms == "Soil Phosphate P Pool"] <- NA
    treatDF$timepoint[treatDF$terms == "Soil Phosphate P Pool"] <- NA
    
    
    out <- summaryBy(delta~Ring,data=soil_phosphate_pool[soil_phosphate_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Phosphate P Pool 0-10cm", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- min(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="0_10"])    
    treatDF$year_end[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- max(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="0_10"])   
    treatDF$timepoint[treatDF$terms == "Soil Phosphate P Pool 0-10cm"] <- length(unique(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="0_10"]))  

    ### Soil Phosphate P pool
    out <- summaryBy(delta~Ring,data=soil_phosphate_pool[soil_phosphate_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Phosphate P Pool 10-30cm", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- min(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="10_30"])  
    treatDF$year_end[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- max(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="10_30"])    
    treatDF$timepoint[treatDF$terms == "Soil Phosphate P Pool 10-30cm"] <- length(unique(soil_phosphate_pool$Date[soil_phosphate_pool$Depth=="10_30"]))  

    
    ### Soil P pool
    out <- summaryBy(delta~Ring,data=soil_p_pool[soil_p_pool$Depth=="0_10",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil P Pool 0-10cm", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Soil P Pool 0-10cm"] <- min(soil_p_pool$Date[soil_p_pool$Depth=="0_10"])    
    treatDF$year_end[treatDF$terms == "Soil P Pool 0-10cm"] <- max(soil_p_pool$Date[soil_p_pool$Depth=="0_10"])  
    treatDF$timepoint[treatDF$terms == "Soil P Pool 0-10cm"] <- length(unique(soil_p_pool$Date[soil_p_pool$Depth=="0_10"]))  

    
    ### Soil P pool
    out <- summaryBy(delta~Ring,data=soil_p_pool[soil_p_pool$Depth=="10_30",],FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil P Pool 10-30cm", 2:7] <- out$delta
    treatDF$year_start[treatDF$terms == "Soil P Pool 10-30cm"] <- min(soil_p_pool$Date[soil_p_pool$Depth=="10_30"])    
    treatDF$year_end[treatDF$terms == "Soil P Pool 10-30cm"] <- max(soil_p_pool$Date[soil_p_pool$Depth=="10_30"])   
    treatDF$timepoint[treatDF$terms == "Soil P Pool 10-30cm"] <- length(unique(soil_p_pool$Date[soil_p_pool$Depth=="10_30"]))  

    
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 6)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 6)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 6)
    
    ### se of the diff
    treatDF$diff_se <- sqrt((treatDF$aCO2_sd^2+treatDF$eCO2_sd^2)/2) * (sqrt(2/3))
    
    ### confidence interval of the diff
    treatDF$diff_cf <- qt(0.975, 4) * treatDF$diff_se
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
    
    write.csv(treatDF, paste0("output/summary_tables/summary_table_delta_P_pool_", norm, 
                              ".csv"), 
              row.names=F)
    
    
    ### plot
    tmpDF1 <- treatDF[,c("terms", "aCO2", "eCO2")]
    tmpDF2 <- treatDF[,c("terms", "aCO2_sd", "eCO2_sd")]
    
    plotDF1 <- reshape::melt(tmpDF1, id.var="terms")
    plotDF2 <- reshape::melt(tmpDF2, id.var="terms")
    colnames(plotDF2) <- c("terms", "variable", "value_sd")
    plotDF2$variable <- gsub("_sd", "", plotDF2$variable)
    
    plotDF <- merge(plotDF1, plotDF2, by=c("terms", "variable"))
    plotDF$terms <- gsub(" P Pool", "", plotDF$terms)
    
    p1 <- ggplot(plotDF, aes(terms, value, group=variable))+
      geom_errorbar(aes(ymin=value-value_sd, ymax=value+value_sd),
                    position=position_dodge2(), width=0.3)+
      geom_point(aes(fill=variable), pch=21, stat = "identity", size=2,
                 position=position_dodge2(width=0.3)) +
      coord_flip()+
      scale_fill_manual(name="",
                        values=c("aCO2"="blue3",
                                 "eCO2"="red2"))
    
    #pdf(paste0("output/checks/delta_P_pool_comparison.pdf"))
    #plot(p1)
    #dev.off()
    
    ##### output tables
    return(treatDF)
      
}

