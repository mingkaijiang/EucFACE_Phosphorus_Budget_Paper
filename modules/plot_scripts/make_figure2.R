make_figure2 <- function(inDF) {
    
    ### extract hedley data
    tmpDF <- inDF[inDF$terms%in%c("Exchangeable Pi Pool", "Exchangeable Po Pool",
                                  "Moderately labile Po Pool", #"Secondary Fe bound Pi Pool",
                                  #"Primary Ca bound Pi Pool", 
                                  "Occluded P Pool"),]
    
    subDF1 <- data.frame(tmpDF$terms, tmpDF$aCO2, tmpDF$aCO2_sd)
    subDF2 <- data.frame(tmpDF$terms, tmpDF$eCO2, tmpDF$eCO2_sd)
    colnames(subDF1) <- colnames(subDF2) <- c("terms", "mean", "sd")
    hedDF <- rbind(subDF1, subDF2)
    
    hedDF$Pool <- rep(c("1_Exchangeable_Pi", "2_Exchangeable_Po", "3_Moderately_labile_Po",
                          #"4_Secondary_Pi", "5_Primary_Pi", 
                        "6_Occluded_P"), 2)
    hedDF$Trt <- rep(c("amb", "ele"), each=4)
    
    ### calculate totals
    hedsumDF <- summaryBy(mean~Trt, FUN=sum, data=hedDF, na.rm=T, keep.names=T)
    
    for (i in c("amb", "ele")) {
        hedDF$frac[hedDF$Trt==i] <- hedDF$mean[hedDF$Trt==i] / hedsumDF$mean[hedsumDF$Trt==i]
    }
    

    
    ### vegetation P
    vegDF <- inDF[inDF$terms%in%c("Canopy P Pool", "Total Wood P Pool", "Forestfloor Leaf Litter P Pool",
                                  "Fine Root P Pool", "Coarse Root P Pool", "Understorey P Pool",
                                  #"Understorey Litter P Pool", 
                                  "Standing Dead Wood P Pool"),]
    
    soilDF1 <- inDF[inDF$terms%in%c("Soil P Pool 0-10cm", "Soil P Pool 10-30cm", 
                                   "Soil P Pool 30-60cm"),]

    
    totDF <- inDF[inDF$terms%in%c("Canopy P Pool", 
                                  "Total Wood P Pool", 
                                  "Sapwood P Pool", 
                                  "Heartwood P Pool", 
                                  "Forestfloor Leaf Litter P Pool",
                                  "Fine Root P Pool", 
                                  "Coarse Root P Pool", 
                                  "Understorey P Pool",
                                  "Understorey Litter P Pool", 
                                  "Standing Dead Wood P Pool",
                                  "Soil P Pool 0-10cm", 
                                  "Soil P Pool 10-30cm", 
                                  "Soil P Pool 30-60cm",
                                  "Soil Inorg P Pool 0-10cm", 
                                  "Soil Inorg P Pool 10-30cm", 
                                  "Soil Inorg P Pool 30-60cm",
                                  "Soil Org P Pool 0-10cm", 
                                  "Soil Org P Pool 10-30cm", 
                                  "Soil Org P Pool 30-60cm",
                                  "Soil Phosphate P Pool 0-10cm", 
                                  "Soil Phosphate P Pool 10-30cm", 
                                  "Soil Phosphate P Pool 30-60cm",
                                  "Microbial P Pool 0-10cm",
                                  "Microbial P Pool 10-30cm",
                                  "Microbial P Pool 30-60cm"),]
    
    
    
    
    plotDF <- data.frame(c(totDF$aCO2[totDF$terms=="Canopy P Pool"], 
                           totDF$eCO2[totDF$terms=="Canopy P Pool"],
                           totDF$aCO2[totDF$terms=="Total Wood P Pool"], 
                           totDF$eCO2[totDF$terms=="Total Wood P Pool"],
                           totDF$aCO2[totDF$terms=="Sapwood P Pool"], 
                           totDF$eCO2[totDF$terms=="Sapwood P Pool"],
                           totDF$aCO2[totDF$terms=="Heartwood P Pool"], 
                           totDF$eCO2[totDF$terms=="Heartwood P Pool"],
                           totDF$aCO2[totDF$terms=="Forestfloor Leaf Litter P Pool"], 
                           totDF$eCO2[totDF$terms=="Forestfloor Leaf Litter P Pool"],
                           totDF$aCO2[totDF$terms=="Fine Root P Pool"], 
                           totDF$eCO2[totDF$terms=="Fine Root P Pool"],
                           totDF$aCO2[totDF$terms=="Coarse Root P Pool"], 
                           totDF$eCO2[totDF$terms=="Coarse Root P Pool"],
                           totDF$aCO2[totDF$terms=="Understorey P Pool"], 
                           totDF$eCO2[totDF$terms=="Understorey P Pool"],
                           totDF$aCO2[totDF$terms=="Understorey Litter P Pool"], 
                           totDF$eCO2[totDF$terms=="Understorey Litter P Pool"],
                           totDF$aCO2[totDF$terms=="Standing Dead Wood P Pool"], 
                           totDF$eCO2[totDF$terms=="Standing Dead Wood P Pool"],
                           totDF$aCO2[totDF$terms=="Soil P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Soil P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Soil P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Soil P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Soil P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Soil P Pool 30-60cm"],
                           totDF$aCO2[totDF$terms=="Soil Inorg P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Soil Inorg P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Soil Inorg P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Soil Inorg P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Soil Inorg P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Soil Inorg P Pool 30-60cm"],
                           totDF$aCO2[totDF$terms=="Soil Org P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Soil Org P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Soil Org P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Soil Org P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Soil Org P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Soil Org P Pool 30-60cm"],
                           totDF$aCO2[totDF$terms=="Soil Phosphate P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Soil Phosphate P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Soil Phosphate P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Soil Phosphate P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Soil Phosphate P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Soil Phosphate P Pool 30-60cm"],
                           totDF$aCO2[totDF$terms=="Microbial P Pool 0-10cm"], 
                           totDF$eCO2[totDF$terms=="Microbial P Pool 0-10cm"],
                           totDF$aCO2[totDF$terms=="Microbial P Pool 10-30cm"], 
                           totDF$eCO2[totDF$terms=="Microbial P Pool 10-30cm"],
                           totDF$aCO2[totDF$terms=="Microbial P Pool 30-60cm"], 
                           totDF$eCO2[totDF$terms=="Microbial P Pool 30-60cm"]), 
                         NA, NA)
    colnames(plotDF) <- c("mean", "sd", "Variable")
    plotDF$sd <- c(totDF$aCO2_sd[totDF$terms=="Canopy P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Canopy P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Total Wood P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Total Wood P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Sapwood P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Sapwood P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Heartwood P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Heartwood P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Forestfloor Leaf Litter P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Forestfloor Leaf Litter P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Fine Root P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Fine Root P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Coarse Root P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Coarse Root P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Understorey P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Understorey P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Understorey Litter P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Understorey Litter P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Standing Dead Wood P Pool"], 
                   totDF$eCO2_sd[totDF$terms=="Standing Dead Wood P Pool"],
                   totDF$aCO2_sd[totDF$terms=="Soil P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil P Pool 30-60cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Inorg P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Inorg P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Inorg P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Inorg P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Inorg P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Inorg P Pool 30-60cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Org P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Org P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Org P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Org P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Org P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Org P Pool 30-60cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Phosphate P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Phosphate P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Phosphate P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Phosphate P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Soil Phosphate P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Soil Phosphate P Pool 30-60cm"],
                   totDF$aCO2_sd[totDF$terms=="Microbial P Pool 0-10cm"], 
                   totDF$eCO2_sd[totDF$terms=="Microbial P Pool 0-10cm"],
                   totDF$aCO2_sd[totDF$terms=="Microbial P Pool 10-30cm"], 
                   totDF$eCO2_sd[totDF$terms=="Microbial P Pool 10-30cm"],
                   totDF$aCO2_sd[totDF$terms=="Microbial P Pool 30-60cm"], 
                   totDF$eCO2_sd[totDF$terms=="Microbial P Pool 30-60cm"])
    plotDF$Variable <- rep(c("Canopy P Pool",
                             "Total Wood P Pool",
                             "Sapwood P Pool",
                             "Heartwood P Pool",
                             "Forestfloor Leaf Litter P Pool",
                             "Fine Root P Pool",
                             "Coarse Root P Pool",
                             "Understorey P Pool",
                             "Understorey Litter P Pool",
                             "Standing Dead Wood P Pool",
                             "Soil P Pool 0-10cm",
                             "Soil P Pool 10-30cm",
                             "Soil P Pool 30-60cm",
                             "Soil Inorg P Pool 0-10cm",
                             "Soil Inorg P Pool 10-30cm",
                             "Soil Inorg P Pool 30-60cm",
                             "Soil Org P Pool 0-10cm",
                             "Soil Org P Pool 10-30cm",
                             "Soil Org P Pool 30-60cm",
                             "Soil Phosphate P Pool 0-10cm",
                             "Soil Phosphate P Pool 10-30cm",
                             "Soil Phosphate P Pool 30-60cm",
                             "Microbial P Pool 0-10cm",
                             "Microbial P Pool 10-30cm",
                             "Microbial P Pool 30-60cm"), 
                           each=2)
    plotDF$Trt <- rep(c("aCO2", "eCO2"), 25)
    plotDF$pos <- with(plotDF, mean + sd)
    plotDF$neg <- with(plotDF, mean - sd)
    plotDF$sd2 <- plotDF$sd^2
    
    plotDF$Cat1 <- 1
    
    plotDF$Cat1[plotDF$Variable%in%c("Canopy P Pool",
                                     "Total Wood P Pool",
                                     "Forestfloor Leaf Litter P Pool",
                                     "Fine Root P Pool",
                                     "Coarse Root P Pool",
                                     "Understorey P Pool",
                                     "Standing Dead Wood P Pool")] <- "Veg"
    
    plotDF$Cat1[plotDF$Variable%in%c("Sapwood P Pool",
                                     "Heartwood P Pool",
                                     "Understorey Litter P Pool",
                                     "Soil P Pool 0-10cm",
                                     "Soil P Pool 10-30cm",
                                     "Soil P Pool 30-60cm",
                                     "Soil Phosphate P Pool 0-10cm",
                                     "Soil Phosphate P Pool 10-30cm",
                                     "Soil Phosphate P Pool 30-60cm",
                                     "Microbial P Pool 0-10cm",
                                     "Microbial P Pool 10-30cm",
                                     "Microbial P Pool 30-60cm")] <- "NA"
    
    plotDF$Cat1[plotDF$Variable%in%c("Soil Inorg P Pool 0-10cm",
                                     "Soil Inorg P Pool 10-30cm",
                                     "Soil Inorg P Pool 30-60cm")] <- "Soil_Inorg"
    
    plotDF$Cat1[plotDF$Variable%in%c("Soil Org P Pool 0-10cm",
                                     "Soil Org P Pool 10-30cm",
                                     "Soil Org P Pool 30-60cm")] <- "Soil_Org"
    
    
    plotDF$Cat2 <- 2
    
    plotDF$Cat2[plotDF$Variable%in%c("Canopy P Pool",
                                     "Sapwood P Pool",
                                     "Heartwood P Pool",
                                     "Forestfloor Leaf Litter P Pool",
                                     "Fine Root P Pool",
                                     "Coarse Root P Pool",
                                     "Understorey P Pool",
                                     "Standing Dead Wood P Pool")] <- "Veg"
    
    plotDF$Cat2[plotDF$Variable%in%c("Total Wood P Pool",
                                     "Understorey Litter P Pool",
                                     "Soil P Pool 0-10cm",
                                     "Soil P Pool 10-30cm",
                                     "Soil P Pool 30-60cm",
                                     #"Standing Dead Wood P Pool",
                                     "Soil Phosphate P Pool 0-10cm",
                                     "Soil Phosphate P Pool 10-30cm",
                                     "Soil Phosphate P Pool 30-60cm",
                                     "Microbial P Pool 0-10cm",
                                     "Microbial P Pool 10-30cm",
                                     "Microbial P Pool 30-60cm")] <- "NA"
    
    plotDF$Cat2[plotDF$Variable%in%c("Soil Inorg P Pool 0-10cm",
                                     "Soil Inorg P Pool 10-30cm",
                                     "Soil Inorg P Pool 30-60cm")] <- "Soil_Inorg"
    
    plotDF$Cat2[plotDF$Variable%in%c("Soil Org P Pool 0-10cm",
                                     "Soil Org P Pool 10-30cm",
                                     "Soil Org P Pool 30-60cm")] <- "Soil_Org"
    
    
    
    ### individiual ring data
    tmpDF <- totDF[c("terms", "R1", "R2", "R3", "R4", "R5", "R6")]
    plotDFi <- reshape2::melt(tmpDF, id.vars="terms", variable.name="Ring")
    plotDFi$Trt[plotDFi$Ring%in%c("R1", "R4", "R5")] <- "eCO2"
    plotDFi$Trt[plotDFi$Ring%in%c("R2", "R3", "R6")] <- "aCO2"
    plotDFi$Cat1 <- 1
    
    plotDFi$Cat1[plotDFi$terms%in%c("Canopy P Pool",
                                    "Total Wood P Pool",
                                    "Forestfloor Leaf Litter P Pool",
                                    "Fine Root P Pool",
                                    "Coarse Root P Pool",
                                    "Understorey P Pool",
                                    "Standing Dead Wood P Pool")] <- "Veg"
    
    plotDFi$Cat1[plotDFi$terms%in%c("Sapwood P Pool",
                                    "Heartwood P Pool",
                                    "Understorey Litter P Pool",
                                    "Soil P Pool 0-10cm",
                                    "Soil P Pool 10-30cm",
                                    "Soil P Pool 30-60cm",
                                    "Soil Phosphate P Pool 0-10cm",
                                    "Soil Phosphate P Pool 10-30cm",
                                    "Soil Phosphate P Pool 30-60cm",
                                    "Microbial P Pool 0-10cm",
                                    "Microbial P Pool 10-30cm",
                                    "Microbial P Pool 30-60cm")] <- "NA"
    
    plotDFi$Cat1[plotDFi$terms%in%c("Soil Inorg P Pool 0-10cm",
                                    "Soil Inorg P Pool 10-30cm",
                                    "Soil Inorg P Pool 30-60cm")] <- "Soil_Inorg"
    
    plotDFi$Cat1[plotDFi$terms%in%c("Soil Org P Pool 0-10cm",
                                    "Soil Org P Pool 10-30cm",
                                    "Soil Org P Pool 30-60cm")] <- "Soil_Org"
    
    
    plotDFi$Cat2 <- 2
    
    plotDFi$Cat2[plotDFi$terms%in%c("Canopy P Pool",
                                    "Sapwood P Pool",
                                    "Heartwood P Pool",
                                    "Forestfloor Leaf Litter P Pool",
                                    "Fine Root P Pool",
                                    "Coarse Root P Pool",
                                    "Understorey P Pool",
                                    "Standing Dead Wood P Pool")] <- "Veg"
    
    plotDFi$Cat2[plotDFi$terms%in%c("Total Wood P Pool",
                                    "Understorey Litter P Pool",
                                    "Soil P Pool 0-10cm",
                                    "Soil P Pool 10-30cm",
                                    "Soil P Pool 30-60cm",
                                    "Soil Phosphate P Pool 0-10cm",
                                    "Soil Phosphate P Pool 10-30cm",
                                    "Soil Phosphate P Pool 30-60cm",
                                    "Microbial P Pool 0-10cm",
                                    "Microbial P Pool 10-30cm",
                                    "Microbial P Pool 30-60cm")] <- "NA"
    
    plotDFi$Cat2[plotDFi$terms%in%c("Soil Inorg P Pool 0-10cm",
                                    "Soil Inorg P Pool 10-30cm",
                                    "Soil Inorg P Pool 30-60cm")] <- "Soil_Inorg"
    
    plotDFi$Cat2[plotDFi$terms%in%c("Soil Org P Pool 0-10cm",
                                    "Soil Org P Pool 10-30cm",
                                    "Soil Org P Pool 30-60cm")] <- "Soil_Org"
    
    
    
    ### total ecosystem P pool
    subDF1 <- plotDFi[plotDFi$Cat1!="NA",]
    plotDF1i <- summaryBy(value~Cat1+Ring+Trt, FUN=sum, data=subDF1, keep.names=T, na.rm=T)
    plotDF1 <- summaryBy(value~Cat1+Trt, FUN=c(mean,sd), data=plotDF1i, keep.names=T, na.rm=T)
    
    totDF1i <- summaryBy(value~Ring+Trt, data=plotDF1i, FUN=sum, keep.names=T, na.rm=T)
    totDF1 <- summaryBy(value~Trt, data=totDF1i, FUN=c(mean,sd), keep.names=T, na.rm=T)
    
    
    ### vegetation + litter P pool
    subDF2 <- plotDFi[plotDFi$Cat2%in%c("Veg", "Lit"),]
    
    plotDF2i <- summaryBy(value~Trt+Ring, FUN=sum, data=subDF2, keep.names=T, na.rm=T)
    plotDF2 <- summaryBy(value~Trt+terms, FUN=c(mean,sd), data=subDF2, keep.names=T, na.rm=T)
    
    totDF2 <- summaryBy(value~Trt, FUN=c(mean,sd), data=plotDF2i, keep.names=T, na.rm=T)
    totDF2i <- summaryBy(value~Ring+Trt, data=plotDF2i, FUN=sum, keep.names=T, na.rm=T)
    
    
    
    ### 0-10 cm soil
    subDF3i <- plotDFi[plotDFi$terms%in%c("Soil Inorg P Pool 0-10cm",
                                          "Soil Org P Pool 0-10cm",
                                          "Microbial P Pool 0-10cm",
                                          "Soil Phosphate P Pool 0-10cm"),]
    
    tmpDFi <- subDF3i[subDF3i$terms=="Soil Org P Pool 0-10cm",]
    tmpDFi$terms <- "Soil Org Residual P Pool 0-10cm"
    subDF3i <- rbind(subDF3i, tmpDFi)
    
    tmpDFi <- subDF3i[subDF3i$terms=="Soil Inorg P Pool 0-10cm",]
    tmpDFi$terms <- "Soil Inorg Residual P Pool 0-10cm"
    subDF3i <- rbind(subDF3i, tmpDFi)
    
    
    for (i in c("R1", "R2", "R3", "R4", "R5", "R6")) {
        subDF3i$value[subDF3i$terms=="Soil Org Residual P Pool 0-10cm"&subDF3i$Ring==i] <- (subDF3i$value[subDF3i$terms=="Soil Org P Pool 0-10cm"&subDF3i$Ring==i] -
                                                                                              subDF3i$value[subDF3i$terms=="Microbial P Pool 0-10cm"&subDF3i$Ring==i])
        
        subDF3i$value[subDF3i$terms=="Soil Inorg Residual P Pool 0-10cm"&subDF3i$Ring==i] <- (subDF3i$value[subDF3i$terms=="Soil Inorg P Pool 0-10cm"&subDF3i$Ring==i] -
                                                                                                subDF3i$value[subDF3i$terms=="Soil Phosphate P Pool 0-10cm"&subDF3i$Ring==i])
        
    }
    
    subDF3i <- subDF3i[subDF3i$terms!="Soil Org P Pool 0-10cm",]
    subDF3i <- subDF3i[subDF3i$terms!="Soil Inorg P Pool 0-10cm",]
    
    totDF3i <- plotDFi[plotDFi$terms=="Soil P Pool 0-10cm",]
    
    subDF3 <- summaryBy(value~terms+Trt, data=subDF3i, FUN=c(mean,sd), na.rm=T, keep.names=T)
    totDF3 <- summaryBy(value~Trt, data=totDF3i, FUN=c(mean,sd), na.rm=T, keep.names=T)
    
    
    
    ### 10-30 cm soil
    subDF4i <- plotDFi[plotDFi$terms%in%c("Soil Inorg P Pool 10-30cm",
                                          "Soil Org P Pool 10-30cm",
                                          "Microbial P Pool 10-30cm",
                                          "Soil Phosphate P Pool 10-30cm"),]
    
    tmpDFi <- subDF4i[subDF4i$terms=="Soil Org P Pool 10-30cm",]
    tmpDFi$terms <- "Soil Org Residual P Pool 10-30cm"
    subDF4i <- rbind(subDF4i, tmpDFi)
    
    tmpDFi <- subDF4i[subDF4i$terms=="Soil Inorg P Pool 10-30cm",]
    tmpDFi$terms <- "Soil Inorg Residual P Pool 10-30cm"
    subDF4i <- rbind(subDF4i, tmpDFi)
    
    
    for (i in c("R1", "R2", "R3", "R4", "R5", "R6")) {
        subDF4i$value[subDF4i$terms=="Soil Org Residual P Pool 10-30cm"&subDF4i$Ring==i] <- (subDF4i$value[subDF4i$terms=="Soil Org P Pool 10-30cm"&subDF4i$Ring==i] -
                                                                                               subDF4i$value[subDF4i$terms=="Microbial P Pool 10-30cm"&subDF4i$Ring==i])
        
        subDF4i$value[subDF4i$terms=="Soil Inorg Residual P Pool 10-30cm"&subDF4i$Ring==i] <- (subDF4i$value[subDF4i$terms=="Soil Inorg P Pool 10-30cm"&subDF4i$Ring==i] -
                                                                                                 subDF4i$value[subDF4i$terms=="Soil Phosphate P Pool 10-30cm"&subDF4i$Ring==i])
        
    }
    
    subDF4i <- subDF4i[subDF4i$terms!="Soil Org P Pool 10-30cm",]
    subDF4i <- subDF4i[subDF4i$terms!="Soil Inorg P Pool 10-30cm",]
    
    totDF4i <- plotDFi[plotDFi$terms=="Soil P Pool 10-30cm",]
    
    subDF4 <- summaryBy(value~terms+Trt, data=subDF4i, FUN=c(mean,sd), na.rm=T, keep.names=T)
    totDF4 <- summaryBy(value~Trt, data=totDF4i, FUN=c(mean,sd), na.rm=T, keep.names=T)
    
    
    
    
    ### 30-60 cm soil
    subDF5i <- plotDFi[plotDFi$terms%in%c("Soil Inorg P Pool 30-60cm",
                                             "Soil Org P Pool 30-60cm",
                                             "Microbial P Pool 30-60cm",
                                             "Soil Phosphate P Pool 30-60cm"),]
    
    tmpDFi <- subDF5i[subDF5i$terms=="Soil Org P Pool 30-60cm",]
    tmpDFi$terms <- "Soil Org Residual P Pool 30-60cm"
    subDF5i <- rbind(subDF5i, tmpDFi)
    
    tmpDFi <- subDF5i[subDF5i$terms=="Soil Inorg P Pool 30-60cm",]
    tmpDFi$terms <- "Soil Inorg Residual P Pool 30-60cm"
    subDF5i <- rbind(subDF5i, tmpDFi)
    
    for (i in c("R1", "R2", "R3", "R4", "R5", "R6")) {
        subDF5i$value[subDF5i$terms=="Soil Org Residual P Pool 30-60cm"&subDF5i$Ring==i] <- (subDF5i$value[subDF5i$terms=="Soil Org P Pool 30-60cm"&subDF5i$Ring==i] -
                                                                                               subDF5i$value[subDF5i$terms=="Microbial P Pool 30-60cm"&subDF5i$Ring==i])
        
        subDF5i$value[subDF5i$terms=="Soil Inorg Residual P Pool 30-60cm"&subDF5i$Ring==i] <- (subDF5i$value[subDF5i$terms=="Soil Inorg P Pool 30-60cm"&subDF5i$Ring==i] - 
                                                                                                    subDF5i$value[subDF5i$terms=="Soil Phosphate P Pool 30-60cm"&subDF5i$Ring==i])
        
        
    }
    
    subDF5i <- subDF5i[subDF5i$terms!="Soil Org P Pool 30-60cm",]
    subDF5i <- subDF5i[subDF5i$terms!="Soil Inorg P Pool 30-60cm",]
    
    totDF5i <- plotDFi[plotDFi$terms=="Soil P Pool 30-60cm",]
    
    subDF5 <- summaryBy(value~terms+Trt, data=subDF5i, FUN=c(mean,sd), na.rm=T, keep.names=T)
    totDF5 <- summaryBy(value~Trt, data=totDF5i, FUN=c(mean,sd), na.rm=T, keep.names=T)
    
    
    ### order labels
    plotDF1$Cat1 <- gsub("Veg", "1_Veg", plotDF1$Cat1)
    
    plotDF2$terms <- gsub("Canopy P Pool", "1_Canopy P Pool", plotDF2$terms)
    plotDF2$terms <- gsub("Understorey P Pool", "2_Understorey P Pool", plotDF2$terms)
    plotDF2$terms <- gsub("Fine Root P Pool", "3_Fine Root P Pool", plotDF2$terms)
    plotDF2$terms <- gsub("Coarse Root P Pool", "4_Coarse Root P Pool", plotDF2$terms)
    plotDF2$terms <- gsub("Heartwood P Pool", "5_Heartwood P Pool", plotDF2$terms)
    plotDF2$terms <- gsub("Sapwood P Pool", "6_Sapwood P Pool", plotDF2$terms)
    plotDF2$terms <- gsub("Standing Dead Wood P Pool", "7_Standing Dead Wood P Pool", plotDF2$terms)
    plotDF2$terms <- gsub("Forestfloor Leaf Litter P Pool", "8_Forestfloor Leaf Litter P Pool", plotDF2$terms)

    
    
    subDF3$terms <- gsub("Microbial P Pool 0-10cm", "1_Microbial P Pool 0-10cm", subDF3$terms)
    subDF3$terms <- gsub("Soil Org Residual P Pool 0-10cm", "2_Soil Org Residual P Pool 0-10cm", subDF3$terms)
    subDF3$terms <- gsub("Soil Inorg Residual P Pool 0-10cm", "4_Soil Inorg P Pool 0-10cm", subDF3$terms)
    subDF3$terms <- gsub("Soil Phosphate P Pool 0-10cm", "3_Soil Phosphate P Pool 0-10cm", subDF3$terms)
    
    
    subDF4$terms <- gsub("Microbial P Pool 10-30cm", "1_Microbial P Pool 10-30cm", subDF4$terms)
    subDF4$terms <- gsub("Soil Org Residual P Pool 10-30cm", "2_Soil Org Residual P Pool 10-30cm", subDF4$terms)
    subDF4$terms <- gsub("Soil Inorg Residual P Pool 10-30cm", "4_Soil Inorg P Pool 10-30cm", subDF4$terms)
    subDF4$terms <- gsub("Soil Phosphate P Pool 10-30cm", "3_Soil Phosphate P Pool 10-30cm", subDF4$terms)
    
    
    subDF5$terms <- gsub("Microbial P Pool 30-60cm", "1_Microbial P Pool 30-60cm", subDF5$terms)
    subDF5$terms <- gsub("Soil Org Residual P Pool 30-60cm", "2_Soil Org Residual P Pool 30-60cm", subDF5$terms)
    subDF5$terms <- gsub("Soil Inorg Residual P Pool 30-60cm", "4_Soil Inorg P Pool 30-60cm", subDF5$terms)
    subDF5$terms <- gsub("Soil Phosphate P Pool 30-60cm", "3_Soil Phosphate P Pool 30-60cm", subDF5$terms)
    
    
    ### plot total ecosystem P pool
    p1 <- ggplot(plotDF1, aes(x=Trt, y=value.mean))+
        geom_bar(aes(fill=Cat1), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF1, aes(x=Trt, ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF1, aes(x=Trt, y=value.mean), pch=19, col="black", size=2)+
        geom_point(data=totDF1i, aes(x=Trt, y=value, pch=Trt), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
        labs(x="", y=expression(paste("Ecosystem P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = c(0.6, 0.2),
              legend.box="horizontal",
              legend.direction="vertical",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_fill_manual(name="P Pool", 
                          values = c("1_Veg" = cbPalette[4],
                                     "Soil_Inorg" = cbPalette[2],
                                     "Soil_Org" = cbPalette[5]),
                          labels=c("1_Veg"="Plant+Litter",
                                   "Soil_Inorg"="Inorganic soil",
                                   "Soil_Org"="Organic soil"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_shape_manual(name="Treatment",
                           values=c("aCO2"=22,
                                    "eCO2"=24),
                           labels=c("aCO2"=expression(aCO[2]),
                                    "eCO2"=expression(eCO[2])))
        

    ### plot vegetation P pool
    p2 <- ggplot(plotDF2, aes(x=Trt, y=value.mean))+
        geom_bar(aes(fill=terms), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF2, aes(x=Trt, ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF2, aes(x=Trt, y=value.mean), pch=19, col="black", size=2)+
        geom_point(data=totDF2i, aes(x=Trt, y=value, pch=Trt), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
        labs(x="", y=expression(paste("Plant + Litter P pool (g P ", m^-2, ")")))+
        #labs(x="", y="")+
        theme_linedraw() +
        #scale_y_continuous(position = "right")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_fill_manual(name="", 
                          values = c("1_Canopy P Pool" = SpectralPalette[1],
                                     "4_Coarse Root P Pool" = SpectralPalette[4],
                                     "3_Fine Root P Pool" = SpectralPalette[3],
                                     "8_Forestfloor Leaf Litter P Pool" = SpectralPalette[9],
                                     "5_Heartwood P Pool" = SpectralPalette[5],
                                     "6_Sapwood P Pool" = SpectralPalette[6],
                                     "7_Standing Dead Wood P Pool" = SpectralPalette[8],
                                     "2_Understorey P Pool" = SpectralPalette[2]),
                          labels=c("1_Canopy P Pool"="Canopy",
                                   "4_Coarse Root P Pool"="Coarseroot",
                                   "3_Fine Root P Pool"="Fineroot",
                                   "8_Forestfloor Leaf Litter P Pool"="Forestfloor leaf litter",
                                   "5_Heartwood P Pool"="Heartwood",
                                   "6_Sapwood P Pool"="Sapwood",
                                   "7_Standing Dead Wood P Pool"="Standing dead wood",
                                   "2_Understorey P Pool"="Understorey aboveground"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_shape_manual(name="Treatment",
                           values=c("aCO2"=22,
                                    "eCO2"=24),
                           labels=c("aCO2"=expression(aCO[2]),
                                    "eCO2"=expression(eCO[2])))+
        guides(shape=F)
    
    
    ### plot soil P pool 0 - 10 cm
    p3 <- ggplot(subDF3, aes(x=Trt, y=value.mean))+
        geom_bar(aes(fill=terms), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF3, aes(x=Trt, ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF3, aes(x=Trt, y=value.mean), pch=19, col="black", size=2)+
        geom_point(data=totDF3i, aes(x=Trt, y=value, pch=Trt), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
        labs(x="0 - 10 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        coord_flip()+
        ylim(0, 20)+
        #ggtitle("0-10 cm")+
        annotate("text", y=18, x=0.7, label="0-10 cm", size=5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 1),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 0-10cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 0-10cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 0-10cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 0-10cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 0-10cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 0-10cm"="Labile P",
                                   "1_Microbial P Pool 0-10cm"="Microbe",
                                   "2_Soil Org Residual P Pool 0-10cm"="Organic residual"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_shape_manual(name="Treatment",
                           values=c("aCO2"=22,
                                    "eCO2"=24),
                           labels=c("aCO2"=expression(aCO[2]),
                                    "eCO2"=expression(eCO[2])))+
        guides(shape=F)

    
    ### plot soil P pool 10 - 30 cm
    p4 <- ggplot(subDF4, aes(x=Trt, y=value.mean))+
        geom_bar(aes(fill=terms), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF4, aes(x=Trt, ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF4, aes(x=Trt, y=value.mean), pch=19, col="black", size=2)+
        geom_point(data=totDF4i, aes(x=Trt, y=value, pch=Trt), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
        labs(x="10 - 30 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 20)+
        coord_flip()+
        #ggtitle("10-30 cm")+
        annotate("text", y=18, x=0.7, label="10-30 cm", size=5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 1),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 10-30cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 10-30cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 10-30cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 10-30cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 10-30cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 10-30cm"="Labile P",
                                   "1_Microbial P Pool 10-30cm"="Microbe",
                                   "2_Soil Org Residual P Pool 10-30cm"="Organic residual"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_shape_manual(name="Treatment",
                           values=c("aCO2"=22,
                                    "eCO2"=24),
                           labels=c("aCO2"=expression(aCO[2]),
                                    "eCO2"=expression(eCO[2])))+
        guides(shape=F)
    
    
    
    
    ### plot soil P pool 30 - 60 cm
    p5 <- ggplot(subDF5, aes(x=Trt, y=value.mean))+
        geom_bar(aes(fill=terms), col="black",
                 stat = "identity", position="stack")+
        geom_errorbar(data=totDF5, aes(x=Trt, ymax=value.mean+value.sd, ymin=value.mean-value.sd), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=totDF5, aes(x=Trt, y=value.mean), pch=19, col="black", size=2)+
        geom_point(data=totDF5i, aes(x=Trt, y=value, pch=Trt), size=2, color="black",
                   position=position_jitterdodge(dodge.width=0.8))+
        labs(x="30 - 60 cm", y=expression(paste("Soil P pool (g P ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 20)+
        coord_flip()+
        annotate("text", y=18, x=0.7, label="30-60 cm", size=5)+
        #ggtitle("30-60 cm")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 1),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 30-60cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 30-60cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 30-60cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 30-60cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 30-60cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 30-60cm"="Labile P",
                                   "1_Microbial P Pool 30-60cm"="Microbe",
                                   "2_Soil Org Residual P Pool 30-60cm"="Organic residual"))+
        scale_x_discrete(limits=c("aCO2", "eCO2"),
                         labels=c("aCO2"=expression(aCO[2]),
                                  "eCO2"=expression(eCO[2])))+
        scale_shape_manual(name="Treatment",
                           values=c("aCO2"=22,
                                    "eCO2"=24),
                           labels=c("aCO2"=expression(aCO[2]),
                                    "eCO2"=expression(eCO[2])))+
        guides(shape=F)
    
    
    
    p6 <- ggplot(hedDF,
                 aes(Trt, frac)) + 
        geom_bar(stat = "identity", aes(fill=Pool), col="black",position="stack")+
        xlab("")+ 
        ylab("Soil P bio-availability (fraction)")+
        theme_linedraw() +
        ylim(0, 1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        scale_fill_manual(name="Hedley P fractionation", 
                          values = c("1_Exchangeable_Pi" = set3Palette[6], 
                                     "2_Exchangeable_Po" = set3Palette[8],
                                     "3_Moderately_labile_Po" = set3Palette[3],
                                     #"4_Secondary_Pi" = set3Palette[4],
                                     #"5_Primary_Pi" = set3Palette[5], 
                                     "6_Occluded_P" = set3Palette[7]),
                          labels=c(expression("Exchangeable " * P[i]), 
                                   expression("Exchangeable " * P[o]), 
                                   expression("Moderately labile " * P[o]), 
                                   #expression("Secondary " * P[i] * " (" * F[e] * "-bound)"),
                                   #expression("Primary " * P[i] * " (" * C[a] *"-bound)"),
                                   "Residual P"))+
        scale_x_discrete(limits=c("amb","ele"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    
    
    ### bar plot
    aDF <- subDF3[subDF3$Trt=="aCO2",]
    eDF <- subDF3[subDF3$Trt=="eCO2",]
    
    aDF$frac <- with(aDF, round(value.mean/sum(value.mean)*100), 1)
    eDF$frac <- with(eDF, round(value.mean/sum(value.mean)*100),1)
    
    p71 <- ggplot(aDF, aes(x = "", y=frac, fill = terms)) +
        coord_polar(theta = "y")+
        geom_col(color="black")+
        geom_label(aes(label=frac), position=position_stack(vjust=0.5),
                  color="black", show.legend=F)+
        theme_void()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 0-10cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 0-10cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 0-10cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 0-10cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 0-10cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 0-10cm"="Labile P",
                                   "1_Microbial P Pool 0-10cm"="Microbe",
                                   "2_Soil Org Residual P Pool 0-10cm"="Organic residual"))
    
    
    
    p72 <- ggplot(eDF, aes(x = "", y=frac, fill = terms)) +
        coord_polar(theta = "y")+
        geom_col(color="black")+
        geom_label(aes(label=frac), position=position_stack(vjust=0.5),
                   color="black", show.legend=F)+
        theme_void()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 0-10cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 0-10cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 0-10cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 0-10cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 0-10cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 0-10cm"="Labile P",
                                   "1_Microbial P Pool 0-10cm"="Microbe",
                                   "2_Soil Org Residual P Pool 0-10cm"="Organic residual"))
    
    
    
    ### bar plot
    aDF <- subDF4[subDF4$Trt=="aCO2",]
    eDF <- subDF4[subDF4$Trt=="eCO2",]
    
    aDF$frac <- with(aDF, round(value.mean/sum(value.mean)*100), 1)
    eDF$frac <- with(eDF, round(value.mean/sum(value.mean)*100),1)
    
    p81 <- ggplot(aDF, aes(x = "", y=frac, fill = terms)) +
        coord_polar(theta = "y")+
        geom_col(color="black")+
        geom_label(aes(label=frac), position=position_stack(vjust=0.5),
                   color="black", show.legend=F)+
        theme_void()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 10-30cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 10-30cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 10-30cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 10-30cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 10-30cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 10-30cm"="Labile P",
                                   "1_Microbial P Pool 10-30cm"="Microbe",
                                   "2_Soil Org Residual P Pool 10-30cm"="Organic residual"))
    
    
    
    p82 <- ggplot(eDF, aes(x = "", y=frac, fill = terms)) +
        coord_polar(theta = "y")+
        geom_col(color="black")+
        geom_label(aes(label=frac), position=position_stack(vjust=0.5),
                   color="black", show.legend=F)+
        theme_void()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 10-30cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 10-30cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 10-30cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 10-30cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 10-30cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 10-30cm"="Labile P",
                                   "1_Microbial P Pool 10-30cm"="Microbe",
                                   "2_Soil Org Residual P Pool 10-30cm"="Organic residual"))
    
    
    
    ### bar plot
    aDF <- subDF5[subDF5$Trt=="aCO2",]
    eDF <- subDF5[subDF5$Trt=="eCO2",]
    
    aDF$frac <- with(aDF, round(value.mean/sum(value.mean)*100), 1)
    eDF$frac <- with(eDF, round(value.mean/sum(value.mean)*100),1)
    
    p91 <- ggplot(aDF, aes(x = "", y=frac, fill = terms)) +
        coord_polar(theta = "y")+
        geom_col(color="black")+
        geom_label(aes(label=frac), position=position_stack(vjust=0.5),
                   color="black", show.legend=F)+
        theme_void()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 30-60cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 30-60cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 30-60cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 30-60cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 30-60cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 30-60cm"="Labile P",
                                   "1_Microbial P Pool 30-60cm"="Microbe",
                                   "2_Soil Org Residual P Pool 30-60cm"="Organic residual"))
    
    
    
    p92 <- ggplot(eDF, aes(x = "", y=frac, fill = terms)) +
        coord_polar(theta = "y")+
        geom_col(color="black")+
        geom_label(aes(label=frac), position=position_stack(vjust=0.5),
                   color="black", show.legend=F)+
        #annotate(geom="text", x=1, y=0.1, text=expression(eCO[2]))+
        theme_void()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", 
                          values = c("4_Soil Inorg P Pool 30-60cm" = Diverge_hsv_Palette[5],
                                     "3_Soil Phosphate P Pool 30-60cm" = Diverge_hsv_Palette[7],
                                     "1_Microbial P Pool 30-60cm" = Diverge_hsv_Palette[1],
                                     "2_Soil Org Residual P Pool 30-60cm" = Diverge_hsv_Palette[3]),
                          labels=c("4_Soil Inorg P Pool 30-60cm"="Inorganic residual",
                                   "3_Soil Phosphate P Pool 30-60cm"="Labile P",
                                   "1_Microbial P Pool 30-60cm"="Microbe",
                                   "2_Soil Org Residual P Pool 30-60cm"="Organic residual"))
    
    
    
    #legend_turnover <- get_legend(p21 + theme(legend.position="bottom",
    #                                        legend.box = 'horizotal',
    #                                        legend.box.just = 'left')
    #                             +guides(fill=guide_legend(nrow=2,byrow=TRUE)))
    #
    #turnover_plots <- plot_grid(p21, p22, legend_turnover, ncol=1, rel_heights=c(1,1,0.2))
    
    #pie_charts <- plot_grid(p71, p72, p81, p82, p91, p92,
    #                        ncol=2, rel_widths=c(1,1))
    
    
    legend_bot_row <- get_legend(p3 + theme(legend.position="bottom",
                                            legend.box = 'horizotal',
                                            legend.box.just = 'left')
                                 +guides(fill=guide_legend(nrow=2,byrow=TRUE)))
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    
    ## plot 
    pdf(paste0("output/figures/figure2.pdf"),
        width=10,height=8)
    top_row <- plot_grid(p1, NULL, p2, ncol=3, rel_widths=c(1, 0.1, 1))
    bot_row_left <- plot_grid(p3, p4, p5, legend_bot_row, ncol=1, rel_heights=c(0.7, 0.7, 1, 0.4))
    bot_row <- plot_grid(bot_row_left, NULL, p6, ncol=3, rel_widths=c(1, 0.1, 1))
    plot_grid(top_row, bot_row, #scale = 0.9,
              ncol = 1, rel_widths = c(1, 1),
              rel_heights=c(1, 1))
    #grid.text(grid.labs,x = c(0.08, 0.48, 0.1, 0.42, 0.75), y = c(0.97, 0.97, 0.5, 0.5, 0.5),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    grid.text(grid.labs,x = c(0.08, 0.54, 0.08, 0.54), 
              y = c(0.97, 0.97, 0.52, 0.52),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    
    ### some numbers
    finalDF <- plotDFi[plotDFi$terms%in%c("Soil Inorg P Pool 0-10cm",
                                          "Soil Org P Pool 0-10cm",
                                          "Soil Inorg P Pool 10-30cm",
                                          "Soil Org P Pool 10-30cm",
                                          "Soil Inorg P Pool 30-60cm",
                                          "Soil Org P Pool 30-60cm"),]
    sumDF <- summaryBy(value~Ring+Trt, data=finalDF, FUN=sum, na.rm=T, keep.names=T)
    trtDF <- summaryBy(value~Trt, data=sumDF, FUN=c(mean,sd), na.rm=T, keep.names=T)
    
    microbeDF <- plotDFi[plotDFi$terms%in%c("Microbial P Pool 0-10cm",
                                            "Microbial P Pool 10-30cm",
                                            "Microbial P Pool 30-60cm"),]
    sumDF <- summaryBy(value~Ring+Trt, data=microbeDF, FUN=sum, na.rm=T, keep.names=T)
    trtDF <- summaryBy(value~Trt, data=sumDF, FUN=c(mean,sd), na.rm=T, keep.names=T)
    
    availpDF <- plotDFi[plotDFi$terms%in%c("Soil Phosphate P Pool 0-10cm",
                                            "Soil Phosphate P Pool 10-30cm",
                                            "Soil Phosphate P Pool 30-60cm"),]
    sumDF <- summaryBy(value~Ring+Trt, data=availpDF, FUN=sum, na.rm=T, keep.names=T)
    trtDF <- summaryBy(value~Trt, data=sumDF, FUN=c(mean,sd), na.rm=T, keep.names=T)

    
    sumDF <- summaryBy(value~Ring+Trt, data=vegDF, FUN=sum, na.rm=T, keep.names=T)
    trtDF <- summaryBy(value~Trt, data=sumDF, FUN=c(mean,sd), na.rm=T, keep.names=T)
}


