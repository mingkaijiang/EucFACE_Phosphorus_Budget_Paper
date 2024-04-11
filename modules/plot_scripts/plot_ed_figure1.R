plot_ed_figure1 <- function() {
    
    #######################################################################
    ### global dataset
    leafDF <- read.csv("data/literature/Leaf_nitrogen_and_phosphorus_concentrations_data.csv")
    
    ### subset trees only
    treeDF <- subset(leafDF, Life_form1 == "T")
    
    treeDF$lab <- "global"
    
    treeDF <- treeDF[,c("Leaf_N", "Leaf_P", "lab")]
    
    ### EucFACE data from Crous et al. 2019, Table 2, Mature leaves
    eucDF <- data.frame(16, 0.7, "EucFACE")
    colnames(eucDF) <- c("Leaf_N", "Leaf_P", "lab")
    
    ### NP ratio indication lines
    nDF <- seq(0, 80, 1)
    p20 <- nDF/20
    p16 <- nDF/16
    npDF <- data.frame(nDF, p20, p16)
    colnames(npDF) <- c("Leaf_N", "Leaf_P20", "Leaf_P16")
    
    
    #### Plotting script
    p4 <- ggplot(treeDF, aes(Leaf_N, Leaf_P))+
        stat_density_2d(geom = "polygon", contour = TRUE,
                        aes(fill = after_stat(level)), colour = "black",
                        bins = 5)+
        geom_point(alpha=0.2, size=0.1)+
        geom_point(aes(x=16, y=0.7), fill="red", size = 4, pch = 21)+
        scale_fill_distiller(palette = "Blues", direction = 1) +
        geom_line(npDF, mapping=aes(Leaf_N, Leaf_P20), col="purple", lty=2)+
        geom_line(npDF, mapping=aes(Leaf_N, Leaf_P16), col="orange")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=14),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position=c(.8, .8),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        xlab(expression(paste("Leaf nitrogen (mg " * g^-1 * ")")))+
        ylab(expression(paste("Leaf phosphorus (mg " * g^-1 * ")")))+
        annotate("text", x=10, y = 6, label = "n = 3201", size=6)
    
    
    
    ### plotting
    pdf("output/si_figures/Leaf_NP_comparison.pdf", width = 6, height = 6)
    plot(p4)
    dev.off()
    
    #######################################################################
    ###################### 3. ISRIC Soil P data ###########################
    isricDF <- read.csv("data/literature/ISRIC_Report_2011_01_Dataset.csv")
    
    ### class
    isricDF$p_brayI <- as.numeric(as.character(isricDF$p_brayI))
    isricDF$p_nz <- as.numeric(as.character(isricDF$p_nz))
    isricDF$p_brayZ <- as.numeric(as.character(isricDF$p_brayZ))
    isricDF$p_olsn <- as.numeric(as.character(isricDF$p_olsn))
    isricDF$p_mehlich3 <- as.numeric(as.character(isricDF$p_mehlich3))
    isricDF$p_h2o <- as.numeric(as.character(isricDF$p_h2o))
    
    
    ### log
    isricDF$log_p_brayI <- log(isricDF$p_brayI)
    isricDF$log_p_brayZ <- log(isricDF$p_brayZ)
    isricDF$log_p_nz <- log(isricDF$p_nz)
    isricDF$log_p_olsn <- log(isricDF$p_olsn)
    isricDF$log_p_mehlich3 <- log(isricDF$p_mehlich3)
    isricDF$log_p_h2o <- log(isricDF$p_h2o)
    
    
    ### subset
    subDF1 <- data.frame(isricDF$log_p_brayI, isricDF$p_brayI, "1_brayI", isricDF$lat_point)
    subDF2 <- data.frame(isricDF$log_p_brayZ, isricDF$p_brayZ, "2_brayZ", isricDF$lat_point)
    subDF3 <- data.frame(isricDF$log_p_nz, isricDF$p_nz, "3_nz", isricDF$lat_point)
    subDF4 <- data.frame(isricDF$log_p_olsn, isricDF$p_olsn, "4_olsn", isricDF$lat_point)
    subDF5 <- data.frame(isricDF$log_p_mehlich3, isricDF$p_mehlich3, "5_mehlich3", isricDF$lat_point)
    subDF6 <- data.frame(isricDF$log_p_h2o, isricDF$p_h2o, "6_h2o", isricDF$lat_point)
    
    ### colnames
    colnames(subDF1) <- colnames(subDF2) <- colnames(subDF3) <- colnames(subDF4) <- colnames(subDF5) <- colnames(subDF6) <- c("log_value", "value", "method", "lat")
    
    ### remove na
    subDF1 <- subDF1[!is.na(subDF1$value),]
    subDF2 <- subDF2[!is.na(subDF2$value),]
    subDF3 <- subDF3[!is.na(subDF3$value),]
    subDF4 <- subDF4[!is.na(subDF4$value),]
    subDF5 <- subDF5[!is.na(subDF5$value),]
    subDF6 <- subDF6[!is.na(subDF6$value),]
    
    ### rbind
    plotDF <- rbind(subDF1, subDF3, subDF4, subDF5, subDF6)
    
    
    ################################### Prepare MAT and MAP, globally forests
    ### read in global biome and climate data at CRU grids
    biomeDF <- read.csv("data/literature/biome_temp_prec_full_1991_2012.csv")
    
    ### create a subset DF
    biomeDF <- subset(biomeDF, BIOME <= 14 & BIOME > 0)
    
    ### convert into lat lon raster
    rDF <- biomeDF[,c("lat", "lon", "BIOME")]
    
    library(sp)
    library(rgdal)
    library(raster)
    coordinates(rDF)=~lon+lat
    gridded(rDF) <- T
    r <- raster(rDF)
    
    ### soil.coords 
    soil.coords <- data.frame(isricDF$long_point, isricDF$lat_point)
    colnames(soil.coords) <- c("lon", "lat")
    
    eDF <- extract(r,             # raster layer
                   soil.coords,     # coordinates to buffer
                   df=TRUE) 
    
    
    ### merge
    plotDF4 <- cbind(isricDF, eDF)
    
    ### check just bray P method
    plotDF5 <- plotDF4[,c("p_brayI", "log_p_brayI", "BIOME")]
    plotDF5 <- plotDF5[!is.na(plotDF5$log_p_brayI),]
    plotDF5 <- plotDF5[!is.na(plotDF5$BIOME), ]
    plotDF5 <- plotDF5[plotDF5$log_p_brayI > -100,]
    plotDF5$BIOME <- as.character(plotDF5$BIOME)
    
    
    ### only forests
    plotDF5 <- subset(plotDF5, BIOME%in%c(1,2,3,4,5,6,12))
    
    ### assign forest type
    plotDF5$BIOME2 <- plotDF5$BIOME
    plotDF5$BIOME2 <- gsub("12", "7_MF", plotDF5$BIOME2)
    plotDF5$BIOME2 <- gsub("1", "1_TSMB", plotDF5$BIOME2)
    plotDF5$BIOME2 <- gsub("2", "2_TSDB", plotDF5$BIOME2)
    plotDF5$BIOME2 <- gsub("3", "3_TSCF", plotDF5$BIOME2)
    plotDF5$BIOME2 <- gsub("4", "4_TBMF", plotDF5$BIOME2)
    plotDF5$BIOME2 <- gsub("5", "5_TCF", plotDF5$BIOME2)
    plotDF5$BIOME2 <- gsub("6", "6_BF", plotDF5$BIOME2)
    
    
    forests <- c("Tropical moist broadleaf",            
                 "Tropical dry broadleaf",            
                 "Tropical conifer",               
                 "Temperate broadleaf & mixed",      
                 "Temperate conifer",                
                 "Boreal forests",              
                 "Mediterranean forests")  
    
    
    ### group forests together
    plotDF6 <- plotDF5
    plotDF6$BIOME3 <- plotDF6$BIOME2
    plotDF6$BIOME3 <- gsub("1_TSMB", "1_tropic", plotDF6$BIOME3)
    plotDF6$BIOME3 <- gsub("2_TSDB", "1_tropic", plotDF6$BIOME3)
    plotDF6$BIOME3 <- gsub("3_TSCF", "1_tropic", plotDF6$BIOME3)
    plotDF6$BIOME3 <- gsub("4_TBMF", "2_temperate", plotDF6$BIOME3)
    plotDF6$BIOME3 <- gsub("5_TCF", "2_temperate", plotDF6$BIOME3)
    plotDF6$BIOME3 <- gsub("6_BF", "3_boreal", plotDF6$BIOME3)
    plotDF6$BIOME3 <- gsub("7_MF", "4_Med", plotDF6$BIOME3)
    
    
    
    ### get sample size
    nDF <- table(plotDF6$BIOME3)
    
    #### only the bray P method
    p3 <- ggplot(plotDF6) +
        geom_jitter(aes(x=BIOME3, y=log_p_brayI, fill=BIOME3), pch=21, alpha=0.4)+
        geom_boxplot(aes(x=BIOME3, y=log_p_brayI, fill=BIOME3), outlier.alpha = 0)+
        #geom_point(aes(x="2_TSDB", y = log(2)), fill="purple", pch = 24, size = 4)+
        geom_hline(yintercept=log(2), col="black", size = 1)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.text.align = 0,
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        scale_y_continuous(name="Soil available P (ppm)", 
                           breaks=c(log(0.01), log(0.1), log(1), log(10), log(100), log(1000)), 
                           labels=c(0.01, 0.1, 1, 10, 100, 1000), 
                           limits=c(-6, 10))+
        scale_x_discrete(name="Forest biome",
                         breaks=c("1_tropic","2_temperate","3_boreal","4_Med"),
                         labels=c("Tropics & subtropics", "Temperate", "Boreal",
                                  "Mediterranean"))+
        annotate("text", x=1, y = 8, label = paste0("n = ", nDF[1]))+
        annotate("text", x=2, y = 8, label = paste0("n = ", nDF[2]))+
        annotate("text", x=3, y = 8, label = paste0("n = ", nDF[3]))+
        annotate("text", x=4, y = 8, label = paste0("n = ", nDF[4]))
    
    
    pdf("output/si_figures/ISRIC_soil_P_by_Bray_by_large_biome.pdf", width = 6, height = 3)
    plot(p3)
    dev.off()
    
    
    
    
    #######################################################################
    ###################### 1. The global map ##############################
    #### data from Dai
    #### files are in nc, and are large
    #### read in data
    require(ncdf4)
    inName1 <- "data/literature/Dai/PBR1.nc"
    nc <- nc_open(inName1)
    
    ### use raster brick to get all the data
    mapDF <- brick(inName1, varname = "PBR")
    
    ### subset the first layer
    mapDF <- subset(mapDF, 2) * 0.01 # converting factor given by authors
    
    ### spatial extent
    mapDF@crs 
    
    nc_close(nc)
    
    ### remove negative values
    mapDF[mapDF<0] <- NA
    
    ### prepare reclassify DF
    ### Terrer real value
    class.m.t <- c(-9.99, 2, 1,
                   2, 5, 2,
                   5, 10, 3,
                   10, 15, 4,
                   15, 20, 5,
                   20, 30, 6,
                   30, 40, 7,
                   40, 140, 8)
    
    rcl.m.t <- matrix(class.m.t,
                      ncol=3,
                      byrow=T)
    
    ### reclassify
    mapDF2 <- reclassify(mapDF, rcl.m.t)
    
    ### prepare plotting
    color.scheme <- c("purple", "orange", "yellow", "lightgreen", "darkgreen")
    color.scheme2 <- c("brown", "red2", "orange", "lightgreen",
                       "lightblue", "cyan4", "blue", "darkblue")
    
    
    #### include leaf NP grid information
    ### global dataset
    leafDF <- read.csv("data/literature/Leaf_nitrogen_and_phosphorus_concentrations_data.csv")
    treeDF <- subset(leafDF, Life_form1 == "T")
    treeDF$lab <- "Leaf NP dataset"
    treeDF <- treeDF[,c("lab", "Latitude", "Longitude")]
    
    
    
    #pdf("output/si_figures/Shangguan_map_with_other_datapoints.pdf", width = 8, height = 4)
    #par(xpd = FALSE)
    #plot(mapDF2, 
    #     col=color.scheme2,
    #     xlab="Longitude", 
    #     ylab="Latitude",
    #     ylim=c(-90, 90),
    #     legend=F, yaxt="n", cex.axis=0.6, cex.lab=0.6)
    #axis(side=2, at=c(-90, -45, 0, 45, 90), labels = T, cex.axis = 0.6)
    #points(x=treeDF$Longitude, y=treeDF$Latitude, type="p", 
    #       pch=21, col="black", bg="black", cex=0.2)
    #points(x=soil.coords$lon, y=soil.coords$lat, type="p", 
    #       pch=22, col="purple", bg="purple", cex=0.2)
    #
    #par(xpd = TRUE)
    #legend(x = -195, y = 30, 
    #       title="soil available P (ppm)",
    #       legend = c("0-2", "2-5", "5-9.34", "9.34-15",
    #                  "15-20", "20-30", "30-40", ">40"), 
    #       fill = color.scheme2,
    #       cex = 0.6, inset = 0.95,
    #       bg="white")
    #dev.off()
    
    ######################################################################
    ###################### 2. PDF plot Shangguan #########################
    ### plot a PDF
    pdfDF <- as.vector(mapDF)
    pdfDF <- pdfDF[!is.na(pdfDF)]
    
    ### get the density
    d <- density(pdfDF)
    
    ### create data frame
    xd <- data.frame(d[c("x", "y")])
    
    ### find probability distribution marks
    probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    quantiles <- quantile(pdfDF, prob=probs)
    xd$quant <- factor(findInterval(xd$x,quantiles))
    
    spectral.colors <- brewer.pal(6,"Spectral")
    
    p2 <- ggplot(xd, aes(x,y)) + 
        geom_line() + 
        geom_ribbon(aes(ymin=0, ymax=y, fill=as.character(quant))) + 
        scale_x_continuous(breaks=c(2, 11, 26, 45),
                           labels=c(2, 11, 26, ">45"),
                           limits = c(0,50),expand=c(0,0)) + 
        scale_fill_manual(name="",
                          limits=c(0:5),
                          labels=c("0-5%", "5-25%", "25-50%", "50-75%", "75-95%", "95-100%"),
                          values=spectral.colors,
                          guide=guide_legend(nrow=6))+
        geom_vline(xintercept=9.34, col="black", size = 1)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=10),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.text.align = 0,
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        guides(color = guide_legend(nrow=5, byrow = T))+
        ylab("Density")+
        xlab("Soil available P (ppm)")
    
    
    plot(p2)
    
    #### Plotting
    pdf("output/si_figures/Shangguan_P_density_plot.pdf", width = 6, height = 3)
    plot(p2)
    dev.off()
    
    #######################################################################
}