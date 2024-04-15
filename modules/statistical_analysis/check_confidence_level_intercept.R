check_confidence_level_intercept <- function(budgetDF,
                                             deltaDF) {
    
    ### clean the data
    inDF1 <- budgetDF[budgetDF$terms%in%c("Total plant P stock",
                                          "Total plant P requirement flux",
                                          "Total plant P retranslocation flux",
                                          "Plant P uptake flux",
                                          "Soil P mineralization flux",
                                          "Labile Pi stock",
                                          "Plant P MRT",
                                          "Plant PUE",
                                          "Microbe P MRT",
                                          "Overstorey GPP efficiency",
                                          "Understorey GPP efficiency",
                                          "Plant GPP efficiency"),
                      c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se")]
    inDF2 <- deltaDF[deltaDF$terms=="Microbial P Pool",
                     c("terms", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd", "diff", "diff_se")]
    
    myDF <- rbind(inDF1, inDF2)
    
    
    ### confidence interval of the diff
    myDF$diff_cf_95 <- qt((1-0.05/2), 4) * myDF$diff_se
    myDF$diff_cf_85 <- qt((1-0.15/2), 4) * myDF$diff_se
    myDF$diff_cf_75 <- qt((1-0.25/2), 4) * myDF$diff_se

    ###determine the confidence level at which CI intercepts with 0
    terms <- unique(myDF$terms)
    p.list <- seq(0.05, 0.9, 0.01)
    
    ### prepare outDF
    outDF <- as.data.frame(matrix(0, nrow=length(terms), ncol = length(p.list)+1))
    colnames(outDF) <- c("terms", paste0("cl_", 1-p.list))    
    outDF$terms <- terms

    ### loop through the terms
    for (i in terms) {
        
        ### subset
        subDF <- myDF[myDF$terms==i,]
        
        ### go through the p-value list
        for (j in 1:length(p.list)) {
            ### finding the confidence interval that does not intercept with 0
            ci.value <- qt((1-p.list[j]/2), 4) * subDF$diff_se
            
            if (ci.value < abs(subDF$diff)) {
                outDF[outDF$terms==i, j+1] <- (1-p.list[j])
            } else {
                outDF[outDF$terms==i, j+1] <- "NA"
            }
            
        }
    }
    
    ### clean
    outDF2 <- data.frame("terms"=terms, "cl_threshold"=NA)
    
    for (i in terms) {
        
        subDF <- c(as.numeric(outDF[outDF$terms==i, 2:length(p.list)+1]))
        outDF2$cl_threshold[outDF2$terms==i] <- max(subDF, na.rm=T)
    }

    ### save table
    write.csv(outDF2, "output/summary_tables/confidence-level_threshold_for_major_variables.csv", row.names=F)

}