generate_linear_model_results <- function(budgetDF,
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
                      c("terms", "R1", "R2", "R3", "R4", "R5", 'R6')]
    inDF2 <- deltaDF[deltaDF$terms=="Microbial P Pool",
                     c("terms", "R1", "R2", "R3", "R4", "R5", 'R6')]
    
    
    
    myDF <- rbind(inDF1, inDF2)
    
    myDF <- reshape2::melt(myDF, id.vars=c("terms"), variable.name="Ring")
    myDF$Trt <- "eCO2"
    myDF$Trt[myDF$Ring%in%c("R2", "R3", "R6")] <- "aCO2"
    
    ### prepare output dataframe
    terms <- unique(myDF$terms)
    
    outDF <- data.frame("variable"=terms,
                        "F_statistic"=NA,
                        "Df"=NA,
                        "Df.res"=NA,
                        "p_value"=NA)
    
    ### perform the statistical test and output statistical summary table
    for (i in terms) {
        
        ### subset
        subDF <- myDF[myDF$terms==i,]
        
        ### linear model
        mod.result <- lm(value~Trt,data=subDF)
        mod.anova <- anova(mod.result)
        
        ### store output
        outDF$F_statistic[outDF$variable==i] <- mod.anova$`F value`[1]
        outDF$Df[outDF$variable==i] <- mod.anova$Df[1]
        outDF$Df.res[outDF$variable==i] <- mod.anova$Df[2]
        outDF$p_value[outDF$variable==i] <- mod.anova$`Pr(>F)`[1]
        
    }
   
    
    ### save table
    write.csv(outDF, "output/summary_tables/linear_model_statistics.csv", row.names=F)

}