make_canopy_P_flux_comparison <- function(inDF1, 
                                          inDF2, 
                                          plot.option) {
    
    ### calculate means across rings
    DF1 <- inDF1[,c("Date", "Ring", "canopy_p_flux")]
    
    DF2 <- inDF2[,c("Date", "Ring", "canopy_p_flux")]
    
    myDF <- merge(DF1, DF2, by=c("Date", "Ring"), all=T)
    colnames(myDF) <- c("Date", "Ring", "oneoff", "smoothed")
    
    plotDF <- melt(myDF, id.var=c("Date", "Ring"))
    
    if (plot.option == T) {
        
        p1 <- ggplot(plotDF, aes(Date, value, group=Ring)) + 
            geom_point() + 
            stat_smooth() +
            facet_wrap(~variable)+
            ylab("Canopy P pool (g P m-2)")
        
        
        pdf("output/checks/canopy_P_flux_comparison.pdf", 
            width=8, height=4)
        
        plot(p1)
        
        dev.off()
        
    } 

}