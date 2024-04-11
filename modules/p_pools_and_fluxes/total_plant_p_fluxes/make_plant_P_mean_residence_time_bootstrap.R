make_plant_P_mean_residence_time_bootstrap <- function(norm, 
                                                       p_stand, 
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
    
    
    ### remove coarse woody component and litter
    p_stand$tot_reduced <- with(p_stand, leaf+fineroot+understorey+sapwood)
    
    
    tmp1 <- data.frame("Ring" = p_stand$Ring, "Trt" = p_stand$Trt, 
                      "Total_standing_P_stock" = p_stand$tot_reduced)
    
    tmp2 <- as.numeric(p_flux[p_flux$terms=="Total vegetation uptake P flux",2:7])
    
    out <- data.frame(tmp1, 
                      "Total_plant_P_uptake_flux"=tmp2)
    
    out$plant_P_MRT <- with(out, Total_standing_P_stock / Total_plant_P_uptake_flux)
    
    return(out)
}