process_c_variables <- function() {
    
    
    #### Ring-specific bulk density
    ### 3 depths profile: 0 - 10,
    ###                   10 - 30,
    ###                   30 - 60 (transition)
    soil_bulk_density <- make_soil_bulk_density()
    
    overstorey_gpp_flux <- make_overstorey_gpp_flux()
    
    understorey_gpp_flux <- make_understorey_gpp_flux()
    
    
    #### Canopy related variables (SLA, LAI, Canopy biomass)
    lai_variable <- make_lai_variable()
    sla_variable <- make_sla_variable()
    
    canopy_c_pool <- make_canopy_c_pool(lai_variable, 
                                        sla_variable, 
                                        sla_option="variable")
    
    
    #### Wood C pool
    # we have sapwood and heartwood in the dataframe
    # excluded dead trees (before 2018)
    wood_c_pool <- make_wood_c_pool(ring_area=FACE_ring_area,
                                    c_frac=c_fraction)
    
    
    ### standing dead wood c pool
    ### only report the max standing dead (i.e. 2018 value)
    standing_dead_c_pool <- make_standing_dead_c_pool(ring_area=FACE_ring_area,
                                                      c_frac=c_fraction)
    
    
    #### Fineroot pool
    ### top 60 cm
    ### biomass was firstly estimated based on a soil bulk density value
    ### that is "assumed",
    ### so we probably will need to back calculate the raw biomass estimates
    ### and then multiply by the most accurate soil bulk density values.
    ### include intermediate root size
    fineroot_c_pool <- make_fineroot_c_pool(back.calculate=T,
                                            soil_bulk_density=soil_bulk_density,
                                            root.size="intermediate")
    
    fineroot_c_pool3 <- make_fineroot_c_pool(back.calculate=T,
                                             soil_bulk_density=soil_bulk_density,
                                             root.size="small")
    
    
    #### Understorey aboveground biomass 
    ### - 1: Varsha's clipping
    ### - 2: Matthias's stereo camera
    ### we have live and dead fraction based on clipping method
    ### We decide to use clipping method to estimate the relative proportion of live and dead 
    ### then we use stereo camera approach to estimate the total biomass
    
    ### estimate % live and % dead
    understorey_c_pool_clipping <- make_understorey_aboveground_c_pool_clipping(c_fraction_ud,
                                                                                strip_area)
    
    understorey_c_pool <- make_understorey_aboveground_c_pool_camera(c_frac=c_fraction_ud,
                                                                     plot.option=T)
    
    
    
    #### Soil C content
    #### return soil C by depths
    soil_c_pool <- make_soil_c_pool(bk_density=soil_bulk_density)
    
    
    #### Microbial C pool
    #### this pool has data only at 0-10cm depth - Cat's data
    microbial_c_pool <- make_microbial_c_pool(soil_bulk_density)
    
    
    #### Soil mycorrhizal pool
    #### But we don't have a P concentration for it and 
    #### therefore it's not included in the P budget
    mycorrhizal_c_pool <- make_mycorrhizal_c_pool(microbial_c_pool)
    
    
    #### Coarse root C pool 
    coarse_root_c_pool <- make_coarse_root_pool(c_fraction, 
                                                fr_pool=fineroot_c_pool) 
    
    
    #### Leaf litter pool - forest floor leaf litter pool
    leaflitter_c_pool <- make_leaflitter_pool(c_fraction)
    
    
    
    ############################## Fluxes ###############################
    
    #### Ltter production (leaf, twig, bark, seed)
    litter_c_production_flux <- make_litter_c_flux(c_fraction)
    
    leaflitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "leaf_flux", "Start_date", "End_date", "Days")]
    write.csv(leaflitter_c_production_flux, "data/processed/leaflitter_c_production_flux.csv", row.names=F)
    
    twiglitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "twig_flux", "Start_date", "End_date", "Days")]
    write.csv(twiglitter_c_production_flux, "data/processed/twiglitter_c_production_flux.csv", row.names=F)
    
    barklitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "bark_flux", "Start_date", "End_date", "Days")]
    write.csv(barklitter_c_production_flux, "data/processed/barklitter_c_production_flux.csv", row.names=F)
    
    seedlitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "seed_flux", "Start_date", "End_date", "Days")]
    write.csv(seedlitter_c_production_flux, "data/processed/seedlitter_c_production_flux.csv", row.names=F)
    
    
    #### Frass production
    frass_c_production_flux <- make_frass_c_production_flux()
    
    
    
    #### 2.3 Canopy C production
    ## need to add insect consumption flux back!
    
    ### herbivore leaf c consumption flux
    ### extrapolated based on frass weight, leaf area consumed and sla data
    herbivory_leaf_consumption_flux <- make_herbivory_leaf_consumption_flux(sla=sla_variable, 
                                                                            frass_flux=frass_c_production_flux)
    
    
    canopy_c_production_flux <- merge_litter_c_and_herbivory_loss(litter=leaflitter_c_production_flux,
                                                                  herbivory=herbivory_leaf_consumption_flux)
    
    
    ## based on change in leaf area and litterfall
    ## calculate change in leaf pool as well as litterfall
    dLEAF_litter_flux <- make_dLAI_litter(litter=leaflitter_c_production_flux, 
                                          sla_variable=sla_variable)
    
    canopy_c_production_flux_new <- make_canopy_c_production_flux_new(inDF=dLEAF_litter_flux)
    
    
    #### 2.5 Wood C production
    wood_c_production <- make_wood_production_flux(wood_c_pool)
    
    
    #### Fineroot production
    #### root size: < 2mm in diameter, not including the intermediate roots
    #### we can only estimate fineroot c production for top 30 cm,
    #### but the 30 - 60 cm fienroot biomass is tiny (3% of total), and therefore 
    #### we assume the production flux is small and negligible
    fineroot_c_production_flux <- make_fineroot_c_production_flux(root.size="intermediate",
                                                                  tot.root = fineroot_c_pool,
                                                                  f.root = fineroot_c_pool3)
    
    #fineroot_c_production_flux2 <- make_fineroot_c_production_flux(root.size="small",
    #                                                               tot.root = fineroot_c_pool,
    #                                                               f.root = fineroot_c_pool3)
    
    
    #### Understorey production flux 
    #### - 1: Varsha's clipping
    #### - 2: Matthias's stereo camera
    understorey_c_flux_clipping <- make_understorey_aboveground_production_flux_clipping(c_fraction_ud)
    
    
    
    #### understorey litter flux
    ### basically understorey dead 
    understorey_litter_c_flux <- make_understorey_litter_flux(c_fraction_ud)
    
    
    #### Coarse root C production
    coarse_root_c_flux <- make_coarse_root_production_flux(coarse_root_c_pool) 
    
    
    
}