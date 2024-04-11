process_p_concentration_variables <- function() {
    
    ############################## Soil ###############################
    
    #### Soil P concentrations 
    ### three depth: 0 - 10 cm
    ###              10 - 30 cm
    ###              30 - 60 cm
    soil_p_concentration <- make_soil_p_concentration()
    write.csv(soil_p_concentration, "data/processed/soil_p_concentration.csv", row.names=F)
    
    ### 
    soil_inorganic_p_concentration <- make_soil_inorganic_p_concentration()
    write.csv(soil_inorganic_p_concentration, "data/processed/soil_inorganic_p_concentration.csv", row.names=F)
    
    ### 
    soil_organic_p_concentration <- make_soil_organic_p_concentration()
    write.csv(soil_organic_p_concentration, "data/processed/soil_organic_p_concentration.csv", row.names=F)
    
    
    #### Soil phosphate conc, this returns % of P, not % of PO4
    #### Only top 10 cm in the earlier datasets (with additional 0 - 2 cm depth)
    #### but Johanna's 2018 data has a complete depth profile (up to transition zone)
    soil_phosphate_concentration <- make_soil_phosphate_concentration()
    write.csv(soil_phosphate_concentration, "data/processed/soil_phosphate_concentration.csv", row.names=F)
    
    
    #### Microbial P conc.
    #### Top 60 cm
    microbial_p_concentration <- make_microbial_p_concentration()
    write.csv(microbial_p_concentration, "data/processed/microbial_p_concentration.csv", row.names=F)
    
    
    #### Hedley fractionation dataset
    ### top 10 cm
    soil_hedley_p_concentration <- make_soil_hedley_p_concentration()
    write.csv(soil_hedley_p_concentration, "data/processed/soil_hedley_p_concentration.csv", row.names=F)
    
    
    ############################## Plant ###############################
    #### Canopy P conc.
    canopy_p_concentration <- make_canopy_p_concentration()
    write.csv(canopy_p_concentration, "data/processed/canopy_p_concentration.csv", row.names=F)
    
    #### Leaf litter P conc. 
    leaflitter_p_concentration <- make_leaflitter_p_concentration()
    write.csv(leaflitter_p_concentration, "data/processed/leaflitter_p_concentration.csv", row.names=F)
    
    
    #### Wood P conc. 
    sapwood_p_concentration <- make_wood_p_concentration()
    write.csv(sapwood_p_concentration, "data/processed/sapwood_p_concentration.csv", row.names=F)
    
    
    #### Frass P conc.
    ### frass flux needs to added to canopy P demand each year.
    frass_p_concentration <- make_frass_p_concentration()
    write.csv(frass_p_concentration, "data/processed/frass_p_concentration.csv", row.names=F)
    
    
    #### Fineroot P conc.
    ### top 30 cm of soil
    fineroot_p_concentration <- make_fineroot_p_concentration()
    write.csv(fineroot_p_concentration, "data/processed/fineroot_p_concentration.csv", row.names=F)
    
    
    #### Understorey P conc.
    understorey_p_concentration <- make_understorey_p_concentration()
    write.csv(understorey_p_concentration, "data/processed/understorey_p_concentration.csv", row.names=F)
    
    
    #### Understorey litter P conc.
    understorey_litter_p_concentration <- make_understorey_litter_p_concentration()
    write.csv(understorey_litter_p_concentration, "data/processed/understorey_litter_p_concentration.csv", row.names=F)
    
    
    ### P resorption coefficients
    plant_p_retranslocation_coefficients <- calculate_plant_p_retranslocation_coefficients(canopy_p_concentration,
                                                                                           leaflitter_p_concentration,
                                                                                           understorey_p_concentration,
                                                                                           understorey_litter_p_concentration,
                                                                                           sapwood_p_concentration,
                                                                                           fineroot_retranslocation_coefficient = retrans_froot)
    
    write.csv(plant_p_retranslocation_coefficients, "data/processed/plant_p_retranslocation_coefficients.csv", row.names=F)
    
}