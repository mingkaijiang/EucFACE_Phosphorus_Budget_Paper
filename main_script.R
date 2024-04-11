####################################################################################################################
##### ---------------------------------------------------------------------------------------------------------##### 
#####                            Master script for EucFACE Phosphorus Budget                                   #####
##### ---------------------------------------------------------------------------------------------------------##### 
####################################################################################################################
####
#### Written by: Mingkai Jiang (jiangmingkai@zju.edu.cn)
#### 
####                              CODE STRUCTURE                
####
#### 1. Compute biomass pools, production fluxes and litter fluxes
####
#### 2. Compute phosphorus concentrations for major pools and fluxes 
####
#### 3. Generate P pools and fluxes
####
#### 4. Generate summary tables, based on unnormalized responses
####
#### 5. Generate un-normalized plots
####
#### 6. Generate normalized responses 
####
#### 7. Explore covariates
####
#### 8. Generate normalized plots

##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 0: Prepare the repository (clean and read in necessary packages)
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("R/prepare.R")

#### turn warnings off globally
# options(warn=-1)

########################################################################################## 
#####
##### Step 1: Preparing C related variables
#####
##### Note: 
##### For all C pools, unit in g C m-2,
##### For all C fluxes, output rate in unit of mg C m-2 d-1, and the period over which this rate applies.
##### Almost all data has been processed in the previous C budget, 
##### so here we do not report the data processing steps (process_c_variables).
##### Instead, we are only reading the data, as below.

#### no longer needed (delete once all code check completed)
# process_c_variables()


############################## Read in C-related pools and fluxes ###############################
#### Ring-specific bulk density
### 3 depths profile: 0 - 10,
###                   10 - 30,
###                   30 - 60 (transition)
soil_bulk_density <- read.csv("data/processed/soil_bulk_density.csv", header=T)

### GPP fluxes (estimated based on MAESPA)
overstorey_gpp_flux <- read.csv("data/processed/overstorey_gpp_flux.csv", header=T)
understorey_gpp_flux <- read.csv("data/processed/understorey_gpp_flux.csv", header=T)

#### Canopy related variables (SLA, LAI, Canopy biomass)
lai_variable <- read.csv("data/processed/lai_data.csv", header=T)
sla_variable <- read.csv("data/processed/sla_data.csv", header=T)
canopy_c_pool <- read.csv("data/processed/canopy_c_pool.csv", header=T)

#### Wood C pool
# we have sapwood and heartwood in the dataframe
# excluded dead trees (before 2018)
wood_c_pool <- read.csv("data/processed/wood_c_pool.csv", header=T)

### standing dead wood c pool
### only report the max standing dead (i.e. 2018 value)
standing_dead_c_pool <- read.csv("data/processed/standing_dead_c_pool.csv", header=T)

#### Fineroot pool
### top 60 cm
fineroot_c_pool <- read.csv("data/processed/fineroot_c_pool.csv", header=T)

#### Understorey aboveground biomass 
understorey_c_pool <- read.csv("data/processed/understorey_aboveground_c_pool_camera.csv", header=T)

#### Soil C content
#### return soil C by depths
soil_c_pool <- read.csv("data/processed/soil_c_pool.csv", header=T)

#### Microbial C pool
#### this pool has data only at 0-10cm depth - Cat's data
microbial_c_pool <- read.csv("data/processed/microbial_c_pool.csv", header=T)

#### Soil mycorrhizal pool
#### But we don't have a P concentration for it and 
#### therefore it's not included in the P budget 
mycorrhizal_c_pool <- read.csv("data/processed/mycorrhizal_c_pool.csv", header=T)

#### Coarse root C pool 
coarse_root_c_pool <- read.csv("data/processed/coarse_root_c_pool.csv", header=T)

#### Leaf litter pool - forest floor leaf litter pool
leaflitter_c_pool <- read.csv("data/processed/leaflitter_c_pool.csv", header=T)

#### Ltter production (leaf, twig, bark, seed)
litter_c_production_flux <- read.csv("data/processed/litter_c_production_flux.csv", header=T)

leaflitter_c_production_flux <- read.csv("data/processed/leaflitter_c_production_flux.csv", header=T)
twiglitter_c_production_flux <- read.csv("data/processed/twiglitter_c_production_flux.csv", header=T)
barklitter_c_production_flux <- read.csv("data/processed/barklitter_c_production_flux.csv", header=T)
seedlitter_c_production_flux <- read.csv("data/processed/seedlitter_c_production_flux.csv", header=T)

#### Frass production
frass_c_production_flux <- read.csv("data/processed/frass_c_production_flux.csv", header=T)

#### Canopy C production
### need to add insect consumption flux back!
### herbivore leaf c consumption flux
### extrapolated based on frass weight, leaf area consumed and sla data
herbivory_leaf_consumption_flux <- read.csv("data/processed/herbivory_leaf_c_consumption_flux.csv", header=T)
canopy_c_production_flux <- read.csv("data/processed/canopy_c_production_flux.csv", header=T)

## based on change in leaf area and litterfall
canopy_c_production_flux_new <- read.csv("data/processed/canopy_c_production_flux_new.csv", header=T)

#### Wood C production
wood_c_production <- read.csv("data/processed/wood_c_production_flux.csv", header=T)

#### Fineroot production
#### root size: < 2mm in diameter, not including the intermediate roots
#### we can only estimate fineroot c production for top 30 cm,
#### but the 30 - 60 cm fienroot biomass is tiny (3% of total), and therefore 
#### we assume the production flux is small and negligible
fineroot_c_production_flux <- read.csv("data/processed/fineroot_c_production_flux.csv", header=T)

#### Understorey production flux 
understorey_c_flux_clipping <- read.csv("data/processed/understorey_c_production_flux.csv", header=T)

#### understorey litter flux
### basically understorey dead 
understorey_litter_c_flux <- read.csv("data/processed/understorey_litter_c_flux.csv", header=T)

#### Coarse root C production
coarse_root_c_flux <- read.csv("data/processed/coarse_root_c_production_flux.csv", header=T)


########################################################################################## 
########################################################################################## 
#####
##### Step 2: Generate P concentrations for all variables based on raw data
#####
process_p_concentration_variables()

############################## Soil ###############################

#### Soil P concentrations 
### three depth: 0 - 10 cm
###              10 - 30 cm
###              30 - 60 cm
soil_p_concentration <- read.csv("data/processed/soil_p_concentration.csv", header=T)
soil_inorganic_p_concentration <- read.csv("data/processed/soil_inorganic_p_concentration.csv", header=T)
soil_organic_p_concentration <- read.csv("data/processed/soil_organic_p_concentration.csv", header=T)

#### Soil phosphate conc, this returns % of P, not % of PO4
soil_phosphate_concentration <- read.csv("data/processed/soil_phosphate_concentration.csv", header=T)

#### Microbial P conc.
#### Top 60 cm
microbial_p_concentration <- read.csv("data/processed/microbial_p_concentration.csv", header=T)

#### Hedley fractionation dataset
### top 10 cm
soil_hedley_p_concentration <- read.csv("data/processed/soil_hedley_p_concentration.csv", header=T)

############################## Plant ###############################
#### Canopy P conc.
canopy_p_concentration <- read.csv("data/processed/canopy_p_concentration.csv", header=T)

#### Leaf litter P conc. 
leaflitter_p_concentration <- read.csv("data/processed/leaflitter_p_concentration.csv", header=T)

#### Wood P conc. 
sapwood_p_concentration <- read.csv("data/processed/sapwood_p_concentration.csv", header=T)

#### Frass P conc.
### frass flux needs to added to canopy P demand each year.
frass_p_concentration <- read.csv("data/processed/frass_p_concentration.csv", header=T)

#### Fineroot P conc.
fineroot_p_concentration <- read.csv("data/processed/fineroot_p_concentration.csv", header=T)

#### Understorey P conc.
understorey_p_concentration <- read.csv("data/processed/understorey_p_concentration.csv", header=T)

#### Understorey litter P conc.
understorey_litter_p_concentration <- read.csv("data/processed/understorey_litter_p_concentration.csv", header=T)

##### Calculate retranslocation coefficients
plant_p_retranslocation_coefficients <- read.csv("data/processed/plant_p_retranslocation_coefficients.csv", header=T)

########################################################################################## 
########################################################################################## 
#####
##### Step 4: Generating P pools and fluxes
#####
############################## P Pools ###############################
#### Soil P pool - top 60 cm
soil_p_pool <- make_soil_p_pool(p_conc=soil_p_concentration,
                                bk_density=soil_bulk_density)
write.csv(soil_p_pool, "data/processed/soil_p_pool.csv", row.names=F)

### Soil inorganic pool - 60 cm
soil_inorganic_p_pool <- make_soil_inorganic_p_pool(p_conc=soil_inorganic_p_concentration,
                                                    bk_density=soil_bulk_density)
write.csv(soil_inorganic_p_pool, "data/processed/soil_inorganic_p_pool.csv", row.names=F)

### soil organic pool - 60 cm
soil_organic_p_pool <- make_soil_organic_p_pool(p_conc=soil_organic_p_concentration,
                                                bk_density=soil_bulk_density)
write.csv(soil_organic_p_pool, "data/processed/soil_organic_p_pool.csv", row.names=F)

#### Soil phosphate pool
#### Top 60 cm
#### Note:
#### This is additional to the microbial PO4-P pool
#### The microbial pool needs to have one step further 
#### to break the cell when extracting.
#### So this pool is more readily available to plants. 
soil_phosphate_pool <- make_soil_phosphate_pool(p_conc=soil_phosphate_concentration,
                                                bk_density=soil_bulk_density)
write.csv(soil_phosphate_pool, "data/processed/soil_phosphate_pool.csv", row.names=F)


#### Soil P pool of different bioavailability
#### Top 10 cm only
soil_p_pool_hedley <- make_soil_p_pool_hedley(p_conc=soil_hedley_p_concentration,
                                              bk_density=soil_bulk_density,
                                              soil_p=soil_p_pool)
write.csv(soil_p_pool_hedley, "data/processed/soil_p_pool_hedley.csv", row.names=F)

#### Microbial P pool 
#### Top 60 cm
microbial_p_pool <- make_microbial_p_pool(p_conc=microbial_p_concentration,
                                          bk_density=soil_bulk_density)
write.csv(microbial_p_pool, "data/processed/microbial_p_pool.csv", row.names=F)

#### Canopy P pool - only for green leaves
canopy_p_pool <- make_canopy_p_pool(p_conc=canopy_p_concentration,
                                    biom=canopy_c_pool)
write.csv(canopy_p_pool, "data/processed/canopy_p_pool.csv", row.names=F)

## calculate change in leaf pool as well as litterfall
dLEAF_litter_flux <- make_dLAI_litter(litter=leaflitter_c_production_flux, 
                                      sla_variable=sla_variable)
canopy_p_pool_new <- make_canopy_p_pool_smoothed(biom=dLEAF_litter_flux)
write.csv(canopy_p_pool_new, "data/processed/canopy_p_pool_new.csv", row.names=F)

### Forest floor leaf litter pool
leaflitter_p_pool <- make_leaflitter_p_pool(p_conc=leaflitter_p_concentration,
                                            c_pool=leaflitter_c_pool,
                                            c_frac=c_fraction)
write.csv(leaflitter_p_pool, "data/processed/leaflitter_p_pool.csv", row.names=F)

#### Wood P pool 
wood_p_pool <- make_wood_p_pool(p_conc=sapwood_p_concentration,
                                c_pool=wood_c_pool,
                                case_consideration = "total")
write.csv(wood_p_pool, "data/processed/wood_p_pool.csv", row.names=F)

sapwood_p_pool <- make_wood_p_pool(p_conc=sapwood_p_concentration,
                                   c_pool=wood_c_pool,
                                   case_consideration = "sapwood")
write.csv(sapwood_p_pool, "data/processed/sapwood_p_pool.csv", row.names=F)

heartwood_p_pool <- make_wood_p_pool(p_conc=sapwood_p_concentration,
                                     c_pool=wood_c_pool,
                                     case_consideration = "heartwood")
write.csv(heartwood_p_pool, "data/processed/heartwood_p_pool.csv", row.names=F)

#### Standing dead p pool
standing_dead_p_pool <- make_wood_p_pool(p_conc=sapwood_p_concentration,
                                         c_pool=standing_dead_c_pool,
                                         case_consideration = "total")
write.csv(standing_dead_p_pool, "data/processed/standing_dead_p_pool.csv", row.names=F)

#### Fine root P biomass pool
fineroot_p_pool <- make_fineroot_p_pool(p_conc=fineroot_p_concentration,
                                        c_pool=fineroot_c_pool)
write.csv(fineroot_p_pool, "data/processed/fineroot_p_pool.csv", row.names=F)

#### Understorey P pool, assume both species contributed equally
#### Also because p_conc and c_pool do not match in time,
#### we are taking the average of p_conc and apply it to c_pool
#### Here we can use Matthias's stereo camera estimate (2) or 
#### Varsha's harvest data (1) to extrapolate for p pool
#### It makes more sense to use harvest at this stage in time!
#### Also, if use Varsha's harvest data, we can use either total or live part of biomass
understorey_p_pool <- make_understorey_p_pool(p_conc=understorey_p_concentration,
                                              p_lit_conc=understorey_litter_p_concentration,
                                              c_pool=understorey_c_pool,
                                              c_frac=c_fraction_ud,
                                              live_or_total = "Total")
write.csv(understorey_p_pool, "data/processed/understorey_p_pool.csv", row.names=F)

### oarse root P pool
### currently assuming sapwood P concentration
coarse_root_p_pool <- make_coarse_root_p_pool(p_conc=sapwood_p_concentration,
                                              c_pool=coarse_root_c_pool,
                                              c_frac=c_fraction)
write.csv(coarse_root_p_pool, "data/processed/coarse_root_p_pool.csv", row.names=F)

############################## P fluxes ###############################
#### Soil P mineralization flux
soil_p_mineralization <- make_soil_p_mineralization_flux(bk_density=soil_bulk_density,
                                                         fineroot_c_pool=fineroot_c_pool,
                                                         which.variable="SoilC")
write.csv(soil_p_mineralization, "data/processed/soil_p_mineralization_flux.csv", row.names=F)

#### Soil P leaching rate
#### estimated based on deep depth (35 - 75 cm) lysimeter data
#### and multiply by water flux of 20 mL m-2 d-1 (over-estimate)
soil_p_leaching <- make_soil_p_leaching_flux()
write.csv(soil_p_leaching, "data/processed/soil_p_leaching_flux.csv", row.names=F)

#### Canopy production flux
canopy_p_flux <- make_canopy_p_production(p_conc=canopy_p_concentration,
                                          c_flux=canopy_c_production_flux,
                                          c_frac=c_fraction)
write.csv(canopy_p_flux, "data/processed/canopy_p_flux.csv", row.names=F)

## considered both change in LAI and litterfall
canopy_p_flux_new <- make_canopy_p_production_new(c_flux=canopy_c_production_flux_new,
                                                  c_frac=c_fraction)
write.csv(canopy_p_flux_new, "data/processed/canopy_p_flux_new.csv", row.names=F)

#### Frass P production
#### Used C fraction for frass to convert c production back to frass biomass
#### We have more p conc than c flux so no need to gap fill. 
frass_c_fraction <- make_frass_c_fraction()
frass_p_production <- make_frass_p_production_flux(p_conc=frass_p_concentration,
                                                   c_flux=frass_c_production_flux,
                                                   c_frac=frass_c_fraction)
write.csv(frass_p_production, "data/processed/frass_p_production_flux.csv", row.names=F)

#### 3.8 Litter P production flux 
#### Literfall biomass (not C) will be calculated within the function
#### for data points where we have C but not P, we can create a separte script
#### and gap-fill P concentration based on average values
leaflitter_p_flux <- make_leaflitter_p_flux(p_conc=leaflitter_p_concentration,
                                            c_flux=leaflitter_c_production_flux,
                                            c_frac=c_fraction)  
write.csv(leaflitter_p_flux, "data/processed/leaflitter_p_flux.csv", row.names=F)

#### Fine root P production flux
fineroot_p_production <- make_fineroot_p_production(p_conc=fineroot_p_concentration,
                                                    c_flux=fineroot_c_production_flux)
write.csv(fineroot_p_production, "data/processed/fineroot_p_production_flux.csv", row.names=F)

#### Fine root litter P production
### assuming P retranslocation coefficient for fine root is 50%
### and fine root c production flux is fine root c litter flux
fineroot_litter_p_flux <- make_fineroot_litter_p_production(p_conc=fineroot_p_concentration,
                                                            c_flux=fineroot_c_production_flux,
                                                            p_retrans=retrans_froot)
write.csv(fineroot_litter_p_flux, "data/processed/fineroot_litter_p_flux.csv", row.names=F)

#### Other litterfall
twig_litter_p_flux <- make_twiglitter_p_flux(p_conc=sapwood_p_concentration, 
                                             litter_flux=twiglitter_c_production_flux)  
write.csv(twig_litter_p_flux, "data/processed/twig_litter_p_flux.csv", row.names=F)

## bark P concentration provided by Kristine
bark_litter_p_flux <- make_barklitter_p_flux(p_conc=sapwood_p_concentration, 
                                             litter_flux=barklitter_c_production_flux)  
write.csv(bark_litter_p_flux, "data/processed/bark_litter_p_flux.csv", row.names=F)

## assume leaf p concentration
seed_litter_p_flux <- make_seedlitter_p_flux(p_conc=canopy_p_concentration, 
                                             litter_flux=seedlitter_c_production_flux)  
write.csv(seed_litter_p_flux, "data/processed/seed_litter_p_flux.csv", row.names=F)

#### Wood p flux
wood_p_flux <- make_wood_p_production(p_conc=sapwood_p_concentration,
                                      c_flux=wood_c_production)
write.csv(wood_p_flux, "data/processed/wood_p_flux.csv", row.names=F)

### Coarse root P flux
coarse_root_p_flux <- make_coarse_root_p_flux(p_conc=sapwood_p_concentration,
                                              c_flux=coarse_root_c_flux,
                                              c_frac=c_fraction)
write.csv(coarse_root_p_flux, "data/processed/coarse_root_p_flux.csv", row.names=F)

#### Understorey production flux
#### Here we can use either stereo camera estimate of biomass (2) or
#### Harvest biomass data (1) to calculate p flux
#### Currently, we are using harvest estimate
understorey_p_flux <- make_understorey_p_flux(p_conc=understorey_p_concentration,
                                              c_flux=understorey_c_flux_clipping,
                                              c_frac=c_fraction_ud)
write.csv(understorey_p_flux, "data/processed/understorey_p_flux.csv", row.names=F)

understorey_litter_p_flux <- make_understorey_litter_p_flux(p_conc=understorey_litter_p_concentration,
                                                            c_flux=understorey_c_flux_clipping,
                                                            #c_flux=understorey_litter_c_flux,
                                                            c_frac=c_fraction_ud)
write.csv(understorey_litter_p_flux, "data/processed/understorey_litter_p_flux.csv", row.names=F)

############################## P retranslocation fluxes ###############################
canopy_P_retranslocation_flux <- calculate_canopy_P_retranslocation_flux(tflux=canopy_p_flux,
                                                                         lflux=leaflitter_p_flux,
                                                                         retransDF=plant_p_retranslocation_coefficients)
write.csv(canopy_P_retranslocation_flux, "data/processed/canopy_P_retranslocation_flux.csv", row.names=F)

fineroot_P_retranslocation_flux <- calculate_fineroot_P_retranslocation_flux(tflux=fineroot_p_production,
                                                                             lflux=fineroot_litter_p_flux,
                                                                             retransDF=plant_p_retranslocation_coefficients)
write.csv(fineroot_P_retranslocation_flux, "data/processed/fineroot_P_retranslocation_flux.csv", row.names=F)

understorey_P_retranslocation_flux <- calculate_understorey_P_retranslocation_flux(tflux=understorey_p_flux,
                                                                                   lflux=understorey_litter_p_flux,
                                                                                   retransDF=plant_p_retranslocation_coefficients)
write.csv(understorey_P_retranslocation_flux, "data/processed/understorey_P_retranslocation_flux.csv", row.names=F)

sapwood_P_retranslocation_flux <- calculate_sapwood_P_retranslocation_flux(tflux=wood_p_flux,
                                                                           retransDF=plant_p_retranslocation_coefficients)
write.csv(sapwood_P_retranslocation_flux, "data/processed/sapwood_P_retranslocation_flux.csv", row.names=F)


coarseroot_P_retranslocation_flux <- calculate_coarseroot_P_retranslocation_flux(tflux=coarse_root_p_flux,
                                                                                 retransDF=plant_p_retranslocation_coefficients)
write.csv(coarseroot_P_retranslocation_flux, "data/processed/coarseroot_P_retranslocation_flux.csv", row.names=F)

############################## delta P Pools ###############################
delta_soil_p_pool <- make_yearly_delta_pool_without_depth_function(inDF=soil_p_pool, 
                                                                   var.col=4)
write.csv(delta_soil_p_pool, "data/processed/delta_soil_p_pool.csv", row.names=F)

delta_soil_phosphate_pool <- make_yearly_delta_pool_without_depth_function(inDF=soil_phosphate_pool, 
                                                                           var.col=4)
write.csv(delta_soil_phosphate_pool, "data/processed/delta_soil_phosphate_pool.csv", row.names=F)

delta_microbial_p_pool <- make_yearly_delta_pool_with_depth_function_microbe(inDF=microbial_p_pool, 
                                                                             var.col=4)
write.csv(delta_microbial_p_pool, "data/processed/delta_microbial_p_pool.csv", row.names=F)

delta_canopy_p_pool <- make_yearly_delta_pool_function(inDF=canopy_p_pool, 
                                                       var.col=3)
write.csv(delta_canopy_p_pool, "data/processed/delta_canopy_p_pool.csv", row.names=F)

delta_wood_p_pool <- make_yearly_delta_pool_function(inDF=wood_p_pool, 
                                                     var.col=3)
write.csv(delta_wood_p_pool, "data/processed/delta_wood_p_pool.csv", row.names=F)

delta_sapwood_p_pool <- make_yearly_delta_pool_function(inDF=sapwood_p_pool, 
                                                        var.col=3)
write.csv(delta_sapwood_p_pool, "data/processed/delta_sapwood_p_pool.csv", row.names=F)

delta_heartwood_p_pool <- make_yearly_delta_pool_function(inDF=heartwood_p_pool, 
                                                          var.col=3)
write.csv(delta_heartwood_p_pool, "data/processed/delta_heartwood_p_pool.csv", row.names=F)

delta_fineroot_p_pool <- make_yearly_delta_pool_function(inDF=fineroot_p_pool, 
                                                         var.col=3)
write.csv(delta_fineroot_p_pool, "data/processed/delta_fineroot_p_pool.csv", row.names=F)

delta_coarse_root_p_pool <- make_yearly_delta_pool_function(inDF=coarse_root_p_pool, 
                                                            var.col=3)
write.csv(delta_coarse_root_p_pool, "data/processed/delta_coarse_root_p_pool.csv", row.names=F)

delta_understorey_p_pool <- make_yearly_delta_pool_function(inDF=understorey_p_pool, 
                                                            var.col=3)
write.csv(delta_understorey_p_pool, "data/processed/delta_understorey_p_pool.csv", row.names=F)

delta_leaflitter_p_pool <- make_yearly_delta_pool_function(inDF=leaflitter_p_pool, 
                                                           var.col=3)
write.csv(delta_leaflitter_p_pool, "data/processed/delta_leaflitter_p_pool.csv", row.names=F)

########################################################################################## 
########################################################################################## 
#####
##### Step 5: Making P budgeting variables and tables, based on raw data
#####

############################## summary tables ###############################
#### Summary Tables
summary_table_concentration <- make_conc_summary_table(norm="unnormalized",
                                                       canopy_p_concentration=canopy_p_concentration,
                                                       sapwood_p_concentration=sapwood_p_concentration,
                                                       fineroot_p_concentration=fineroot_p_concentration,
                                                       leaflitter_p_concentration=leaflitter_p_concentration,
                                                       understorey_p_concentration=understorey_p_concentration,
                                                       understorey_litter_p_concentration=understorey_litter_p_concentration,
                                                       frass_p_concentration=frass_p_concentration,
                                                       microbial_p_concentration=microbial_p_concentration,
                                                       soil_p_concentration=soil_p_concentration,
                                                       soil_inorganic_p_concentration=soil_inorganic_p_concentration,
                                                       soil_organic_p_concentration=soil_organic_p_concentration,
                                                       soil_phosphate_concentration=soil_phosphate_concentration,
                                                       soil_hedley_p_concentration=soil_hedley_p_concentration)


### P pools by treatment and ring
summary_table_pool <- make_pool_summary_table(norm="unnormalized",
                                              soil_p_pool=soil_p_pool,
                                              soil_inorganic_p_pool=soil_inorganic_p_pool,
                                              soil_organic_p_pool=soil_organic_p_pool,
                                              soil_phosphate_pool=soil_phosphate_pool,
                                              soil_p_pool_hedley=soil_p_pool_hedley,
                                              microbial_p_pool=microbial_p_pool,
                                              canopy_p_pool=canopy_p_pool,
                                              leaflitter_p_pool=leaflitter_p_pool,
                                              wood_p_pool=wood_p_pool,
                                              sapwood_p_pool=sapwood_p_pool,
                                              heartwood_p_pool=heartwood_p_pool,
                                              standing_dead_p_pool=standing_dead_p_pool,
                                              fineroot_p_pool=fineroot_p_pool,
                                              understorey_p_pool=understorey_p_pool,
                                              coarse_root_p_pool=coarse_root_p_pool)


summary_table_delta_pool <- make_delta_pool_summary_table(norm="unnormalized",
                                                          soil_p_pool=delta_soil_p_pool,
                                                          soil_phosphate_pool=delta_soil_phosphate_pool,
                                                          microbial_p_pool=delta_microbial_p_pool,
                                                          canopy_p_pool=delta_canopy_p_pool,
                                                          leaflitter_p_pool=delta_leaflitter_p_pool,
                                                          wood_p_pool=delta_wood_p_pool,
                                                          sapwood_p_pool=delta_sapwood_p_pool,
                                                          heartwood_p_pool=delta_heartwood_p_pool,
                                                          fineroot_p_pool=delta_fineroot_p_pool,
                                                          understorey_p_pool=delta_understorey_p_pool,
                                                          coarse_root_p_pool=delta_coarse_root_p_pool)

### P fluxes by treatment and ring
summary_table_flux <- make_flux_summary_table(norm="unnormalized",
                                              soil_p_mineralization=soil_p_mineralization,
                                              soil_p_leaching=soil_p_leaching,
                                              canopy_p_flux=canopy_p_flux,
                                              frass_p_production=frass_p_production,
                                              leaflitter_p_flux=leaflitter_p_flux,
                                              fineroot_p_production=fineroot_p_production,
                                              fineroot_litter_p_flux=fineroot_litter_p_flux,
                                              twig_litter_p_flux=twig_litter_p_flux,
                                              bark_litter_p_flux=bark_litter_p_flux,
                                              seed_litter_p_flux=seed_litter_p_flux,
                                              wood_p_flux=wood_p_flux,
                                              coarse_root_p_flux=coarse_root_p_flux,
                                              understorey_p_flux=understorey_p_flux,
                                              understorey_litter_p_flux=understorey_litter_p_flux)

### C pools by treatment and ring
summary_table_c_pool <- make_c_pool_summary_table(norm="unnormalized",
                                                  canopy_c_pool=canopy_c_pool,
                                                  wood_c_pool=wood_c_pool,
                                                  standing_dead_c_pool=standing_dead_c_pool,
                                                  fineroot_c_pool=fineroot_c_pool,
                                                  coarse_root_c_pool=coarse_root_c_pool,
                                                  understorey_c_pool=understorey_c_pool,
                                                  soil_c_pool=soil_c_pool,
                                                  microbial_c_pool=microbial_c_pool,
                                                  mycorrhizal_c_pool=mycorrhizal_c_pool,
                                                  leaflitter_c_pool=leaflitter_c_pool)

### C fluxes by treatment and ring
summary_table_c_flux <- make_c_flux_summary_table(norm="unnormalized",
                                                  leaflitter_c_production_flux=leaflitter_c_production_flux,
                                                  twiglitter_c_production_flux=twiglitter_c_production_flux,
                                                  barklitter_c_production_flux=barklitter_c_production_flux,
                                                  seedlitter_c_production_flux=seedlitter_c_production_flux,
                                                  canopy_c_production_flux=canopy_c_production_flux,
                                                  wood_c_production=wood_c_production,
                                                  fineroot_c_production_flux=fineroot_c_production_flux,
                                                  coarse_root_c_flux=coarse_root_c_flux,
                                                  understorey_c_flux_clipping=understorey_c_flux_clipping,
                                                  understorey_litter_c_flux=understorey_litter_c_flux,
                                                  frass_c_production_flux=frass_c_production_flux)

### CP ratios
summary_cp_ratios <- make_cp_ratios(norm="unnormalized",
                                    c_pool=summary_table_c_pool,
                                    p_pool=summary_table_pool,
                                    c_flux=summary_table_c_flux,
                                    p_flux=summary_table_flux)


############################## Budget tables ###############################
### vegetation standing P stocks
vegetation_standing_p_stock <- make_vegetation_standing_p_stock(norm="unnormalized",
                                                                leaf=canopy_p_pool,
                                                                wood=wood_p_pool,
                                                                sapwood=sapwood_p_pool,
                                                                fineroot=fineroot_p_pool,
                                                                coarseroot=coarse_root_p_pool,
                                                                understorey=understorey_p_pool,
                                                                dead=standing_dead_p_pool,
                                                                forestfloor=leaflitter_p_pool)

### P mean residence time in plant
plant_p_MRT <- make_plant_P_mean_residence_time(norm="unnormalized",
                                                p_stand=vegetation_standing_p_stock,
                                                p_flux=summary_table_flux)

### Plant P use efficiency
plant_p_use_efficiency <- make_plant_P_use_efficiency(norm="unnormalized",
                                                      c_flux=summary_table_c_flux,
                                                      p_flux=summary_table_flux)

### calculate efficiency of P for GPP
### i.e. GPP / leaf P pool for both overstorey and understorey
### saves a plot.
plant_GPP_efficiency <- make_plant_GPP_efficiency(norm="unnormalized",
                                                  p_pool=summary_table_pool,
                                                  p_flux=summary_table_flux,
                                                  can_gpp=overstorey_gpp_flux,
                                                  und_gpp=understorey_gpp_flux)


#### P budget summary
total_p_budget <- make_total_p_budget(norm="unnormalized",
                                      summary_table_flux,
                                      summary_table_pool,
                                      vegetation_standing_p_stock,
                                      plant_p_MRT,
                                      plant_p_use_efficiency,
                                      plant_GPP_efficiency)


########################################################################################## 
########################################################################################## 
#####
##### Step 6. Plotting P budget figures, based on unnormalized data
#####
##### Note: for the following plotting script in this step,
####        we will need to go into the function
####        to plot.
####        So, firstly, copy and paste inDF = total_p_budget_norm in the console,
####        then open the function, then plot. 
inDF=total_p_budget
inDF2=summary_table_flux
inDF3=plant_GPP_efficiency
make_figure3(inDF=total_p_budget,
             inDF2=summary_table_flux,
             inDF3=plant_GPP_efficiency)


### Concentration
inDF=summary_table_concentration
make_ed_figure2(inDF=summary_table_concentration)

### P pool
inDF=summary_table_pool
make_figure2(inDF=summary_table_pool)

### P flux
inDF=summary_table_flux
make_p_fluxes_summary_plots(inDF=summary_table_flux)

### compare ecosystem P budget to China
make_make_comparison_to_China_pool_figure(soil_p_concentration)

### plot CO2 effect on the same figure
### contains Figure 4, extended data figures 7 and 8
budgetDF=total_p_budget
concDF=summary_table_concentration
poolDF=summary_table_pool
fluxDF=summary_table_flux
deltaDF=summary_table_delta_pool
cpDF=summary_cp_ratios
plot_CO2_effect_on_the_same_figure(budgetDF=total_p_budget,
                                   concDF=summary_table_concentration,
                                   poolDF=summary_table_pool,
                                   fluxDF=summary_table_flux,
                                   deltaDF=summary_table_delta_pool,
                                   cpDF=summary_cp_ratios)

### plot CP ratios
inDF=summary_cp_ratios
ppool=summary_table_pool
plot_ed_figure4(inDF=summary_cp_ratios,
                ppool=summary_table_pool)


### microbial P concentration
make_si_figure2()

### plot P resorption coefficients, si figure 3
plot_plant_resorption_coefficients(plant_p_retranslocation_coefficients)


### make ed figure 1
plot_ed_figure1()



########################################################################################## 
########################################################################################## 
##### Step 7: Analyze time-series (long-term vs short-term, seasonal variation)

analyze_time_effect_on_concentration(canopy_p_concentration=canopy_p_concentration,
                                     sapwood_p_concentration=sapwood_p_concentration,
                                     fineroot_p_concentration=fineroot_p_concentration,
                                     leaflitter_p_concentration=leaflitter_p_concentration,
                                     understorey_p_concentration=understorey_p_concentration,
                                     understorey_litter_p_concentration=understorey_litter_p_concentration,
                                     frass_p_concentration=frass_p_concentration,
                                     microbial_p_concentration=microbial_p_concentration,
                                     soil_p_concentration=soil_p_concentration,
                                     soil_inorganic_p_concentration=soil_inorganic_p_concentration,
                                     soil_organic_p_concentration=soil_organic_p_concentration,
                                     soil_phosphate_concentration=soil_phosphate_concentration,
                                     soil_hedley_p_concentration=soil_hedley_p_concentration)



analyze_time_effect_on_pool(soil_p_pool=soil_p_pool,
                            soil_inorganic_p_pool=soil_inorganic_p_pool,
                            soil_organic_p_pool=soil_organic_p_pool,
                            soil_phosphate_pool=soil_phosphate_pool,
                            soil_p_pool_hedley=soil_p_pool_hedley,
                            microbial_p_pool=microbial_p_pool,
                            canopy_p_pool=canopy_p_pool,
                            leaflitter_p_pool=leaflitter_p_pool,
                            wood_p_pool=wood_p_pool,
                            sapwood_p_pool=sapwood_p_pool,
                            heartwood_p_pool=heartwood_p_pool,
                            standing_dead_p_pool=standing_dead_p_pool,
                            fineroot_p_pool=fineroot_p_pool,
                            understorey_p_pool=understorey_p_pool,
                            coarse_root_p_pool=coarse_root_p_pool)


analyze_time_effect_on_flux(soil_p_mineralization=soil_p_mineralization,
                            soil_p_leaching=soil_p_leaching,
                            canopy_p_flux=canopy_p_flux,
                            frass_p_production=frass_p_production,
                            leaflitter_p_flux=leaflitter_p_flux,
                            fineroot_p_production=fineroot_p_production,
                            fineroot_litter_p_flux=fineroot_litter_p_flux,
                            twig_litter_p_flux=twig_litter_p_flux,
                            bark_litter_p_flux=bark_litter_p_flux,
                            seed_litter_p_flux=seed_litter_p_flux,
                            wood_p_flux=wood_p_flux,
                            coarse_root_p_flux=coarse_root_p_flux,
                            understorey_p_flux=understorey_p_flux,
                            understorey_litter_p_flux=understorey_litter_p_flux,
                            canopy_P_retranslocation_flux=canopy_P_retranslocation_flux,
                            sapwood_P_retranslocation_flux=sapwood_P_retranslocation_flux,
                            understorey_P_retranslocation_flux=understorey_P_retranslocation_flux,
                            fineroot_P_retranslocation_flux=fineroot_P_retranslocation_flux,
                            coarseroot_P_retranslocation_flux=coarseroot_P_retranslocation_flux)


### plot the time sequence data for selected variables
### canopy P pool
plot_canopy_p_pool_time_sequence(canopy_p_pool=canopy_p_pool)

plot_microbial_p_pool_time_sequence(microbial_p_pool=microbial_p_pool)

#plot_leaflitter_p_pool_time_sequence(leaflitter_p_pool=leaflitter_p_pool)

plot_fineroot_p_concentration_time_sequence(fineroot_p_concentration=fineroot_p_concentration)


########################################################################################## 
########################################################################################## 
#####
##### Step 7: Making P budgeting variables and tables, by using bootstrapping method
#####

############################## summary tables ###############################

norm = "bootstrap"

#### Summary Tables
summary_table_concentration_bootstrap <- make_conc_summary_table_bootstrap(norm="bootstrap",
                                                                           canopy_p_concentration=canopy_p_concentration,
                                                                           sapwood_p_concentration=sapwood_p_concentration,
                                                                           fineroot_p_concentration=fineroot_p_concentration,
                                                                           leaflitter_p_concentration=leaflitter_p_concentration,
                                                                           understorey_p_concentration=understorey_p_concentration,
                                                                           understorey_litter_p_concentration=understorey_litter_p_concentration,
                                                                           frass_p_concentration=frass_p_concentration,
                                                                           microbial_p_concentration=microbial_p_concentration,
                                                                           soil_p_concentration=soil_p_concentration,
                                                                           soil_inorganic_p_concentration=soil_inorganic_p_concentration,
                                                                           soil_organic_p_concentration=soil_organic_p_concentration,
                                                                           soil_phosphate_concentration=soil_phosphate_concentration,
                                                                           soil_hedley_p_concentration=soil_hedley_p_concentration)


### P pools by treatment
summary_table_pool_bootstrap <- make_pool_summary_table_bootstrap(norm="bootstrap",
                                                                  soil_p_pool=soil_p_pool,
                                                                  soil_inorganic_p_pool=soil_inorganic_p_pool,
                                                                  soil_organic_p_pool=soil_organic_p_pool,
                                                                  soil_phosphate_pool=soil_phosphate_pool,
                                                                  soil_p_pool_hedley=soil_p_pool_hedley,
                                                                  microbial_p_pool=microbial_p_pool,
                                                                  canopy_p_pool=canopy_p_pool,
                                                                  leaflitter_p_pool=leaflitter_p_pool,
                                                                  wood_p_pool=wood_p_pool,
                                                                  sapwood_p_pool=sapwood_p_pool,
                                                                  heartwood_p_pool=heartwood_p_pool,
                                                                  standing_dead_p_pool=standing_dead_p_pool,
                                                                  fineroot_p_pool=fineroot_p_pool,
                                                                  understorey_p_pool=understorey_p_pool,
                                                                  coarse_root_p_pool=coarse_root_p_pool)


### delta p pools
summary_table_delta_pool_bootstrap <- make_delta_pool_summary_table_bootstrap(norm="bootstrap",
                                                                              soil_p_pool=delta_soil_p_pool,
                                                                              soil_phosphate_pool=delta_soil_phosphate_pool,
                                                                              microbial_p_pool=delta_microbial_p_pool,
                                                                              canopy_p_pool=delta_canopy_p_pool,
                                                                              leaflitter_p_pool=delta_leaflitter_p_pool,
                                                                              wood_p_pool=delta_wood_p_pool,
                                                                              sapwood_p_pool=delta_sapwood_p_pool,
                                                                              heartwood_p_pool=delta_heartwood_p_pool,
                                                                              fineroot_p_pool=delta_fineroot_p_pool,
                                                                              understorey_p_pool=delta_understorey_p_pool,
                                                                              coarse_root_p_pool=delta_coarse_root_p_pool)


### P fluxes by treatment and ring
summary_table_flux_bootstrap <- make_flux_summary_table_bootstrap(norm="bootstrap",
                                                                  soil_p_mineralization=soil_p_mineralization,
                                                                  soil_p_leaching=soil_p_leaching,
                                                                  canopy_p_flux=canopy_p_flux,
                                                                  frass_p_production=frass_p_production,
                                                                  leaflitter_p_flux=leaflitter_p_flux,
                                                                  fineroot_p_production=fineroot_p_production,
                                                                  fineroot_litter_p_flux=fineroot_litter_p_flux,
                                                                  twig_litter_p_flux=twig_litter_p_flux,
                                                                  bark_litter_p_flux=bark_litter_p_flux,
                                                                  seed_litter_p_flux=seed_litter_p_flux,
                                                                  wood_p_flux=wood_p_flux,
                                                                  coarse_root_p_flux=coarse_root_p_flux,
                                                                  understorey_p_flux=understorey_p_flux,
                                                                  understorey_litter_p_flux=understorey_litter_p_flux,
                                                                  canopy_P_retranslocation_flux=canopy_P_retranslocation_flux,
                                                                  sapwood_P_retranslocation_flux=sapwood_P_retranslocation_flux,
                                                                  understorey_P_retranslocation_flux=understorey_P_retranslocation_flux,
                                                                  fineroot_P_retranslocation_flux=fineroot_P_retranslocation_flux,
                                                                  coarseroot_P_retranslocation_flux=coarseroot_P_retranslocation_flux)

### C pools by treatment and ring
summary_table_c_pool_bootstrap <- make_c_pool_summary_table_bootstrap(norm="bootstrap",
                                                                      canopy_c_pool=canopy_c_pool,
                                                                      wood_c_pool=wood_c_pool,
                                                                      standing_dead_c_pool=standing_dead_c_pool,
                                                                      fineroot_c_pool=fineroot_c_pool,
                                                                      coarse_root_c_pool=coarse_root_c_pool,
                                                                      understorey_c_pool=understorey_c_pool,
                                                                      soil_c_pool=soil_c_pool,
                                                                      microbial_c_pool=microbial_c_pool,
                                                                      mycorrhizal_c_pool=mycorrhizal_c_pool,
                                                                      leaflitter_c_pool=leaflitter_c_pool)

### C fluxes by treatment and ring
summary_table_c_flux_bootstrap <- make_c_flux_summary_table_bootstrap(norm="bootstrap",
                                                                      leaflitter_c_production_flux=leaflitter_c_production_flux,
                                                                      twiglitter_c_production_flux=twiglitter_c_production_flux,
                                                                      barklitter_c_production_flux=barklitter_c_production_flux,
                                                                      seedlitter_c_production_flux=seedlitter_c_production_flux,
                                                                      canopy_c_production_flux=canopy_c_production_flux,
                                                                      wood_c_production=wood_c_production,
                                                                      fineroot_c_production_flux=fineroot_c_production_flux,
                                                                      coarse_root_c_flux=coarse_root_c_flux,
                                                                      understorey_c_flux_clipping=understorey_c_flux_clipping,
                                                                      understorey_litter_c_flux=understorey_litter_c_flux,
                                                                      frass_c_production_flux=frass_c_production_flux)


### CP ratios
summary_cp_ratios_bootstrap <- make_cp_ratios_bootstrap(norm="bootstrap",
                                                        soil_p_pool=soil_p_pool,
                                                        microbial_p_pool=microbial_p_pool,
                                                        canopy_p_pool=canopy_p_pool,
                                                        leaflitter_p_pool=leaflitter_p_pool,
                                                        wood_p_pool=wood_p_pool,
                                                        sapwood_p_pool=sapwood_p_pool,
                                                        heartwood_p_pool=heartwood_p_pool,
                                                        standing_dead_p_pool=standing_dead_p_pool,
                                                        fineroot_p_pool=fineroot_p_pool,
                                                        understorey_p_pool=understorey_p_pool,
                                                        coarse_root_p_pool=coarse_root_p_pool,
                                                        canopy_c_pool=canopy_c_pool,
                                                        wood_c_pool=wood_c_pool,
                                                        standing_dead_c_pool=standing_dead_c_pool,
                                                        fineroot_c_pool=fineroot_c_pool,
                                                        coarse_root_c_pool=coarse_root_c_pool,
                                                        understorey_c_pool=understorey_c_pool,
                                                        soil_c_pool=soil_c_pool,
                                                        microbial_c_pool=microbial_c_pool,
                                                        mycorrhizal_c_pool=mycorrhizal_c_pool,
                                                        leaflitter_c_pool=leaflitter_c_pool,
                                                        understorey_litter_c_flux=understorey_litter_c_flux,
                                                        understorey_litter_p_flux=understorey_litter_p_flux,
                                                        frass_c_production_flux=frass_c_production_flux,
                                                        frass_p_production=frass_p_production)



############################## Compare bootstrapped and original results ###############################
### To compare bootstrapped CO2 effect against the original results
compare_bootstrapped_and_original_CO2_effect_size(summary_table_concentration=summary_table_concentration,
                                                  summary_table_concentration_bootstrap=summary_table_concentration_bootstrap,
                                                  summary_table_pool=summary_table_pool,
                                                  summary_table_pool_bootstrap=summary_table_pool_bootstrap,
                                                  summary_table_flux=summary_table_flux,
                                                  summary_table_flux_bootstrap=summary_table_flux_bootstrap)




############################## Budget tables ###############################
### vegetation standing P stocks
#total_p_budget_bootstrap <- make_total_p_budget_bootstrap(norm="bootstrap",
#                                                          canopy_p_pool=canopy_p_pool,
#                                                          wood_p_pool=wood_p_pool,
#                                                          sapwood_p_pool=sapwood_p_pool,
#                                                          fineroot_p_pool=fineroot_p_pool,
#                                                          coarse_root_p_pool=coarse_root_p_pool,
#                                                          understorey_p_pool=understorey_p_pool,
#                                                          standing_dead_p_pool=standing_dead_p_pool,
#                                                          leaflitter_p_pool=leaflitter_p_pool,
#                                                          soil_p_mineralization=soil_p_mineralization,
#                                                          soil_p_leaching=soil_p_leaching,
#                                                          canopy_p_flux=canopy_p_flux,
#                                                          frass_p_production=frass_p_production,
#                                                          leaflitter_p_flux=leaflitter_p_flux,
#                                                          fineroot_p_production=fineroot_p_production,
#                                                          fineroot_litter_p_flux=fineroot_litter_p_flux,
#                                                          twig_litter_p_flux=twig_litter_p_flux,
#                                                          bark_litter_p_flux=bark_litter_p_flux,
#                                                          seed_litter_p_flux=seed_litter_p_flux,
#                                                          wood_p_flux=wood_p_flux,
#                                                          coarse_root_p_flux=coarse_root_p_flux,
#                                                          understorey_p_flux=understorey_p_flux,
#                                                          understorey_litter_p_flux=understorey_litter_p_flux,
#                                                          canopy_P_retranslocation_flux=canopy_P_retranslocation_flux,
#                                                          sapwood_P_retranslocation_flux=sapwood_P_retranslocation_flux,
#                                                          understorey_P_retranslocation_flux=understorey_P_retranslocation_flux,
#                                                          fineroot_P_retranslocation_flux=fineroot_P_retranslocation_flux,
#                                                          coarseroot_P_retranslocation_flux=coarseroot_P_retranslocation_flux,
#                                                          canopy_c_production_flux=canopy_c_production_flux,
#                                                          wood_c_production=wood_c_production,
#                                                          fineroot_c_production_flux=fineroot_c_production_flux,
#                                                          coarse_root_c_flux=coarse_root_c_flux,
#                                                          understorey_c_flux_clipping=understorey_c_flux_clipping,
#                                                          frass_c_production_flux=frass_c_production_flux,
#                                                          twiglitter_c_production_flux=twiglitter_c_production_flux,
#                                                          barklitter_c_production_flux=barklitter_c_production_flux,
#                                                          seedlitter_c_production_flux=seedlitter_c_production_flux)
#
#
########################################################################################### 
########################################################################################### 
#
#### plot CO2 effect on the same figure
#budgetDF=total_p_budget_bootstrap
#concDF=summary_table_concentration_bootstrap
#poolDF=summary_table_pool_bootstrap
#fluxDF=summary_table_flux_bootstrap
#deltaDF=summary_table_delta_pool_bootstrap
#cpDF=summary_cp_ratios_bootstrap
#plot_CO2_effect_on_the_same_figure_bootstrap(budgetDF=total_p_budget_bootstrap,
#                                             concDF=summary_table_concentration_bootstrap,
#                                             poolDF=summary_table_pool_bootstrap,
#                                             fluxDF=summary_table_flux_bootstrap,
#                                             deltaDF=summary_table_delta_pool_bootstrap,
#                                             cpDF=summary_cp_ratios_bootstrap)
#




###### ---------------- End -------------------- ######
#### clear wk space
print("end of project")
rm(list=ls(all=TRUE))
options(war=0)

