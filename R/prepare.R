
if(!dir.exists("data")) {
    dir.create("data")
}

if(!dir.exists("output")) {
    dir.create("output")
}

if(!dir.exists("output/checks")) {
    dir.create("output/checks")
}

if(!dir.exists("output/summary_tables")) {
    dir.create("output/summary_tables")
}

if(!dir.exists("output/figures")) {
    dir.create("output/figures")
}

if(!dir.exists("output/si_figures")) {
    dir.create("output/si_figures")
}




if(!require(HIEv)){
    stop("Install the HIEv package first from bitbucket.org/remkoduursma/HIEv")
}

setToken(tokenfile="tokenfile.txt", quiet=TRUE)
setToPath("data/raw")

if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               readxl, 
               lubridate,
               gdata,
               ggplot2,
               RColorBrewer,
               knitr,
               imputeTS,
               lme4,
               car,
               treemapify,
               matrixStats,
               treemapify,
               multcomp,
               grid,  
               gridExtra,  # plot
               cowplot,    # plot
               mgcv)       # gam


# Loading constants
source("R/constants.R")

# Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)

# color blind friendly
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set3Palette <- brewer.pal(n = 10, name = "Set3")

YlOrRdPalette <- rev(brewer.pal(n = 9, name = "YlOrRd"))

GreensPalette <- rev(brewer.pal(n = 9, name = "Greens"))

SpectralPalette <- brewer.pal(n = 9, name = "Spectral")

Diverge_hsv_Palette <- colorspace::diverge_hcl(8)

Pastel1Palette <- brewer.pal(n=9, name = "Pastel1")


