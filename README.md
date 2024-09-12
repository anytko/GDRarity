<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoFunPhy

<!-- badges: start -->
<!-- badges: end -->

## Overview

GeoFunPhy is a tool used to classify the eco-evolutionary rarity of
species varying in geographic, phylogenetic, and functional trait
rarity. The ecological status proposed in GeoFunPhy can be used to infer
larger organismal strategy.

-   get\_phy() sources a choice of phylogeny from Smith & Brown 2018
-   build\_trait\_data() constructs a dataframe of chosen functional
    traits from the LEDA trait database
-   calc\_range\_size() calculates the range size of species
-   avg\_evol\_dist() calculates the evolutionary distinctiveness of
    species across distinct times and multiple phylogenetic extents
-   fun\_dist() calculates functional distinctiveness (FD) for species
    based on trait data
-   scale\_by\_median() scales desired data within a dataframe using
    median z scores
-   elbow\_plot() displays an elbowplot for different K means for a
    variable
-   compute\_wcss() computes the Within-Cluster Sum of Squares (WCSS)
    for a given variable and a range of cluster values (k) using k-means
    clustering
-   define\_custom\_k\_ranges() performs k-means clustering on a
    variable and define custom ranges based on cluster centers
-   check\_EER\_status\_k() classifies the eco-evolutionary rarity (EER)
    of chosen species within a dataframe
-   get\_range\_convex\_hulls() creates convex hulls for a species using
    cleaned GBIF occurence data
-   clip\_polygons\_to\_land() clips range convex hulls to continent
    boundaries
-   range\_sizes() calculates the range size of clipped convex hulls
-   get\_continent\_sf() reads and formats a GeoJSON document with
    continent data
-   plot\_convex\_hulls() plots species’ occurence polygons onto a world
    map
-   range\_poly\_map() creates and plots species’ occurence polygons
    onto a world map
-   plot\_EER\_status() visualizes eco-evolutionary rarity categories in
    3D space

## Installation

You can install the development version of GeoFunPhy like so:

    devtools::install_github("anytko/GeoFunPhy")

## Example

This is a basic example which demonstrates GeoFunPhy useage and shows
how you can determine the ecological status of common tree genera using
SLA, seed\_mass, and canopy\_height:

    library(GeoFunPhy)

## Create Trait Dataframe

    trait_df <- build_trait_data(columns_to_select = c("SLA", "seed_mass", "canopy_height"), genera = c("Acer_", "Pinus_", "Fraxinus_", "Quercus_", "Tsuga_", "Ulmus", "Populus", "Betula_"))

    trait_df <- rownames_to_column(trait_df, var = "species_name")

## Input Phylogeny

Users can input their own phylogeny or use one of the four phylogeneies
listed in Smith & Brown 2018 including:

-   ALLMB: GenBank and Open Tree of Life taxa with a backbone provided
    by Magallón et al. 2015
-   ALLOTB: GenBank and Open Tree of Life taxa with a backbone provided
    by Open Tree of Life version 9.1
-   GBMB: GenBank taxa with a backbone provided by Magallón et al. 2015
-   GBOTB: GenBank taxa with a backbone provided by Open Tree of Life
    version 9.1”

<!-- -->

    GBMB_phylogeny <- get_phy("GBMB")

## Calculate Range Sizes

    range_sizes_df <- calc_range_size(data_frame = trait_df, num_cores = 7, gbif_limit = 2500)

## Plot Range Polygons

### Plot the range polygons of a species from the table above: Acer campestre

    acer_campestre_range <- data.frame(species_name = c("Acer campestre"))

    range_poly_map(data_frame = acer_campestre_range)
    #> Warning in RColorBrewer::brewer.pal(min(n_species, 12), "Set1"): minimal value for n is 3, returning requested palette with 3 different levels

## Calculate Average Evolutionary Distinctiveness

    trait_range_evol_df <- avg_evol_dist(phy = GBMB_phylogeny, data_frame = trait_df, num_cores = 6, time_slices = c(5, 10, 20, 35))

## Calculate Functional Distinctiveness

In order to calculate functional distinctiveness, we need to remove any
species with NA values for any trait.

    merged_df <- merge(merge(trait_df, range_sizes_df, by = "species_name", all = TRUE), trait_range_evol_df, by = "species_name", all = TRUE)

    merged_df <- na.omit(merged_df)

    fun_trait_range_evol_df <- fun_dist(data_frame = merged_df, trait_columns = c("SLA", "seed_mass", "canopy_height"))

## Transform Dataframe Around the Median

    scaled_df <- scale_by_median(data_frame = fun_trait_range_evol_df, columns_chosen = c("range_size", "mean_evol_dist"))

## Create Elbow Plots to Detmine Optimal K Means

### The optimal K means value can be determined by finding the “elbow” or bend in the plot. For example, in the three plots below a K = 3 represents the plot’s elbow.

    range_elbow_plot <- elbow_plot(data = scaled_df, variable = "range_size")

<img src="man/figures/README-Elbow Plot Range Size-1.png" width="100%" />

    evol_dist_elbow_plot <- elbow_plot(data = scaled_df, variable = "mean_evol_dist")

<img src="man/figures/README-Elbow Plot Evolutionary Distinctiveness-1.png" width="100%" />

    fun_dist_elbow_plot <- elbow_plot(data = scaled_df, variable = "fun_dist")

<img src="man/figures/README-Elbow Plot Functional Distinctiveness-1.png" width="100%" />

## Determine Eco-Evolutionary Rarity Status

### Hint: Use the elbow plots above to determine optimal K means values

    eco_stat_df <- check_EER_status_k(data_frame = scaled_df, range_size_col = "range_size", mean_evol_dist_col = "mean_evol_dist", fun_dist_col = "fun_dist", range_size_k = 3, mean_evol_dist_k = 3, fun_dist_k = 3)

## Visualizing Eco-evolutionary Rarity Status

We can map the EER of species from this dataframe onto interactive 3D
space. This object can be manipulated and positioned by the user in
order to best visualize the respective ecological statuses.

    figure <- plot_EER_status(data_frame = eco_stat_df, fun_dist = "fun_dist", evol_dist = "mean_evol_dist", range_size = "range_size")
