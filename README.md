---
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# EcoStatusR

<!-- badges: start -->
<!-- badges: end -->

## Overview

EcoStatusR is a tool used to classify the ecological status of species
varying in geographic, phylogenetic, and functional trait rarity. The
ecological status proposed in EcoStatusR can be used to infer larger
organismal strategy.

- get_phy() sources a choice of phylogeny from Smith & Brown 2018
- build_trait_data() constructs a dataframe of chosen functional traits
  from the LEDA trait database
- calc_range_size() calculates the global range size of species using
  GBIF data
- avg_evol_dist() calculates the evolutionary distinctiveness of species
  across multiple phylogenetic extents
- fun_dist() calculates functional distinctiveness (FD) for species
  based on trait data
- scale_by_median() scales desired data within a dataframe using median
  z scores
- elbow_plot() displays an elbowplot for different K means for a variable
- compute_wcss() computes the Within-Cluster Sum of Squares (WCSS) for a given variable and a range of cluster values (k) using k-means clustering
- choose_optimal_k() prompts the user to input the optimal number of clusters (k) for a given variable.
- define_custom_k_ranges() performs k-means clustering on a variable and define custom ranges based on cluster centers
- check_eco_status() classifies the ecological status of chosen species
  within a dataframe
- create_range_polygons() creates convex hulls for a species using cleaned gbif occurence data
- plot_convex_hulls() plots species' occurence polygons onto a world map
- range_poly_map() creates and plots species' occurence polygons onto a world map 
- plot_eco_status() visualizes ecological statuses in 3D space

## Installation

You can install the development version of EcoStatusR like so:

``` r
devtools::install_github("anytko/EcoStatusR")
```

## Example

This is a basic example which demonstrates EcoStatusR useage and shows
how you can determine the ecological status of common tree genera using SLA, seed_mass, and canopy_height:


```r
library(EcoStatusR)
```

## Create Trait Dataframe

```r
trait_df <- build_trait_data(columns_to_select = c("SLA", "seed_mass", "canopy_height"), genera = c("Acer_", "Pinus_", "Fraxinus_", "Quercus_", "Tsuga_", "Ulmus", "Populus", "Betula_"))

trait_df <- rownames_to_column(trait_df, var = "species_name")
```

## Input Phylogeny
Users can input their own phylogeny or use one of the four phylogeneies listed in Smith & Brown 2018 including: 

* ALLMB: GenBank and Open Tree of Life taxa with a backbone provided by Magallón et al. 2015
* ALLOTB: GenBank and Open Tree of Life taxa with a backbone provided by Open Tree of Life version 9.1
* GBMB: GenBank taxa with a backbone provided by Magallón et al. 2015
* GBOTB: GenBank taxa with a backbone provided by Open Tree of Life version 9.1"


```r
GBMB_phylogeny <- get_phy("GBMB")
```

## Calculate Range Size

```r
trait_range_df <- calc_range_size(data_frame = trait_df, num_cores = 6, gbif_limit = 2500)
#> Warning in split.default(species_names, 1:num_cores): data length is not a
#> multiple of split variable

trait_range_df$species_name <- gsub("^\\d+\\.", "", trait_range_df$species_name)
```


## Plot Range Polygons
### Plot the range polygons of a species from the table above: Acer campestre

```r
acer_campestre_range <- data.frame(species_name = c("Acer campestre"))

range_poly_map(data_frame = acer_campestre_range)
```


## Calculate Average Evolutionary Distinctiveness


```r
trait_range_evol_df <- avg_evol_dist(phy = GBMB_phylogeny, data_frame = trait_range_df, num_cores = 6, time_slices = c(5, 10, 20, 35))
```

## Calculate Functional Distinctiveness
In order to calculate functional distinctiveness, we need to remove any species with NA values for any trait. 

```r
merged_df <- merge(merge(trait_df, trait_range_df, by = "species_name", all = TRUE), trait_range_evol_df, by = "species_name", all = TRUE)

merged_df <- na.omit(merged_df)
```


```r
fun_trait_range_evol_df <- fun_dist(data_frame = merged_df, trait_columns = c("SLA", "seed_mass", "canopy_height"))
```

## Transform Dataframe Around the Median

```r
scaled_df <- scale_by_median(data_frame = fun_trait_range_evol_df, columns_chosen = c("range_size", "mean_evol_dist"))
```

## Create Elbow Plots to Detmine Optimal K Means
### The optimal K means value can be determined by finding the "elbow" or bend in the plot. For example, in the three plots below a K = 3 represents the plot's elbow.


```r
range_elbow_plot <- elbow_plot(data = scaled_df, variable = "range_size")
```

<div class="figure">
<img src="man/figures/README-Elbow Plot Range Size-1.png" alt="plot of chunk Elbow Plot Range Size" width="100%" />
<p class="caption">plot of chunk Elbow Plot Range Size</p>
</div>


```r
evol_dist_elbow_plot <- elbow_plot(data = scaled_df, variable = "mean_evol_dist")
```

<div class="figure">
<img src="man/figures/README-Elbow Plot Evolutionary Distinctiveness-1.png" alt="plot of chunk Elbow Plot Evolutionary Distinctiveness" width="100%" />
<p class="caption">plot of chunk Elbow Plot Evolutionary Distinctiveness</p>
</div>


```r
fun_dist_elbow_plot <- elbow_plot(data = scaled_df, variable = "fun_dist")
```

<div class="figure">
<img src="man/figures/README-Elbow Plot Functional Distinctiveness-1.png" alt="plot of chunk Elbow Plot Functional Distinctiveness" width="100%" />
<p class="caption">plot of chunk Elbow Plot Functional Distinctiveness</p>
</div>


## Determine Eco-Evolutionary Status
### Hint: Use the elbow plots above to determine optimal K means values

```r
eco_stat_df <- check_eco_status_k(data_frame = scaled_df, range_size_col = "range_size", mean_evol_dist_col = "mean_evol_dist", fun_dist_col = "fun_dist", range_size_k = 3, mean_evol_dist_k = 3, fun_dist_k = 3)
```



## Visualizing Ecological Status 

We can map the ecological statuses of species from this dataframe onto interactive 3D space. This object can be manipulated and positioned by the user in order to best visualize the respective ecological statuses. 



```r
figure <- plot_eco_status(data_frame = eco_stat_df, fun_dist = "fun_dist", evol_dist = "mean_evol_dist", range_size = "range_size")
```


