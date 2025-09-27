
<p style="font-size: 2.5em; font-weight: bold; margin-bottom: 0;">
GDRarity
<img src="vignettes/logo.png" align="right" width="120" alt="GDRarity logo"/>
</p>

# An R package for the Global Model of Discretized Rarity

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17214385.svg)](https://doi.org/10.5281/zenodo.17214385)
<!-- badges: end -->

## Overview

The Global Model of Discretized Rarity (GDR) integrates geographic,
functional, and phylogenetic dimensions of rarity at regional and local
scales. GDRarity is an R package that characterizes the GDR and its 63
restrictions.

Its webpage is at <https://anytko.github.io/GDRarity/>; its source code
is at <https://github.com/anytko/GDRarity>.

## Installation

You can install the development version of GDRarity like so:

``` r
devtools::install_github("anytko/GDRarity")
```

### Features

- Automates restriction selection based on available data.
- Characterizes the rarity of species using multiple restrictions
  simultaneously.
- Tests various methods of calculating geographic, functional, and
  phylogenetic dimensions of rarity and rarity thresholding.
- Analyzes restriction performance in explaining a response variable
  (i.e. biological processes).
- Accepts additional, user specified dimensions of rarity to create or
  augment new restrictions of the GDR.

### Caveats

- Only pulls plant trait data from the LEDA and BIEN plant trait
  databases.
- Certain methods, such as calculating regional geographic rarity using
  extent of occurrence (range size) requires an internet connection and
  can be computationally intensive.
- Does not incorporate Rabinowitz Rarity as a GDR restriction, but
  habitat specificity can be added as an additional, user specified
  dimension of rarity.

## Example

The 63 restrictions of the GDR can be applied using phylogeny, species,
abundance, and trait data within one function.

``` r
library(GDRarity)
```

``` r
species_70_path <- system.file("extdata", "species_70.csv", package = "GDRarity")
abundance_70_path <- system.file("extdata", "abundance_70.csv", package = "GDRarity")

species_70 <- read.csv(species_70_path)
abundance_70 <- read.csv(abundance_70_path)

all_restrictions <- gdrare_pipeline(
      species_df = species_70,
      abundance_df = abundance_70,
      trait_cols = c("SLA", "seed_mass", "canopy_height"),
      geo_rarity_method = "taxonomic",
      fun_rarity_method = "min_distance")
```

Results of Restriction GRFLPR

``` r
all_restrictions$classified_df %>%
  select(species, GR_raw, FL_raw, PR_raw, GRFLPR) %>%
  head(6) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%  
  knitr::kable()
```

| species                | GR_raw | FL_raw | PR_raw | GRFLPR    |
|:-----------------------|-------:|-------:|-------:|:----------|
| Acer_campestre         |  0.656 |  0.409 |  2.193 | GR-FL+PR- |
| Alnus_glutinosa        |  0.447 |  0.237 |  2.442 | GR-FL-PR- |
| Alopecurus_myosuroides |  0.825 |  0.144 | -1.178 | GR-FL-PR- |
| Anthemis_cotula        |  0.871 |  0.134 | -0.008 | GR-FL-PR- |
| Arum_maculatum         |  0.565 |  0.423 |  3.105 | GR-FL+PR+ |
| Calluna_vulgaris       |  0.446 |  0.129 | -0.564 | GR-FL-PR- |
