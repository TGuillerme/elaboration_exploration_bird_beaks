# Innovation and elaboration on the avian tree of life

Author(s): [Thomas Guillerme](https://github/TGuillerme), [Jen Bright](https://www.hull.ac.uk/staff-directory/jen-bright), [Christopher Cooney](https://www.cooneylab.co.uk/), Zoë Varley, [Natalie Cooper](http://nhcooper123.github.io/), [Andrew P. Beckerman](https://andbeck.github.io/beckslab/), [Gavin H. Thomas](https://www.sheffield.ac.uk/biosciences/people/academic-staff/gavin-thomas)

This repository contains all the code and data used in [the paper](https://www.science.org/doi/10.1126/sciadv.adg1641).

[![DOI](https://zenodo.org/badge/337779300.svg)](https://zenodo.org/badge/latestdoi/337779300)

## Poster

 You can find the poster associated to this work [here](https://figshare.com/articles/poster/Innovation_and_elaboration_on_the_avian_tree_of_life/20480754).

## Supplementary material

 * The supplementary tables and figures mentioned in the main text is available from [DOI:10.1126/sciadv.adg1641](https://www.science.org/doi/10.1126/sciadv.adg1641).
 * The supplementary information about the mcmcmcglmmm method is available as a [reproducible vignette](https://raw.rawgit.net/TGuillerme/mcmcmcglmmm/main/inst/MCMCglmm_mini_chains.html).
 * The supplementary information about the elaboration and innovation analysis (or projection analysis) is available as a [reproducible vignette](https://raw.rawgit.net/TGuillerme/dispRity/master/inst/vignettes/Projection_analysis.html).

## Data

The raw data phylogenetic data is available from [here](https://birdtree.org
The raw morphological data is available from [here](https://figshare.com/articles/dataset/Innovation_and_elaboration_on_the_avian_tree_of_life/20480355).
You can access the processed data used in this analysis [here](https://figshare.com/articles/dataset/Innovation_and_elaboration_on_the_avian_tree_of_life/20480355).
If you use either the processed or raw phylogenetic data in a publication, please cite the following papers [Cooney et al 2017](https://www.nature.com/articles/nature21074), [Chira et al 2020](https://royalsocietypublishing.org/doi/full/10.1098/rspb.2020.1585) and [Hughes et al 2022](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.13905).

## Short reproducible examples

 * To reproduce the mcmcmcglmmm method, please refer to the [detailed vignette in the `mcmcmcglmmm` package](https://github.com/TGuillerme/mcmcmcglmmm/blob/main/inst/MCMCglmm_mini_chains.Rmd).
 * To reproduce the elaboration and innovation analysis, please refer to the [detailed vignette in the `dispRity` package](https://github.com/TGuillerme/dispRity/blob/mcmcmcglmmm/inst/vignettes/Projection_analysis.Rmd).

## Reproducing the whole analysis

To reproduce the entire paper, you can follow these steps:

 1. **Data download**: you can get the raw (and processed `mcmcmcglmmm` data) from [here](https://figshare.com/articles/dataset/Innovation_and_elaboration_on_the_avian_tree_of_life/20480355).
 2. **Data and packages preparation**: run the following scripts `01-Data_preparation.Rmd` to generate the base data for the anlysis (the shapespace and the cleaned tree distribution). This script takes approximatively 5 minutes to run.
 3. **mcmcmcglmmm analysis** OPTIONAL: this script (`02-mcmcmcglmmm_analysis.Rmd`) calculates the pGLMM model. It takes 3 CPU years to run and requires some manipulations on a cluster so can be skipped by directly using the data provided in `Data/Processed`. If you have difficulties reruning this analyses or a modification thereof please contact [Thomas](mailto:guillert@tcd.ie). For a more manageable example, you can refer to the [reproducible vignette](https://raw.rawgit.net/TGuillerme/mcmcmcglmmm/main/inst/MCMCglmm_mini_chains.html).
 4. **Measuring elaboration and innovation**: run the script `03-Elaboration_innovation_analysis.Rmd` to calculate all the elaboration and innovation data and data subsections for plotting. This script computes the core calculations of the project. For a smaller example, you can refer to the [reproducible vignette](https://raw.rawgit.net/TGuillerme/dispRity/master/inst/vignettes/Projection_analysis.html).
 5. **Plotting the ellipses for each group**: run the script `05-Figure_with_phylogeny.Rmd` to reproduce figure 1. 
 6. **Plotting the phylogeny**: run the script `04-Figure_with_ellipse_scalling_and_8D_histograms.Rmd` to reproduce figure 2.
 7. **Elaboration and innovation statistics**: run the script `07-Orthogonality_and_statistics.Rmd` to calculate the elaboration and innovation statistics in the manuscript and reproduce figure 3 and 4.
 8. **Supplementary materials**: runt the script `08-Supplementary_figures_tables.Rmd` to compile the supplementary materials and the script `09-Figure_key.Rmd` to specifically reproduce the cheat sheet figure.

## Additional scripts

Note that this repository contains also additional scripts for additional analysis that are not discussed in the paper or in its supplementary materials and are available but not detailed and tested here.
These are:

 * `06-Figure_with_correlations`: looking at the correlation between innovation and elaboration;
 * `10-novelty_predictions`: looking at the predictability of innovation and elaboration;
 * `11-sensitivity_analysis`: looking at the effects of the data on our results;
 * `12-variance_partitioning`: looking at different interpretations of variance partitioning in high dimensional nested data.

## Citing this work

To cite the paper, please use:
 * Thomas Guillerme et al. Innovation and elaboration on the avian tree of life.Sci. Adv.9,eadg1641(2023). [DOI:10.1126/sciadv.adg1641](https://www.science.org/doi/10.1126/sciadv.adg1641)

To cite this repository, please use:
 * Thomas Guillerme, & Natalie Cooper. (2022). TGuillerme/elaboration_exploration_bird_beaks: v0.1-preprint (v0.1). Zenodo. [https://doi.org/10.5281/zenodo.6984934](https://doi.org/10.5281/zenodo.6984934)

To cite the mcmcmcglmmm method, please use **both**:
 * Thomas Guillerme et al. Innovation and elaboration on the avian tree of life.Sci. Adv.9,eadg1641(2023). [DOI:10.1126/sciadv.adg1641](https://www.science.org/doi/10.1126/sciadv.adg1641)
 * Thomas Guillerme, & Natalie Cooper. (2022). TGuillerme/mcmcmcglmmm: v0.9 (v0.9). Zenodo. [https://doi.org/10.5281/zenodo.6974582](https://doi.org/10.5281/zenodo.6974582)

To cite the elaboration and innovation method, please use **both**:
* Thomas Guillerme et al. Innovation and elaboration on the avian tree of life.Sci. Adv.9,eadg1641(2023). [DOI:10.1126/sciadv.adg1641](https://www.science.org/doi/10.1126/sciadv.adg1641)
* Guillerme, T., 2018. dispRity: a modular R package for measuring disparity. Methods in Ecology and Evolution, 9(7), pp.1755-1763. [https://doi.org/10.1111/2041-210X.13022](https://doi.org/10.1111/2041-210X.13022)

