# Innovation and elaboration on the avian tree of life

Author(s): [Thomas Guillerme](https://github/TGuillerme), [Natalie Cooper](http://nhcooper123.github.io/), [Andrew P. Beckerman](https://andbeck.github.io/beckslab/), [Gavin H. Thomas](https://www.sheffield.ac.uk/biosciences/people/academic-staff/gavin-thomas)

This repository contains all the code and data used in [the manuscript]().

<!-- Repo zenodo: [![DOI](https://zenodo.org/badge/102496441.svg)](https://zenodo.org/badge/latestdoi/102496441) -->
<!-- Preprint link: [![DOI](https://zenodo.org/badge/102496441.svg)](https://zenodo.org/badge/latestdoi/102496441) -->
<!-- Paper link: [![DOI](https://zenodo.org/badge/102496441.svg)](https://zenodo.org/badge/latestdoi/102496441) -->

## Supplementary material

 * The supplementary tables and figures mentioned in the main text is available [here](link_to_supp).
 * The supplementary information about the mcmcmcglmmm method is available as a [reproducible vignette](https://raw.rawgit.net/TGuillerme/mcmcmcglmmm/main/inst/MCMCglmm_mini_chains.html).
 * The supplementary information about the elaboration and innovation analysis (or projection analysis) is available as a [reproducible vignette](https://raw.rawgit.net/TGuillerme/dispRity/master/inst/vignettes/Projection_analysis.html).

## Data
The raw data phylogenetic data is available from [here](https://birdtree.org/).
The raw morphological data is available from [here](beak_data).
You can access the processed data used in this analysis [here](processed_data_repo).
If you use either the processed or raw phylogenetic data in a publication, please cite the following papers [Cooney et al 2017](https://www.nature.com/articles/nature21074), [Chira et al 2020](https://royalsocietypublishing.org/doi/full/10.1098/rspb.2020.1585) and [Hughes et al 2022](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.13905).

## Analysis
This repository contains the scripts listed below to reproduce the results and figures elements in THE_PAPER.
However, note that due to the large amount of data and computational time and resources involved (2.1 CPU years, 4TB RAM), we provide smaller reproducible examples below (see [Reproducible section below](#reproducible)).

 * **01-Data_preparation** contains the script to transform the raw data mentioned above into the processed data used in this analysis.
 * **02-mcmcmcglmmm_analysis** contains the script to 1) generate the mini-chains for the mcmcmcglmmm analysis and 2) combine the mini-chains into the posterior results used in the analysis. Note that this script is computationally heavy. For a more manageable example, you can refer to the [reproducible vignette](https://raw.rawgit.net/TGuillerme/mcmcmcglmmm/main/inst/MCMCglmm_mini_chains.html).
 * **03-Elaboration_innovation_analysis** contains the script to measure the elaboration and innovation. Note that this script is moderately computationally heavy. For a more manageable example, you can refer to the [reproducible vignette](https://raw.rawgit.net/TGuillerme/dispRity/master/inst/vignettes/Projection_analysis.html).
 * **04-Figure_with_ellipse_scalling_and_8D_histograms** contains the script to reproduce the elements in figure 1 in the main text.
 * **05-Figure_with_phylogeny** contains the script to reproduce the elements in figure 3 in the main text.
 <!-- * **06-Figure_with_correlations**  contains the script to reproduce the elements in figure 2 in the main text. -->
 * **07-Supplementary_figures_tables** contains the script to reproduce the supplementary tables and figures].


## Reproducible examples {#reproducible}
 * To reproduce the mcmcmcglmmm method, please refer to the [detailed vignette in the `mcmcmcglmmm` package](https://github.com/TGuillerme/mcmcmcglmmm/blob/main/inst/MCMCglmm_mini_chains.Rmd).
 * To reproduce the elaboration and innovation analysis, please refer to the [detailed vignette in the `dispRity` package](https://github.com/TGuillerme/dispRity/blob/MCMCglmm/inst/vignettes/Projection_analysis.Rmd).


## Citing this work

To cite the paper, please use:
<!-- Paper cite (link to cite formats) -->

To cite this repository, please use:
<!-- Repo Zenodo cite (link to cite formats) -->

To cite the mcmcmcglmmm method, please use **both**:
<!-- Paper cite (link to cite formats) -->
<!-- mcmcmcglmmm Zenodo cite (link to cite formats) -->

To cite the elaboration and innovation method, please use **both**:
<!-- Paper cite (link to cite formats) -->
<!-- dispRity paper (link to cite formats) -->

