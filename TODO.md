# TODO list for completion

## TODOs comment from gdocs

 - [x] edits on fig_ortho (remove passersiformes and spaces)
 - [x] edits on fig_ortho change transparency to dashes
 - [x] edits on fig_ortho add the 0.75 line
 - [x] edit par5 with link to par4
 - [x] table with the boxplot values results to add to supplementary
 - [x] make passseriformes all results in supplementary
 - [x] add PC1 loading for phylo group

## Analyses

 - [ ] Passeriformes bit: just rerun the whole thing on passerines with 50 levels (2 sub-orders and 48 patch clades (~ families?)) [TG]
 - [ ] Sensitivity for clade effect: make a null to see if the main results (the nested structure) and orthogonality is present with no phylo signal. For tha just simulate a Brownian motion trait and a pure birth tree, cut two clades (first split) and run the beer pipeline on that? [TG]: actually do the simulations on the charadriiformes tree with two simulations: one is randomly assigning the beak shapes to each bird and second is generating brownian motion beask [for the pankackness, transform the pca to be spherical and rerun the analysis on charadriiformes to see if that changes anything.
 - [x] fast piggles
 - [x] check something about random skewers method (basically how many ellipses are expeccted to be the same geometrically) + measure the angle variance for each ellipse
 - [x] make sure what we see is what we see.
 - [x] put all the ellipses together in a figure and compare them scaled aligned. Then compare the position of the ellipses for each cherries in terms of angle, alignment and distance.
 - [x] Compare passeriformes ellipses from the whole space model (level 2) to the the ones from the passeriformes space model (level 0) to see the difference there.
 - [ ] measure the variance explained in the model for the random terms and the residuals

Standardise the vocabulary
Describe the model more clearly
Explain the parent to parent parent's details more clearly
Highlight that ellipses are not areas but posterior VCVs
Highlight that in figure 1 we're mainly looking at the orientation of ellipses, no shape and size, (the size is scaled and the shape is just a producct of 8 dimensionality)
Maybe change "line of least resistance" to something else. 
- Maybe check the phylo signal for the ellipses (orthogonality) with the non scaled non-absolute elaboration values

 - [ ] check variance partitioning with just 1D and then check the variance partition. Try running a 100 iterations on brms with the super-order model


## Manuscript

 - [x] clean the google doc [TG]
 - [x] add figures [TG]
 - [x] go through all the manuscript by the end of Friday [GT]
 - [x] Check the methods section [NC AB]
 - [x] Decide on the main narrative (the beer brocolli?) [to be figured out while working on the draft]
 - [x] Lines of least resistance becomes major axis of phenotypic VCV
 - [x] Finalise the manuscript writing (multiple passes, set an order and deadline for the multiple passes?)
 - [x] Pepper with references (using google docs or LaTeX?) [TG to do later]
 - [x] do a completion checkkist: abstract + acknowledgments + funding grant codes 
 - [ ] ESEB comment: Make clearer in the methods section about the nested phylo terms
 - [x] make a glossary 
 - [ ] rewrite the narrative start
 - [ ] cite (Stebbins 1974, Futuyma et al 1993, Schluter 1996) for LOLR (https://www.biorxiv.org/content/10.1101/2022.09.13.507810v1.full) (in general first intro paragraph)


## Figures

 - [x] Finalise figure anus: what info do we want in it? [GT]
 - [x] Finalise correlation figure: alignments and stuff [TG]
 - [x] Finalise ellipses figure: alignnments and stuff [TG]
 - [x] Check figure 0 (helper) [TG]
 - [x] Write a detailed explanation for that figure (cheat sheet)
 - [x] Figure 4: change "group" to "clade"
 - [x] Figure 4: remove the bottom row (and just write down the test results in the figure)
 - [ ] Update figure orthogonality (missing a couple of orders)
 - [ ] ggplotise figure 4 (plus integrate comments from overleaf):
          natalie.cooper: this figure needs the headers removing and slightly large axes labels. Looks a bit scrappy compared to the rest of the figures at the moment
          natalie.cooper: Also get rid of the bhattacharya bit on the plots, just put into the legend
          gavin.thomas: Yeah, I agree. I hate to say it but it looks like it needs a ggplot overhaul, or at least filling the areas under the curves along with the smartening up that Natalie suggests.
 - [ ] Figure anus: change "novelty" to elaboration/innovation in the right boxplot and remove it from the left one.

## Supplementaries

 - [x] supp tables from the main text
 - [x] supp math from the main text
 - [x] supp passerines results
 - [x] supp big ellipse table containing n, sd, orthogonality, elaboration/innovation.
 - [x] Passerines?
 - [x] methods vignettes (but see reproducibility)?
 - [x] start adding them at the bottom of the google doc [TG]
      - [x] missing the math thingy
      - [x] missing the projection/rejection illustration

## README and reproducibility

 - [x] check list is already in the README (needing to check english/compiling/links for all of them)
 - [x] deciding of a release date for the packages?
 - [x] make sure the data is there and available (tidied and packaged!)
 - [x] Natalie to check the vignettes

## Poster

 - [x] Do poster for ESEB [TG]