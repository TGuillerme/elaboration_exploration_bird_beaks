## Loading packages
library(brms)

## Loading the demo data
data(demo_data)
data(demo_trees)
data(demo_tree)

######
# Basic phylo demo from brms
# https://paul-buerkner.github.io/brms/articles/brms_phylogenetics.html
######

## Loading demo data
phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read.table(
  "https://paul-buerkner.github.io/data/data_simple.txt", 
  header = TRUE
)

## Creating some clade to test the clade effect
clade_a <- extract.clade(phylo, node = 327)

## Get the vcv matrix for the tree
vcv_full_tree <- ape::vcv.phylo(phylo)

## Creating a clade column (species names)
clade <- data_simple$phylo
## Creating a clade_levels column (whether species are in clade 1 or 2)
clade_levels <- data_simple$phylo %in% clade_a$tip.label + 1
## Creating a random third variable for fun
third_variable <- runif(nrow(data_simple))
## Combining all that together
data_simple <- cbind(var = third_variable, data_simple, clade, clade_levels)
## Pouf!
head(data_simple)

## Simple model from the example: univariate + phylogenetic random effect
model_simple <- brm(
  phen ~ cofactor + (1|gr(phylo, cov = vcv_full_tree)), 
  data = data_simple, 
  family = gaussian(), 
  data2 = list(vcv_full_tree = vcv_full_tree),
  chains = 1,
  iter = 1000
)

#######
## Same model but with an additional clade random effect
## univariate + phylogenetic random effect + clade level phylogenetic random effect
#######
# The idea here is to just add a clade effect using the clade column (species names) with the tree vcv structure and sorting them by clade.
#
# Does this measures a separate random phylogenetic effect per clade??
#
######
model_clade <- brm(
  phen ~ cofactor + (1|gr(clade, cov = vcv_full_tree, by = clade_levels)) + (1|gr(phylo, cov = vcv_full_tree)), 
  data = data_simple, 
  family = gaussian(), 
  data2 = list(vcv_full_tree = vcv_full_tree),
  chains = 1,
  iter = 1000
)


#######
## Same model but with a multivariate repsonse
## multivariate + phylogenetic random effect + clade level phylogenetic random effect
#######
# The idea here is to make the response variable multivariate following this tutorial
# https://paul-buerkner.github.io/brms/articles/brms_multivariate.html
#
# Does this measures a separate random phylogenetic effect per clade on the three data variables (var, phen and cofactor)?
#
######

# Multivariate model (one clade)
model_clade_multi <- brm(
  mvbind(var, phen, cofactor) ~ clade + (1|gr(clade, cov = vcv_full_tree, by = clade_levels)) + (1|gr(phylo, cov = vcv_full_tree)), 
  data = data_simple, 
  family = gaussian(), 
  data2 = list(vcv_full_tree = vcv_full_tree),
  chains = 1,
  iter = 1000
)


## You can install "beer" manually from the attached zip file using the following
#install.packages("beer_0.0.3.tar.gz", repos = NULL)
library(beer) 

## Preparing the demo data
vcv_full_tree <- ape::vcv.phylo(demo_tree)
demo_data <- cbind(demo_data, "clade_sp" = demo_data$animal)

#######
## The multivariate model with the demo data
## multivariate + phylogenetic random effect + clade level phylogenetic random effect
#######
# This is the same as "model_clade_multi" but applied on my own demo data
#
######
brms_model <- brm(
  mvbind(PC1, PC2, PC3) ~ clade + (1|gr(clade_sp, cov = vcv_full_tree, by = clade)) + (1|gr(animal, cov = vcv_full_tree)), 
  data = demo_data, 
  family = gaussian(), 
  data2 = list(vcv_full_tree = vcv_full_tree),
  chains = 1,
  iter = 1000
)

#######
## Comparing this to MCMCglmm
#######
library(MCMCglmm)

## MCMCglmm priors
priorRG5 <- list(R=list(
      R1 = list(V = diag(3), nu = 0.002)), 
    G=list(
      G1 = list(V = diag(3), nu = 0.002),
      G2 = list(V = diag(3), nu = 0.002),
      G3 = list(V = diag(3), nu = 0.002),
      G4 = list(V = diag(3), nu = 0.002)))

## The MCMCglmm model
mcmcglmm_model <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1,
                     family = rep("gaussian", 3),
                     random =
                       ~ us(at.level(clade,1):trait):animal
                       + us(at.level(clade,2):trait):animal
                       + us(at.level(clade,3):trait):animal
                       + us(trait):animal,
                     rcov = ~ us(trait):units,
                     prior = priorRG5, pedigree = demo_tree, data = demo_data, nitt = 1000, burnin = 0, thin = 1)