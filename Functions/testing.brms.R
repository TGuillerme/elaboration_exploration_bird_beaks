## Loading packages
library(brms)
library(beer)
library(MCMCglmm)

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

# Saving parameters (speeding up?)
save_param <- save_pars(group = FALSE)


# Multivariate model (one clade)
model_clade_multi <- brm(
  mvbind(var, phen, cofactor) ~ clade_levels + (1|gr(clade, cov = vcv_full_tree, by = clade_levels)) + (1|gr(phylo, cov = vcv_full_tree)), 
  data = data_simple, 
  family = gaussian(), 
  data2 = list(vcv_full_tree = vcv_full_tree),
  chains = 1,
  iter = 1000,
  save_pars = save_param
)
post <- posterior_samples(model_clade_multi)




















## Loading packages
library(brms)
library(beer)
library(MCMCglmm)

## Loading the demo data
data(demo_data)
data(demo_trees)
data(demo_tree)

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
# Within threading parallelisation
save_param <- save_pars(group = FALSE)



brms_run <- system.time( #0.71 sec
  brms_model <- brm(
    mvbind(PC1, PC2, PC3) ~ clade + (1|gr(clade_sp, cov = vcv_full_tree, by = clade)) + (1|gr(animal, cov = vcv_full_tree)), 
    data = demo_data, 
    family = gaussian(), 
    data2 = list(vcv_full_tree = vcv_full_tree),
    chains = 1,
    warmup = 250,
    iter = 1000,
    save_pars = save_param
  )
)

brms_post <- posterior_samples(brms_model)
colnames(brms_post)
#  [1] "b_PC1_Intercept"                           
#  [2] "b_PC2_Intercept"                           
#  [3] "b_PC3_Intercept"                           
#  [4] "b_PC1_cladeplovers"                        
#  [5] "b_PC1_cladesandpipers"                     
#  [6] "b_PC2_cladeplovers"                        
#  [7] "b_PC2_cladesandpipers"                     
#  [8] "b_PC3_cladeplovers"                        
#  [9] "b_PC3_cladesandpipers"                     
# [10] "sd_animal__PC1_Intercept"                  
# [11] "sd_clade_sp__PC1_Intercept:cladegulls"     
# [12] "sd_clade_sp__PC1_Intercept:cladeplovers"   
# [13] "sd_clade_sp__PC1_Intercept:cladesandpipers"
# [14] "sd_animal__PC2_Intercept"                  
# [15] "sd_clade_sp__PC2_Intercept:cladegulls"     
# [16] "sd_clade_sp__PC2_Intercept:cladeplovers"   
# [17] "sd_clade_sp__PC2_Intercept:cladesandpipers"
# [18] "sd_animal__PC3_Intercept"                  
# [19] "sd_clade_sp__PC3_Intercept:cladegulls"     
# [20] "sd_clade_sp__PC3_Intercept:cladeplovers"   
# [21] "sd_clade_sp__PC3_Intercept:cladesandpipers"
# [22] "sigma_PC1"                                 
# [23] "sigma_PC2"                                 
# [24] "sigma_PC3"                                 
# [25] "rescor__PC1__PC2"                          
# [26] "rescor__PC1__PC3"                          
# [27] "rescor__PC2__PC3"                          
# [28] "lp__"     

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
mcmcglmm_time <- system.time(
  mcmcglmm_model <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1,
                       family = rep("gaussian", 3),
                       random =
                         ~ us(at.level(clade,1):trait):animal
                         + us(at.level(clade,2):trait):animal
                         + us(at.level(clade,3):trait):animal
                         + us(trait):animal,
                       rcov = ~ us(trait):units,
                       prior = priorRG5, pedigree = demo_tree, data = demo_data, nitt = 1000, burnin = 0, thin = 1)
  )
mcmc_post <- cbind(mcmcglmm_model$VCV), mcmcglmm_model$Sol)

colnames(mcmcglmm_model$Sol)

## Clade levels
#[,c(1:3)]   #:Clade1:PC1,2,3 vs. Clade1:PC1
#[,c(4:6)]   #:Clade1:PC1,2,3 vs. Clade1:PC2
#[,c(7:9)]   #:Clade1:PC1,2,3 vs. Clade1:PC3
#[,c(10:12)] #:Clade2:PC1,2,3 vs. Clade2:PC1
#[,c(13:15)] #:Clade2:PC1,2,3 vs. Clade2:PC2
#[,c(16:18)] #:Clade2:PC1,2,3 vs. Clade2:PC3
#[,c(19:21)] #:Clade3:PC1,2,3 vs. Clade3:PC1
#[,c(22:24)] #:Clade3:PC1,2,3 vs. Clade3:PC2
#[,c(25:27)] #:Clade3:PC1,2,3 vs. Clade3:PC3
## Phylo level
#[,c(28,36)] #:animal:PC1,2,3 vs. animal:PC1,2,3
#[,c(37,45)] #:unit:PC1,2,3 vs. unit:PC1,2,3
