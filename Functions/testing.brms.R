set.seed(42)
## Loading beer
library(beer)
library(MCMCglmm)

## Loading the demo data
data(demo_data)
data(demo_trees)
data(demo_tree)

## Setting the consensus tree
consensus_tree <- demo_tree

## Params
data         = demo_data
dimensions   = c(1:4)
tree         = consensus_tree
trait.family = "gaussian"
randoms      = c("global", "clade")
residuals    = "global"
priors       = 0.02
verbose      = TRUE
parameters   = list(nitt   = 10000, thin   = 100, burnin = 0)


### bmrs DEMO


library(brms)
## Phylo version
phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read.table(
  "https://paul-buerkner.github.io/data/data_simple.txt", 
  header = TRUE
)

## Set up the clades
clade_a <- extract.clade(phylo, node = 327)

## Get the vcv matrix
vcv_full_tree <- ape::vcv.phylo(phylo)

## Update the data
clade <- data_simple$phylo
clade_levels <- data_simple$phylo %in% clade_a$tip.label + 1
third_variable <- runif(nrow(data_simple$phylo))
data_simple <- cbind(var = third_variable, data_simple, clade, clade_levels)
head(data_simple)

## Simple model (one clade)
model_simple <- brm(
  phen ~ cofactor + (1|gr(clade, cov = vcv_full_tree, by = clade_levels)) + (1|gr(phylo, cov = vcv_full_tree)), 
  data = data_simple, 
  family = gaussian(), 
  data2 = list(vcv_full_tree = vcv_full_tree),
  chains = 2,
  iter = 1000,
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
)

## Multivariate model (one clade)
# model_simple <- brm(
#   mvbind(var, phen, cofactor) ~ clade-1 + (1|gr(clade, cov = vcv_full_tree, by = clade_levels)) + (1|gr(phylo, cov = vcv_full_tree)), 
#   data = data_simple, 
#   family = gaussian(), 
#   data2 = list(vcv_full_tree = vcv_full_tree),
#   chains = 2,
#   iter = 1000
# )


### MODEL COMPARE


priorRG5 <- list(R=list(
      R1 = list(V = diag(3), nu = 0.002)), 
    G=list(
      G1 = list(V = diag(3), nu = 0.002),
      G2 = list(V = diag(3), nu = 0.002),
      G3 = list(V = diag(3), nu = 0.002),
      G4 = list(V = diag(3), nu = 0.002)))
vcv_full_tree <- ape::vcv.phylo(demo_tree)

demo_data <- cbind(demo_data, "clade_sp" = demo_data$animal)


brms_run <- system.time(
  brms_model <- brm(
    mvbind(PC1, PC2, PC3) ~ clade-1 + (1|gr(clade_sp, cov = vcv_full_tree, by = clade)) + (1|gr(animal, cov = vcv_full_tree)), 
    data = demo_data, 
    family = gaussian(), 
    data2 = list(vcv_full_tree = vcv_full_tree),
    chains = 1,
    iter = 10000,
    thin = 100
  )
)


mcmcglmm_run <- system.time(
  mcmcglmm_model <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1, family = rep("gaussian", ntraits),
                       random =
                         ~ us(at.level(clade,1):trait):animal
                         + us(at.level(clade,2):trait):animal
                         + us(at.level(clade,3):trait):animal
                         + us(trait):animal,
                      rcov=
                         ~ us(trait):units,
                       prior= priorRG5, pedigree = demo_tree, data = demo_data, nitt = 10000, burnin = 5000, thin = 100)
)


