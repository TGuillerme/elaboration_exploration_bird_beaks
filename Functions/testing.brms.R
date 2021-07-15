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



## Phylo version
phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read.table(
  "https://paul-buerkner.github.io/data/data_simple.txt", 
  header = TRUE
)

clade <- data_simple$phylo
clade_levels <- data_simple$phylo %in% clade_a$tip.label + 1
data_simple <- cbind(data_simple, clade, clade_levels)
head(data_simple)



clade_a <- extract.clade(phylo, node = 202)
clade_b <- extract.clade(phylo, node = 327)

# random =
#  ~ us(at.level(clade,1):trait):animal
#  + us(at.level(clade,2):trait):animal
#  + us(at.level(clade,3):trait):animal
#  + us(trait):animal,

vcv_full_tree <- ape::vcv.phylo(phylo)
vcv_clade_a <- ape::vcv.phylo(clade_a)
vcv_clade_b <- ape::vcv.phylo(clade_b)

(1|gr(phylo, by = clade, cov = vcv_full_tree))

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

## Simple model (three clades)
model_clades <- brm(
  phen ~ cofactor + (1|gr(phylo, cov = vcv_full_tree)), + (2|gr(clade_a, cov = vcv_clade_a)) + (3|gr(clade_b, cov = vcv_clade_b)), 
  data = data_simple, 
  family = gaussian(), 
  data2 = list(vcv_full_tree = vcv_full_tree,
               vcv_clade_a   = vcv_clade_a,
               vcv_clade_b   = vcv_clade_b),
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
)