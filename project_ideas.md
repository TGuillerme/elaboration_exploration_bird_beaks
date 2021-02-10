# Bird beak sciency paper project

# Easy(ish) questions
 
 1. Are evolutionary rates (molecular) related to big changes in beak morphology or not? If not is it to something else (e.g. can you evolve a different beak shape without big evo changes if you have a founder effect or something like that).
 Mapping changes in rates (BAMM?) to changes in morphology using some disparity measure?

 2. What is driving changes in beak change (i.e. flamingo effect) across multiple evolutionary scales (i.e. clade levels)? Measure beak shape as a deviation from "normal" evolutionary trajectory and check if big changes can be somehow predictable (islands, climate change, etc)?


# Project 1: morpho vs. molecular rates

1. measure beak shape distance from one focal species to its closest clade (divided by branch length in time  and in molecular changes)
2. check the distribution of these distances. Species that elaborate have small and a species that explore have big?
3. assumption is that this distance grows function of time regularly but that it jumps if founding event

Tony Gosman did things kind of along those lines. 

# Project 2: beak deviation

## Measuring beak shape:
    
### Trait space:

We can use the bird beak data (easy) and build a trait space either using:

 * the classic route: procrustes + pca on xyz coordinates = shape.
    * Advantages: already done + easy;
    * Inconvenient: hard to interpret;
 * the fancy route: procrustes + pca on vector coordinates = shape [see here](https://raw.githack.com/TGuillerme/landvR/master/inst/vignettes/Landmark_variation_differences.html)
    * Advantages: new and easier to interpret;
    * Inconvenient: might require some theoretical defence (but we can do both)
-> Test it on a smaller dataset to see if it changes things.


### Trait space through time:

Once we have our trait space, we can do an ancestral character estimations of the PC scores for each OTU.
This gives us access to beak shape through time (nice!) but ace is just a glorified mean.
However we can "deglorify" that by using a "fuzzy trait-space" approach where node values are not point estimates but discretised distributions (e.g. 50 or 100 point estimates).

### Trait deviation:

Once we have our trait space through time, we can define axis of "evolutionary trajectory" or probably more politically correct to call it "elaboration trajectory" this can be calculated in several different ways for an OTU in a particular clade:

 a. OTU's evolutionary axis: the axis between the root and the OTU's descendant
 b. OTU's clade evolutionary axis: the axis between the root and the OTU's clade centroid
 c. OTU's clade axis: the axis between the clades centroid and the OTU's descendant
 d. Clade's axis: the best fitted line within the clade of interest
 e. Tree's axis: the best fitted line accross the whole data (PC1)
 
We can then measure the position of the OTU in relation to this axis in two different terms:
 
 1. It's **projected position** on the elaboration trajectory: big value means far away elaboration, small values means close elaborations; negative values means elaboration on the opposite trajectory of the axis (backwards?) and positive values means elaboration in the same trajectory of the axis.
 2. It's **projected distance** on the elaboration trajectory: big values means big innovations and small values means small innovations.
 3. It's **angle** from the elaboration trajectory: 90 means completely orthogonal and 0 means no angle.

We then end up having twice as many scores as we have OTUs (each OTU gets a projected position and distance value).
We can then:

 1. Compare the values among each other per individuals or per clades (distributions): for example, flamingoes have bigger projected distances than passerines.
 2. Compare the values to a null model (per individuals or per clades) to get which individuals or which clades are abnormally projected.
 3. Compare the values to a response variable (e.g. islandness) and PCMing the shit out of it.

> Implementation in `dispRity` get a `projection` metric that measures either "distance" or "position". Then get a `evo.proj` metric that measures either "distance" or "position" but using a tree and allow options a, b, c, d and e above.


## How this fits with Andrew's paper (Ecol. Letters)

The approach is slightly similar but less fancy (no MCMC) and here we focus on individuals rather than matrices.

