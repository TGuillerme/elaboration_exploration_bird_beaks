# TODO list for coding

- [ ] wrap up the manual neatly (links with @seealso and @example)
- [ ] create some demo data for demonstrating the pipeline.

# Content description

The wrapper pipeline package must contain

 - [ ] some functions for preparing the mini chains `make.mini.chains`
 - [ ] some functions for running the mini chains `run.mini.chains`
 - [ ] some functions for analysing the mini chains results `diagnose.mini.chains`
 - [ ] some functions for combining the mini chains in a big `MCMCglmm` object `combine.mini.chains`
 - [x] some function to extract the posterior covar matrices from the `MCMCglmm` object (can be an object of class `"beer"`?): `get.covar`
 - [x] some function for extracting the major axis from the matrices: `get.axes`
 - [x] some function for running the blob-wise test: i.e. doing the aligning and then calculating the angle projection and rejection from the axis. `analyses.group`
 - [x] some function for running the tip-wise test: i.e. getting the projection and rejection for each taxa. `analyses.tip`
 - [x] some function for plotting the results of groups comparisons: `plot.analyses.group`
 - [ ] some function for plotting the tip-wise results in a circular phylogeny: `plot.tree` (wrapper from `phytools` simMap)
 - [x] some function for plotting the trait space (`plot.space`)
 - [x] some function for plotting the ellipses (modif `get.ellipses`): `plot.ellipses`
 - [x] some function for plotting the main axes (modif `get.axes`): `plot.axes`
