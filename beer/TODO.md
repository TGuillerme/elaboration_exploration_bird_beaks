# TODO list for coding

- [ ] wrap up the manual neatly (links with @seealso and @example)
- [ ] create some demo data for demonstrating the pipeline.

# Content description

The wrapper pipeline package must contain

 - [ ] some functions for preparing the mini chains `make.mini.chains`
 - [ ] some functions for running the mini chains `run.mini.chains`
 - [ ] some functions for analysing the mini chains results `diagnose.mini.chains`
 - [ ] some functions for combining the mini chains in a big `MCMCglmm` object `combine.mini.chains`
 - [ ] some function to extract the posterior covar matrices from the `MCMCglmm` object (can be an object of class `"beer"`?): `get.covar`
 - [ ] some function for extracting the major axis from the matrices: `get.axes`
 - [ ] some function for running the blob-wise test: i.e. doing the aligning and then calculating the angle projection and rejection from the axis. `group.results`
 - [ ] some function for running the tip-wise test: i.e. getting the projection and rejection for each taxa. `tip.results`
 - [ ] some function for plotting the results of groups comparisons: `plot.groups`
 - [ ] some function for plotting the tip-wise results in a circular phylogeny: `plot.tree` (wrapper from `phytools` simMap)
 - [ ] some function for plotting the trait space (wrapper from `dispRity::plot.preview`)
 - [ ] some function for plotting the ellipses (modif `get.ellipses`): `plot.ellipses`
 - [ ] some function for plotting the main axes (modif `get.axes`): `plot.axes`
