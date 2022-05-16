# Agenda:

 - Review the protocol:
     - Data
     - mcmcmcglmmm. TODO: discuss convergence
     - projections
 
 - Show some results

 - Discuss a question forward
     - Distribution of exploration differential: for any cherry, does one species explore more than the other?
     - Distribution of negative elaboration: which species elaborate in the other way?
     - Distribution of exploration/elaboration: is it random or clustered?
     - From Natalie's public engagement: "idea of close relatives that don’t look very similar" (which species elaborate/explore more within their own groups?)
     - "Special among flamigoes": which species are "truly" special (explore/elaborate in a nested way) and which ones are "truly" not?
     - Which birds fall out of the 95% for rejection and projections? And within these which ones are overlapping (95% out of the rejection + 95% out of the projections): the truly special ones.

TODO technicals (Thomas):
- check the prep.data function to see if it's sorting things correctly!
    - Done. It was fucked up.
- do 2D slice representations of all ellipses/axes
    - Done. (but not displayed)
- plot the difference to the phylo + the differences within per group

- elaboration scores: just show the absolute values
    - Done.
- color all the scatter plots by the next level down ()
    - Done.
- Do methods in Thomas english

- Check if the coloured ellipses are corresponding to the right level (clade)
    - Done.
- Calculate the average ellipse/axis on the average VCV rather than on all the ellipses/axis
    - Done.
- Find the highest elaborator/innovator overall (for the species vs. group, do an absolute and a scaled version)
    - Done.
- Re-do the box plots: order them by scores and remove the angle bit
    - Done
- Check the blind spots in the ela/ino corelation plots (add a species in a weird place of the morphospace)
    - Add species with elaboration around 0.8 and exploration around 0
    - Add species with elaboration around 0 and exploration around 1.0

- On the correlation plots add a phylo panel with the clades coloured
    - Done.

- Do the bird beak thing from the other way around (i.e. from the correlation plot to the PCA: where do things fall in the PCA when plotted in a weird corrner)
- Vary length and orientation of Phylo axis for the tissue pulling thing

- For ranking, try using just one of the two variables AND try to rank by the difference (elaboration - innovation)

- For finding Simone's Sausage, do the flattening the PCA from 2, 3, 4, .. 8 D and then the correlation plot and everytime measure the distance between the minimum innovation and the centre (the idea being that that distance increases non-linearly with the number of dimensions)

Question ideas:
