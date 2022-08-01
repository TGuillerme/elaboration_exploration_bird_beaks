library(dispRity)
source("../Functions/covar.utilities.R")
source("../Functions/ploting.utilities.R")

## Load the data
load(file = "../Data/Processed/shapespace_allbirds_lvl_superorder_order.rda")
shapespace <- shapespace_allbirds_lvl_superorder_order[[1]]$space
tree <- shapespace_allbirds_lvl_superorder_order[[1]]$consensus_tree
rm(shapespace_allbirds_lvl_superorder_order)
load(file = "../Data/Processed/ploting_data.rda")
load(file = "../Data/Processed/tip_colours_super_orders.rda")
load(file = "../Data/Processed/tip_colours_orders.rda")


## Ploting the trait space
pdf(file = "shapespace_plot.pdf", height = 8, width = 8)
plot(NULL, xlim = range(c(shapespace[, c(1,2)])), ylim = range(c(shapespace[, c(1,2)])),
	xlab = paste0("PC1 (", paste(round(var(shapespace[, 1])/sum(var(shapespace[,c(1:8)]))*100, 2), "%)")),
	ylab = paste0("PC2 (", paste(round(var(shapespace[, 2])/sum(var(shapespace[,c(1:8)]))*100, 2), "%)")))
## Sort levels by size
plot_order <- sort(table(shapespace[, "level1"]), decreasing = TRUE)
for(i in 1:length(plot_order)) {
	select_col <- "grey"
	if(names(plot_order)[i] %in% names(tip_colours_super_orders)) {
		select_col <- tip_colours_super_orders[names(plot_order[i])]
	}
	points(shapespace[which(shapespace[, "level1"] == names(plot_order[i])), c(1,2)], col = select_col, pch = 19, cex = 0.5)
}
dev.off()


## Ploting the tree
pdf(file = "tree_orders_col.pdf", height = 40, width = 26)

tree <- ladderize(ploting_data$consensus_tree, right = FALSE)

## Add the elaboration/exploration scores
elaborations <- ploting_data$median_elaborations[match(tree$tip.label, names(ploting_data$median_elaborations))]
innovations <- ploting_data$median_innovations[match(tree$tip.label, names(ploting_data$median_innovations))]

## Transform them into a colour gradient
col.grad <- colorRamp(rev(c("#d7191c", "#fdae61", "#ffffbf", "#abdda4", "#2b83ba")))
cols_elab <- col.grad(elaborations/max(elaborations))
cols_innov <- col.grad(innovations/max(elaborations))

## Match the colours correctly to match the ellipse figure
#TODO: fix this!
level_colors <- tip_colours_orders
level_vector <- shapespace[, "level2"]
names(level_vector) <- rownames(shapespace)
edge_colors <- as.factor(match.tip.edge(level_vector, tree, replace.na = 0))
levels(edge_colors) <- c("grey","grey", level_colors[match(levels(level_vector), names(level_colors))[-1]])
plot(tree, show.tip.label = FALSE, edge.color = as.character(edge_colors), edge.width = 3, edge.lty = 1)
tiplabels("", frame = "none", pch = 15, col = rgb(cols_elab, maxColorValue = 255), offset = 2)
tiplabels("", frame = "none", pch = 15, col = rgb(cols_innov, maxColorValue = 255), offset = 4)
dev.off()


## Tiplabels is clade name
clade_names <- rep("grey", Ntip(tree))
names(clade_names) <- tree$tip.label
load("../Data/Processed/clade_info.rda")
load("../Data/Processed/clade_info_passeriformes.rda")

for(i in 1:27) {
    clade_names[clade_info$orders[[i]]$species] <- clade_info$orders[[i]]$col
}
for(i in 1:23) {
    clade_names[clade_info_passeriformes$families[[i]]$species] <- clade_info_passeriformes$families[[i]]$col
}

length(unique(clade_names))

plot(tree, show.tip.label = FALSE, edge.color = as.character(edge_colors), edge.width = 3, edge.lty = 1)
tiplabels("", frame = "none", pch = 15, col = clade_names, offset = 2)


# 164.760
# 210.800

# 20
# 150

data(charadriiformes)

## Creating a dispRity object from the charadriiformes model
covar <- MCMCglmm.subsets(data       = charadriiformes$data,
                          posteriors = charadriiformes$posteriors,
                          group      = MCMCglmm.levels(
                                         charadriiformes$posteriors)[1:4],
                          rename.groups = c("gulls", "plovers",
                                            "sandpipers", "phylogeny"))

pdf(file = "covars.pdf", height = 8, width = 8)
## Same plot with more options
covar.plot(covar, n = 100, ellipses = TRUE, major.axes = TRUE, 
           col = c("orange", "blue", "darkgreen", "grey", "grey"),
           legend = TRUE, points = TRUE, points.cex = 0.5,
           main = "Charadriiformes shapespace")
dev.off()





## Correlation plots
library(dispRity)
source("../Functions/ploting.utilities.R")
source("../Functions/correlations.utilities.R")
## Loading the projection/rejections results
load(file = "../Data/Processed/shapespace_allbirds_lvl_superorder_order_results_list.rda")
results <- shapespace_allbirds_lvl_superorder_order_results_list
rm(shapespace_allbirds_lvl_superorder_order_results_list)
## Calculate the correlations for the projections onto the phylo axis
super_phylo_cor <- get.correlations(results$super_results_phylo)
## Calculate the correlations for the projections onto the group' axes
super_group_cor <- get.correlations(results$super_results_within, exclude.phylo = TRUE)


load("../Data/Processed/tip_colours_orders.rda")

## Set the pdf parameters
pdf("correlations.pdf", height = 12, width = 8)

## Set up the plotting layout
layout_matrix <- cbind(matrix(1,2,2), matrix(0,2,2))
layout_matrix <- rbind(rbind(layout_matrix, matrix(1:8+1, 2, 4, byrow = TRUE)), matrix(9:16+1, 2, 4,  byrow = TRUE))
plot_layout <- layout(layout_matrix)
# layout.show(plot_layout)

## Setting the colour pallette for the subsequent plots
colour.palette <- gg.color.hue

## --------
## PANEL A
## --------

## Plotting the densities (with the quadrants) in the first panel
par(mar = c(5, 4, 4, 2) + 0.1)
plot.densities(super_phylo_cor$phylogeny, with.quadrant = TRUE)

## --------
## PANEL B
## --------

## Getting the subsets for the colouring
col_subs <- c(split(shapespace$level2, f = shapespace$level1)[-1], "phylogeny" = list(shapespace$level1))

## Set the vector of legend positions
legends_pos <- c("topleft", "topright", "topright", "topright",
                 "topleft", "topright", "topleft", "topright")
## Create the vector for counting the legend numbers
legend_counter <- 1

## Plotting each individual correlation plots
for(i in 1:8) {
    par(mar = c(2,2.5,2,1.5))
    plot.correlations(super_phylo_cor[[i]], col.sub = col_subs[[i]], legend.pos = legends_pos[i], ID = i, pt.cex = 0.5, legend.cex = 2/3, legend_counter = legend_counter, contour = NULL, with.quadrant = TRUE, colour.palette = tip_colours_orders)
}

## --------
## PANEL C
## --------

## Getting the subsets for the colouring
col_subs <- c(split(shapespace$level2, f = shapespace$level1)[-1], "phylogeny" = list(shapespace$level1))

## Set the vector of legend positions
legends_pos <- c("bottomright", "topright", "topleft", "topleft",
                 "topleft", "topleft", "topright", "bottomright")
## Resetting the legend_counter
legend_counter <- 1

## Plotting each individual correlation plots
for(i in 1:8) {
    par(mar = c(2,2.5,2,1.5))
    plot.correlations(super_group_cor[[i]], col.sub = col_subs[[i]], legend.pos = legends_pos[i], ID = i, pt.cex = 0.5, legend.cex = 2/3, legend_counter = legend_counter, with.quadrant = FALSE, contour = NULL)
}
dev.off()


