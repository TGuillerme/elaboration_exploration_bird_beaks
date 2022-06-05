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
pdf(file = "tree_orders.pdf", height = 40, width = 26)

tree <- ladderize(ploting_data$consensus_tree, right = FALSE)

## Add the elaboration/exploration scores
elaborations <- ploting_data$median_elaborations[match(tree$tip.label, names(ploting_data$median_elaborations))]
innovations <- ploting_data$median_innovations[match(tree$tip.label, names(ploting_data$median_innovations))]

## Transform them into a colour gradient
col.grad <- colorRamp(c("blue","orange"))
cols_elab <- col.grad(elaborations/max(elaborations))
cols_innov <- col.grad(innovations/max(innovations))

## Match the colours correctly to match the ellipse figure
#TODO: fix this!
level_colors <- tip_colours_orders
level_vector <- shapespace[, "level2"]
names(level_vector) <- rownames(shapespace)
edge_colors <- as.factor(match.tip.edge(level_vector, tree, replace.na = 0))
levels(edge_colors) <- c("grey","grey", level_colors[match(levels(level_vector), names(level_colors))[-1]])
plot(tree, show.tip.label = FALSE, edge.color = as.character(edge_colors), edge.width = 3, edge.lty = 1)
tiplabels("", frame = "none", pch = 15, col = rgb(cols_elab, maxColorValue = 256), offset = 2)
tiplabels("", frame = "none", pch = 15, col = rgb(cols_innov, maxColorValue = 256), offset = 4)
dev.off()

164.760
210.800

20
150