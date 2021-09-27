#@param data a dispRity object with covar (see details)
#@param tree the tree
#@param col.tips a vector of colours for the tips
#@param vals.gradient optional, a vector of colours for creating the values gradient
#@param vals.cent.tend the central tendency for colouring the values (default is median)
#@param tree.args optional arguments to be passed to ggtree::ggtree
#@param data.plot.fun the function to plot the data (by default ggplot2::geom_boxplot)
#@param data.args optional arguments to be passed to the data plotting function

#@details Note that the tips are attributed to the data first through the tree (i.e. selecting only tips present in the tree), and then through the groups by input order (i.e. if data for one tip is overlapping between group1 and group2, the data from group2 will override the one from group1).




#install.packages("BiocManager")
#BiocManager::install("ggtreeExtra")
#BiocManager::install("phyloseq")

library(ggplot2)
library(ggtreeExtra)
library(ggtree)
library(phyloseq)
library(dplyr)

tree <- demo_tree
data <- results_phylo$position

circular.plot <- function(data, tree, col.tips, vals.gradient, vals.cent.tend = median, tree.args = list(layout = "fan", open.angle = 10), data.args, data.plot.fun = ggplot2::geom_boxplot, ...) {

    do_col_tips <- do_cent_tend <- FALSE

    ## Sanitizing
    if(missing(tree)) {
        tree <- data$tree[[1]]
        ## Add a stop
    } 

    if(!missing(vals.gradient)) {
        ##TODO: Check sanitizing
        do_cent_tend <- TRUE
    }

    if(!missing(col.tips)) {
        ##TODO: Check sanitizing
        do_col_tips <- TRUE
    }

    ## Prepare the data 
    ## Get the list of tips
    list_of_tips <- tree$tip.label
    data_sorting <- matrix(NA, nrow = length(list_of_tips), ncol = data$call$bootstrap[[1]], dimnames = list(list_of_tips))
    n_groups <- n.subsets(data)
    if(do_col_tips) {
        col_tips <- rep(NA, nrow(data_sorting))
        names(col_tips) <- rownames(data_sorting)
    }

    ## Fill the matrix
    for(one_group in 1:n_groups) {
        ## Select the rows in the matrix (in the tree)
        selected_rows <- rownames(data_sorting) %in% rownames(data$disparity[[one_group]]$elements)
        ## Select the rows in the data (the disparity)
        selected_tips <- rownames(data$disparity[[one_group]]$elements) %in% rownames(data_sorting)[selected_rows]
        ## Fill the rows in common
        if(any(selected_rows) && any(selected_tips)) {
            data_sorting[selected_rows, ] <- data$disparity[[one_group]]$elements[selected_tips, ]
        }
        ## Add the colours for each tips (if required)
        if(do_col_tips) {
            col_tips[selected_rows] <- names(data$disparity)[one_group]
        }
    }

    ## Get the central tendencies
    if(do_cent_tend) {
        centrals <- apply(data_sorting, 1, vals.cent.tend)
    }

    ## Combine the finale table (tips + values + means + group)
    data_frame <- data.frame(tips = rep(rownames(data_sorting), each = data$call$bootstrap[[1]]),
                             values = c(t(data_sorting)))
    if(do_cent_tend) {
        data_frame$central <- rep(centrals, each = data$call$bootstrap[[1]])
    }
    if(do_col_tips) {
        data_frame$group <- rep(col_tips, each = data$call$bootstrap[[1]])
    }


    ## Plotting bits

    ## Get the tree arguments
    edges_args <- tree.args
    edges_args$tr <- tree
    tree_plot <- do.call(ggtree, edges_args)

    ## Adding tip points
    if(do_col_tips) {
        tips_plot <- geom_tippoint(col = col.tips[as.numeric(as.factor(col_tips))])
        tree_plot <- tree_plot + tips_plot
    }

    ## Add the box plots
    if(do_cent_tend) {
        tree_plot <- tree_plot + 
            geom_fruit(
                 data    = data_frame,
                 geom    = data.plot.fun,
                 mapping = aes(
                             y=tips,
                             x=values,
                             fill=central
                           ),
                 size=2,
                 axis.params=list(
                                 axis       = "x",
                                 text.size  = 1.8,
                                 hjust      = 1,
                                 vjust      = 0.5,
                                 nbreak     = 3,
                             ),
                 grid.params=list()
                ) + scale_fill_gradient(low  = vals.gradient[1],
                                        high = vals.gradient[2])
    } else {
        tree_plot <- tree_plot + 
            geom_fruit(
                 data    = data_frame,
                 geom    = data.plot.fun,
                 mapping = aes(
                             y=tips,
                             x=values
                           ),
                 size=2,
                 axis.params=list(
                                 axis       = "x",
                                 text.size  = 1.8,
                                 hjust      = 1,
                                 vjust      = 0.5,
                                 nbreak     = 3,
                             ),
                 grid.params=list()
                )        
    }
    return(tree_plot)


p3 <- fruit_plot(
           p = tree_plot, 
           data=data_frame, 
           geom=data.plot.fun,
           mapping=aes(y=tips, x=values, fill=central),
           size=2.5, 
           starstroke=0
      )


    test <- tree_plot + 
            geom_fruit(
                 data    = data_frame,
                 geom    = data.plot.fun,
                 mapping = aes(
                             y=tips,
                             x=values,
                             group = central
                           )
                )    

}

plot <- circular.plot(data = results_phylo$position, tree = demo_tree, col.tips = c("orange", "blue", "darkgreen"), vals.gradient = c("blue", "orange"))

   











# p <- p +
#      scale_fill_discrete(
#          name="Phyla",
#          guide=guide_legend(keywidth=0.8, keyheight=0.8, ncol=1)
#      ) +
#      theme(
#          legend.title=element_text(size=9), # The title of legend 
#          legend.text=element_text(size=7) # The label text of legend, the sizes should be adjust with dpi.
#      )
# p