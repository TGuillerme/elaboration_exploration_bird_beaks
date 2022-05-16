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

# library(ggplot2)
# library(ggtreeExtra)
# library(ggtree)
# library(phyloseq)
# library(dplyr)

# tree <- demo_tree
# data <- results_phylo$position

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
}



#@param data dispRity data
#@param tree the tree
#@param gradient optional, a colour gradient
#@param clades optional, a vector of colour for each clade
#@param clade.names optional, whether to add the clade names or not
#@param legend logical, whether to add the legend (TRUE) or not (FALSE)
#@param fun which phytools function to use (default is phytools::plotTree.wBars)
#@param ... any arguments to be passed to the phytools functions


circular.plot.phytools <- function(data, tree, gradient, clades, clade.names = FALSE, legend = FALSE, fun = phytools::plotTree.wBars, ...) {

    dots <- list(...)

    ## Placeholder for the nodes labels
    clade_nodes <- numeric()
    if(!missing(clades)) {
        ## Vector of clade colours
        if(length(clades) != n.subsets(data)) {
            clades <- rep(clades, n.subsets(data))
        }
    }
    
    ## Set up the data
    list_of_tips <- tree$tip.label
    data_sorting <- matrix(NA, nrow = length(list_of_tips), ncol = data$call$bootstrap[[1]], dimnames = list(list_of_tips))
    n_groups <- n.subsets(data)
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
        ## Find the MRCAs for the node plottings
        clade_nodes[one_group] <- getMRCA(tree, tip = rownames(data$disparity[[one_group]]$elements[selected_tips, ]))
        names(clade_nodes)[one_group] <- names(data$disparity)[one_group]
    }

    ## Get the central tendencies
    centrals <- apply(data_sorting, 1, median)

    ## Get the colours for the central values
    ranged_central <- centrals - min(centrals)
    ranged_central <- ranged_central/max(ranged_central)
    data_cols <- rgb(colorRamp(c(gradient[1], gradient[2]))(ranged_central)/255)


    ## Get the phytools args
    plotTree_args <- list()
    plotTree_args$tree <- tree
    plotTree_args$x <- centrals
    plotTree_args$col <- data_cols
    if(is.null(dots$type)) {
        plotTree_args$type <- "fan"
    } else {
        plotTree_args$type <- dots$type
    }
    if(!is.null(dots$border)) {
        plotTree_args$border <- dots$border
    }
    if(!is.null(dots$width)) {
        plotTree_args$width <- dots$width
    }
    if(!is.null(dots$args.plotTree)) {
        plotTree_args$args.plotTree <- dots$args.plotTree
    }
    if(!is.null(dots$args.barplot)) {
        plotTree_args$args.barplot <- dots$args.barplot
    }

    ## Plot the tree
    do.call(fun, plotTree_args)
    
    ## And with clade labels for the beauty of it
    if(clade.names) {
        ## clade_args
        clade_args <- list()
        if(is.null(dots$mark.node)) {
            clade_args$mark.node <- FALSE
        }
        if(!is.null(dots$lab.offset)) {
            clade_args$lab.offset <- dots$lab.offset
        }        
        if(!is.null(dots$ln.offset)) {
            clade_args$ln.offset <- dots$ln.offset
        }
        if(!is.null(dots$lwd)) {
            clade_args$lwd <- dots$lwd
        }
        for(i in 1:length(clade_nodes)) {
            clade_args$text <- names(clade_nodes)[i]
            clade_args$node <- clade_nodes[i]
            clade_args$col <- clades[i]
            do.call(phytools::arc.cladelabels, clade_args)
        }
    }

    if(legend) {
        ## Adding the legend
        legend("topright", labels = round(range(centrals), digits = 3), pch = 15, col = c(gradient[1], gradient[2]))
    }

    return(invisible())
}






