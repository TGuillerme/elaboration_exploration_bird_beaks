#' @title gg.color.hue
#'
#' @description Create the ggplot hue palette.
#' 
#' @param n the number of colours
#' 
## The ggplot color panel
gg.color.hue <- function(n) {
    grDevices::hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100)[1:n]
}

#' @title visualisation.group
#'
#' @description plotting the ellipses from a covar object
#' 
#' @param data a dispRity object containing the shapespace and covar components
#' @param subset which subset to plot
#' @param dimensions which dimensions to plot
#' @param n the number of ellipses to plot
#' 
## The ggplot color panel
visualisation.group <- function(data, subset, dimensions, n = 100) {
    ## Get the subset
    sub <- get.subsets(data, subsets = subset)

    ## Make 2D
    if(!missing(dimensions)) {
        sub$matrix[[1]] <- sub$matrix[[1]][,dimensions]
        sub$call$dimensions <- dimensions
        fun2d <- function(covar) {
            covar$VCV <- covar$VCV[dimensions, dimensions]
            covar$loc <- covar$loc[dimensions]
            return(covar)
        }
        sub$covar <- lapply(sub$covar, lapply, fun2d)
        main_D <- "2D"
    } else {
        main_D <- "8D"
    }

    ## Make the tile
    main <- paste0(n, " random ", subset, " VCV (", main_D, ")")

    ## Plot
    covar.plot(sub, major.axes = TRUE, ellipses = TRUE, points = FALSE, legend = FALSE, main = main)
    covar.plot(sub, major.axes = mean, ellipses = mean, points = FALSE, col = "red", add = TRUE)
    rm(sub)
}

#' @title plot.one.ellipse
#'
#' @description Plots a single ellipse (typically for the ellipse figure)
#' 
#' @param data the dispRity object with the covar components
#' @param name.col the colour name
#' @param x logical, whether to plot the x axis
#' @param y logical, whether to plot the y axis
#' @param main the plot title
#' @param legend logical, whether to add the legend
#' @param with.phylo logical, whether to plot the phylo ellipse
#' @param lwd line thickness for the ellipses
#' @param lims either a set of limits (default is c(-1/3, 1/3)) or "dynamic" to try to get the best limits
#' 
## Plotting the ellipses
plot.one.ellipse <- function(data, name.col, x = TRUE, y = TRUE, main = "", legend = FALSE, with.phylo = FALSE, lwd = 4, lims = c(-1/3, 1/3)) {

    ## Get the PC %ages
    if(x || y) {
        vars <- dispRity::variances(data$matrix[[1]])
        pc_var <- round((vars/sum(vars)*100), 2)
    }

    ## Select the subsets
    if(!with.phylo) {
        subsets <- names(name.col)
        cols <- name.col
    } else {
        subsets <- c("phylogeny", names(name.col))
        cols <- c("grey", name.col)
    }

    ## Do the empty plot
    if(lims[1] == "dynamic") {


        ## detect the limits
        ellipse_data <- get.subsets(data, subsets = subsets)
        covars <- sample.n(ellipse_data$covar, n = 100)
        ellipses <- mean

        ## Scaling the ellipses
        covars <- lapply(covars, function(one_covar, scale) mapply(scale.VCV, one_covar, scale, SIMPLIFY = FALSE), scale = covars[[subsets[1]]])
        
        ## Get the VCV central tendencies
        covars_cent_tend <- lapply(covars, VCV.cent.tend, ellipses)

        ## Get the centres of the ellipses
        centres <- lapply(unlist(ellipse_data$subsets, recursive = FALSE) ,function(group, data, fun) fun(data[c(group), ]), data = ellipse_data$matrix[[1]], fun = colMeans)
        ## Get the ellipses
        all_ellipses <- lapply(level.ellipses(covars_cent_tend, dimensions = c(1,2), npoints = 50, centres, level = 0.95), list)

        ## Get the scaled ranges       
        xlims <- range(c(all_ellipses[[1]][[1]][, 1], all_ellipses[[2]][[1]][, 1]))
        ylims <- range(c(all_ellipses[[1]][[1]][, 2], all_ellipses[[2]][[1]][, 2]))
        # cat(paste0(names(name.col), "\n"))
        # cat(paste0("xlims = c(", paste0(xlims, collapse = ", "), ")\ndiff = ", diff(xlims), "\n"))
        # cat(paste0("ylims = c(", paste0(ylims, collapse = ", "), ")\ndiff = ", diff(ylims), "\n"))

        ## hard coded range
        hard_range <- 0.45
        if(hard_range > 0) {
            ## Spread the x range
            x_range_diff <- hard_range - diff(xlims)
            if(x_range_diff > 0) {
                xlims[1] <- xlims[1] - (x_range_diff/2)
                xlims[2] <- xlims[2] + (x_range_diff/2)
            }
            ## Spread the y range
            y_range_diff <- hard_range - diff(ylims)
            if(x_range_diff > 0) {
                ylims[1] <- ylims[1] - (y_range_diff/2)
                ylims[2] <- ylims[2] + (y_range_diff/2)
            }
        }

    } else {
        xlims <- ylims <- lims
    }

    plot(NULL, xlim = xlims, ylim = ylims, xaxt = "n", yaxt = "n", xlab = ifelse(x, paste0("PC1 (", pc_var[1], "%)"), ""), ylab = ifelse(y, paste0("PC2 (", pc_var[2], "%)"), ""), main = main, asp = 1)
    if(x) axis(1)
    if(y) axis(2)
    abline(v = 0, col = "grey", lwd = 0.5)
    abline(h = 0, col = "grey", lwd = 0.5)


    if(!with.phylo) {
        covar.plot(get.subsets(data, subsets = subsets),
               col = cols,
               ellipses = mean,
               legend = FALSE,
               points = FALSE,
               cex = 0.5, apply.to.VCV = TRUE,
               lwd = lwd,
               add = TRUE,
               legend = legend)
    } else {
        covar.plot(get.subsets(data, subsets = subsets),
               col = cols,
               ellipses = mean,
               legend = FALSE,
               points = FALSE,
               cex = 0.5, apply.to.VCV = TRUE,
               lwd = lwd,
               add = TRUE,
               legend = legend,
               scale = "phylogeny")
    }
}

#' @title add.dims
#'
#' @description Add the dimensions barplot to an ellipse plot (typically for the ellipse figure)
#' 
#' @param data the dispRity object with the covar components
#' @param name.col the colour name
#' @param n the number of covar matrices to use
#' @param new whether to smart add (FALSE; default) to a previous plot or not
#' @param lwd lwd, default = 3 
#' 
## Plot the variation per dimension
add.dims <- function(data, name.col, n = 1000, new = FALSE, xlims, lwd = 3) {
    covars_cent_tend <- lapply(sample.n(get.subsets(data, subset = names(name.col))$covar, n), VCV.cent.tend, mean)
    dim_var <- apply((get.one.axis(covars_cent_tend[[1]], axis = 1, level = 0.95)), 2, dist)
    dim_var <- dim_var/max(dim_var)/10

    ## Select the quadrant
    if(!new) {
        if(colMeans(data$matrix[[1]][c(data$subsets[[names(name.col)]]$elements), c(1,2)])[1] > 0.1) {
            x_start <- -0.2
        } else {
            x_start <- 0.2
        }
        y_start <- -0.1
        ## Plot the dimensions
        buffer <- 0
    } else {
        ## Default xlimits
        if(missing(xlims)) {
            xlims <- c(0, max(dim_var))
        }
        plot(NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = rev(c(1, length(dim_var))), xlim = xlims)
        #plot(NULL, ylim = rev(c(1, length(dim_var))), xlim = xlims)
        x_start <- 0
        y_start <- 1
        buffer <- 0
    }

    for(one_dim in 1:length(dim_var)) {
        lines(x = c(x_start, x_start+dim_var[one_dim]), y = rep(y_start+buffer, 2), lwd = lwd, col = name.col)
        if(!new) {
            buffer <- buffer - 0.02
        } else {
            buffer <- buffer + 1
        }
    }
}

#' @title plot.one.proj.rej
#'
#' @description Plot the elaboration/innovation thermos (typically for the ellipse figure)
#' 
#' @param data a list of dispRity objects with calculated projections and rejections (typically from dispRity.covar.projections)
#' @param name.col the colour name
#' @param x logical, whether to plot the x axis
#' @param y logical, whether to plot the y axis
#' @param main the plot title
#' @param ei.col optional, the colour for elaboration and innovation (overrides name.col)
#' 
## Plot the projection/rejection
plot.one.proj.rej <- function(data, name.col, x = TRUE, y = TRUE, main = "", ei.col) {

    ## Get the proj/rej
    elaboration <- get.disparity(data$position, concatenate = FALSE)
    exploration <- get.disparity(data$distance, concatenate = FALSE)

    ## Get the correct group
    if(length(ID <- which(names(elaboration) %in% names(name.col))) < 1) {
        ID <- which(names(elaboration) %in% paste0(names(name.col), ":phylogeny"))
    }
    elaboration <- c(elaboration[[ID]])
    exploration <- c(exploration[[ID]])

    ## Change the colours
    col <- c(name.col, adjustcolor(name.col, alpha.f = 0.5))

    ## Overiding colours
    if(!missing(ei.col)) {
        col <- ei.col
    }

    
    ## Set the limits
    quant_95 <- quantile(c(elaboration, exploration), prob = 0.975)
    lim_up <- c(ceiling(quant_95/5)*5)

    ## Get the quantiles
    elaboration_quantiles <- quantile(elaboration, prob = c(0.025, 0.25, 0.50, 0.975))
    exploration_quantiles <- quantile(exploration, prob = c(0.025, 0.25, 0.50, 0.975))

    ## Empty plot
    par(bty = "o")
    plot(NULL, xlim = c(-0.5,1.5), ylim = c(0,lim_up), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

    ## Add the lines
    lines(lty = 2, x = c(0,0), y = elaboration_quantiles[c(1,4)], col = col[1], lwd = 1)
    lines(lty = 1, x = c(0,0), y = elaboration_quantiles[c(2,3)], col = col[1], lwd = 3)
    lines(lty = 2, x = c(1,1), y = exploration_quantiles[c(1,4)], col = col[2], lwd = 1)
    lines(lty = 1, x = c(1,1), y = exploration_quantiles[c(2,3)], col = col[2], lwd = 3)
    points(0, median(elaboration), pch = 19, col = col[1])
    points(1, median(exploration), pch = 19, col = col[2])

    ## Add the y axis
    axis(2, at = seq(from = 0, to = lim_up, by = 5), labels = seq(from = 0, to = lim_up, by = 5), las = 2)
}

#' @title plot.short.tree
#'
#' @description plots the shorten tree (with tip numbers) and outputs the group colour vector (typically for the ellipse figure)
#' 
#' @param tree the complete tree
#' @param shapespace the shapespace data frame (with levels)
#' @param level which level to plot the short tree for
#' @param colour.palette the colour palette to draw from
#' @param tip.order the order of the tips in the tree (can be left missing)
#'
plot.short.tree <- function(tree, shapespace, level, colour.palette = gg.color.hue, tip.order) {

    ## Ladderizing the tree
    tree <- ladderize(tree)

    ## Getting the correct level
    selected_level <- coloured_levels <- shapespace[, level]

    ## Replace the order levels as colours
    colour_palette <- c("grey", colour.palette(length(levels(coloured_levels)[-1])))
    levels(coloured_levels) <- c("grey", colour.palette(length(levels(coloured_levels)[-1])))

    ## Shorten the tree
    selected_tips <- character()
    for(one_level in 2:length(levels(selected_level))) {
        selected_tips[one_level-1] <- rownames(shapespace)[which(selected_level == levels(selected_level)[one_level])[1]]
        names(selected_tips)[one_level-1] <- levels(selected_level)[one_level]
    }

    ## Shorten the tree
    tree_short <- drop.tip(tree, tip = tree$tip.label[!(tree$tip.label %in% selected_tips)])
    tree_short$tip.label <- names(selected_tips)[match(tree_short$tip.label, selected_tips)]

    ## Setting the tips edges colours
    tip_edges <- match.tip.edge(tree_short$tip.label, tree_short, replace.na = "grey")
    ## Rainbow the tip edges
    tip_edges[!(tip_edges %in% "grey")] <- rev(colour.palette(Ntip(tree_short)))
    ## Adding the number of tips
    tree_plot <- tree_short
    n_species <- table(selected_level)
    for(i in 1:Ntip(tree_plot)) {
        ## Get the number of species
        n_sp <- n_species[which(names(n_species) %in% tree_plot$tip.label[i])]
        tree_plot$tip.label[i] <- paste0(tree_plot$tip.label[i], " (", n_sp, " sp)")
    }

    ## Plotting the tree
    par(mar = c(0, 0, 0, 0)+0.1)
    plot(tree_plot, show.tip.label = TRUE, edge.color = tip_edges, edge.width = 3, cex = 1)

    ## Save and return the tip order
    if(!missing(tip.order)) {
        tip_colours <- colour.palette(Ntip(tree_short))
        names(tip_colours) <- names(sort(tip.order))
        return(tip_colours)
    }
}

#' @name wrap.plot.ellipses
#'    
#' @description plots the ellipses, dimensionality and projection/rejection thermos
#'
#' @param i the index of the element to plot (usually along tip_order)
#' @param tip.colours the named vector with colours and group names
#' @param shapespace the shapespace as a dispRity object
#' @param results the list of dispRity objects containing the results (typically output from dispRity.covar.projections)
#' @param ei.col optional, the colour for elaboration and innovation (overriding tip.colours)
#' @param phylo.plot optional, change the margins and plot options to match the big phylo plot (FALSE; default)
#
## Plotting the ellipses, the dimensions and the projection/rejection
wrap.plot.ellipses <- function(i, tip.colours, shapespace, results, ei.col, phylo.plot = FALSE) {
    ## Select the colour and name
    name.col <- tip.colours[i]

    ## Plot the ellipse
    if(!phylo.plot) {
        par(bty = "n", mar = c(1, 1, 1, 1)+0.1)
        lwd <- 4
        lims <- c(-1/3, 1/3)
    } else {
        par(bty = "n", mar = c(0, 0, 0, 0))
        lwd <- 1.8
        lims <- "dynamic"
    }
    plot.one.ellipse(shapespace, name.col, x = FALSE, y = FALSE, with.phylo = TRUE, lwd = lwd, lims = lims)

    ## Calculate which dimension contains the most variance
    if(!phylo.plot) {
        add.dims(shapespace, name.col)
        n <- 1000
        covars_cent_tend <- lapply(sample.n(get.subsets(shapespace, subset = names(name.col))$covar, n), VCV.cent.tend, mean)
        dim_var <- apply((get.one.axis(covars_cent_tend[[1]], axis = 1, level = 0.95)), 2, dist)
        text_main <- paste0(names(name.col))
        text(0,0.3, text_main, cex = 1.2, col = name.col)
    } else {
        par(bty = "n", mar = c(1, 1, 1, 1))
        add.dims(shapespace, name.col, new = TRUE)
    }

    ## Plot the elaboration/exploration
    if(!phylo.plot) {
        par(mar = c(2, 1, 1, 1)+0.1)
    } else {
        par(mar = c(0, 1, 0, 0))
    }
    plot.one.proj.rej(results, name.col, x = TRUE, y = FALSE, ei.col = ei.col)
}

#' @name scale rgb color
#'    
#' @description Scales a point on the blue/yellow/green colour scheme 
#'
#' @param x the x coordinates
#' @param y the y coordinates
#' @param max.x the highest x value
#' @param max.y the highest y value
#' @param alpha a transparency scaling (1 is not transparent)
#'
## Scale point to RGB
scale.rgb <- function(x, y, max.x, max.y, alpha = 1) {
    return(rgb(red = 255-((x/max.x)*255), blue = 255-((y/max.y)*255), green = 255, maxColorValue = 255, alpha = alpha*255))
}

#' @name add.quadrant
#'
#' @description add the elaboration innovation quadrants on a plot
#'
#' @param data the data output from get.correlations
#' @param midpoint where to put the midpoint of the quadrant, either a numeric value (default is 1) or "middle" for the middle of the plot window
#'
add.quadrant <- function(data, midpoint = 1, color = TRUE) {
    max.x = max(range(pretty(data$x)))
    max.y = max(range(pretty(data$y)))

    ## Select the quadrant values
    if(is(midpoint, "character") && midpoint == "middle") {
        x_val1 <- mean(range(pretty(data$x)))
        y_val1 <- mean(range(pretty(data$y)))
    } else {
        x_val1 <- y_val1 <- midpoint
    }
    x_val2 <- max(range(pretty(data$x)))
    y_val2 <- max(range(pretty(data$y)))

    if(color) {
        ## Bottom left
        polygon(x = c(0, 0, x_val1, x_val1),
                y = c(0, y_val1, y_val1, 0),
                col = scale.rgb(0, 0, max.x, max.y, alpha = 1/3), border = "white")
        ## Bottom right
        polygon(x = c(x_val1, x_val1, x_val2, x_val2),
                y = c(0, y_val1, y_val1, 0),
                col = scale.rgb(x_val2, 0, max.x, max.y, alpha = 1/3), border = "white")
        ## Top left
        polygon(x = c(0, 0, x_val1, x_val1),
                y = c(y_val1, y_val2, y_val2, y_val1),
                col = scale.rgb(0, y_val2, max.x, max.y, alpha = 1/3), border = "white")
        ## Top right
        polygon(x = c(x_val1, x_val1, x_val2, x_val2),
                y = c(y_val1, y_val2, y_val2, y_val1),
                col = scale.rgb(x_val2, y_val2, max.x, max.y, alpha = 1/3), border = "white")
    }

    abline(v = x_val1, lwd = 0.5, lty = 2)
    abline(h = y_val1, lwd = 0.5, lty = 2)
}

#' @name quadrant.color
#'
#' @description get the quadrant equivalent color for a species elaboration/innovation
#'
#' @param elaboration the elaboration score
#' @param innovation the innovation score
#' @param exact.color whether to return the exact color (TRUE) or the quadrant color (FALSE - default)
#' @param alpha the transparency value (default is 1/3 to match the one in the correlation figure)
#'
quadrant.color <- function(elaboration, innovation, exact.color = FALSE, alpha = 1/3) {

    ## Max values (from the whole dataset)
    max.x <- 2.5
    max.y <- 1.4

    if(exact.color) {
        return(rgb(red = 255-((elaboration/max.x)*255), blue = 255-((innovation/max.y)*255), green = 255, maxColorValue = 255, alpha = alpha*255))
    }

    ## Get the position in the quadrant
    quadrant <- as.integer(c(elaboration < (max.x/2), innovation < (max.y/2)))

    ## Get the position name
    if(all(quadrant == c(1, 1))) {
        return(rgb(red = 255, blue = 255, green = 255, maxColorValue = 255, alpha = alpha*255))
    } else {
        if(all(quadrant == c(1, 0))) {
            return(rgb(red = 0, blue = 255, green = 255, maxColorValue = 255, alpha = alpha*255))
        } else {
            if(all(quadrant == c(0, 1))) {
                return(rgb(red = 255, blue = 0, green = 255, maxColorValue = 255, alpha = alpha*255))
            } else {
                if(all(quadrant == c(0, 0))) {
                    return(rgb(red = 0, blue = 0, green = 255, maxColorValue = 255, alpha = alpha*255))
                }
            }
        }
    }
}

#' @name plot.densities
#'
#' @description plotting the densities of the results of a correlation plot
#'
#' @param data the data to plot (output from get.correlations)
#' @param with.quadrant whether to add the quadrant or not
##
plot.densities <- function(data, with.quadrant = TRUE) {
    plot(NULL, xlab = "elaboration", ylab = "innovation", xlim = range(pretty(data$x)), ylim = range(pretty(data$y)))
    if(with.quadrant) {
        add.quadrant(data)
    }
    ## Calculate the points densities
    colour_densities <- densCols(cbind(data$x, data$y), colramp = scales::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "D"))
    points(cbind(data$x, data$y), col = colour_densities, pch = 20)
}


#' @name plot.correlations
#'    
#' @description wrapping function for plotting the correlation results
#'
#' @param cor.results the correlation results (output from get.correlations)
#' @param col.sub the list of factors for sub dividing the colours for plotting (basically the overall group colours)
#' @param col.fun the function for the colour palette
#' @param legend.pos where to plot the legend
#' @param ID the plot number
#' @param pt.cex point size
#' @param legend.cex legend size
#' @param legend_counter counting the legend for numbering
#' @param contour whether to add contour lines (leave as null for no contour, else, add a named vector of distances from centre)
#' @param with.quadrant whether to add the quadrants or not
#' @param quadrant.color whether to colour the quadrant
#'
## Plotting one correlation
plot.correlations <- function(cor.results, col.sub, col.fun = colour.palette, legend.pos = "topleft", pt.cex = 0.5, legend.cex = 0.5, ID, legend_counter, contour = NULL, with.quadrant = FALSE, quadrant.color = FALSE) {

    if(missing(col.sub)) {
        col.sub <- NULL
    }

    ## Subbing the colours
    if(!is.null(col.sub)) {
        ## Reset the levels
        col_sub <- as.factor(as.character(col.sub))
        ## Make the empty level grey
        
        # if(is(col.fun)) ## With col.fun a function or not (if not, take the colours directly from the colour vector)


        if(any(levels(col_sub) == "")) {
            col_avail <- c("grey", col.fun(length(levels(col_sub))-1))
        } else {
            col_avail <- col.fun(length(levels(col_sub)))
        }




        ## Create the colour vector
        col_vector <- col_avail[as.numeric(col_sub)]
    } else {
        if(is.null(dots$col)) {
            col_vector <- "black"
        } else {
            col_vector <- dots$col
        }
    }

    ## Sort the data by group size
    all_data <- cbind(x = cor.results$x, y = cor.results$y)
    ## Classify the groups per numbers of species
    all_data <- data.frame(all_data, col = col_vector)

    ## Sort the data per group size
    group_order <- sort(table(all_data$col), decreasing = TRUE)

    ## Set the plotting data
    plot_data <- data.frame()
    # col_adjust <- seq(from = 1/length(group_order), to = 1, by = 0.1)*3
    for(i in 1:length(group_order)) {
        ## Get the data from the biggest group
        selected_data <- all_data[which(all_data$col == names(group_order[i])), ]
        ## Transparantise the colours (for the background)
        # selected_data$col <- adjustcolor(selected_data$col, alpha = col_adjust[i])
        ## Combine the plotting data
        plot_data <- rbind(plot_data, selected_data)
    }

    ## Empty plot
    plot(NULL, xlim = range(plot_data$x), ylim = range(plot_data$y), xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = strsplit(cor.results$main, split = ":")[[1]][1])
    axis(1, at = pretty(plot_data$x, n = 4))
    axis(2, at = pretty(plot_data$y, n = 4), las = 2)

    if(with.quadrant) {
        add.quadrant(plot_data, color = quadrant.color)
    }

    ## Adding contours
    if(!is.null(contour)) {

        ## Get the contour data
        contour_data <- contour[match(rownames(plot_data), names(contour))]

        ## This function is copied/modified from ContourFunctions
        estimate.contour <- function(x, y, z) {
            X <- data.frame(x, y, z)
            ## Set the model for estiamtions
            if(nrow(X) > 25) {
                ## Predicting model (using a general additive model)
                gammod <- mgcv::gam(z ~ te(x, y), data = X, family= "gaussian")
                ## Predicting function
                pred.func <- function(xx) {
                  predict(gammod, data.frame(x = xx[,1], y = xx[,2]), type = "response")
                }
            } else {
                ## Using a local regression
                lfmod <- locfit::locfit(z ~ x + y, data = X, family = "gaussian")
                ## Predicting function
                pred.func <- function(xx) {
                  predict(lfmod, data.frame(x=xx[,1], y=xx[,2]))
                }
            }

            ## Calculating the contour matrix
            return(ContourFunctions::eval_over_grid_with_batch(x = seq(min(x), max(x), length.out = 100),
                                                               y = seq(min(y), max(y), length.out = 100),
                                                               fn = pred.func,
                                                               batchmax = 500))
        }

        ## Adding the contours
        z_matrix <- estimate.contour(x = plot_data$x, y = plot_data$y, z = unname(contour_data))
        x_seq <- seq(min(plot_data$x), max(plot_data$x), length.out = 100)
        y_seq <- seq(min(plot_data$y), max(plot_data$y), length.out = 100)
        contour(x_seq, y_seq, z_matrix,
                lwd = 1, add = TRUE,
                col = hcl.colors(12, palette = "viridis"),
                labels = pretty(contour_data))
    }

    ## Plot the points
    points(plot_data$x, plot_data$y, pch = 19, cex = pt.cex, col = plot_data$col)

    if(!is.null(col.sub)) {
        ## See if there is any others
        levels <- levels(col_sub)
        if(length(other <- which(levels == "")) > 0) {
            legend_text <- c(1, 1:(length(levels(col_sub))-1)+legend_counter)
        } else {
            legend_text <- c(1:(length(levels(col_sub)))+legend_counter)
        }
        ## Update the legend counter
        legend_counter <<- max(legend_text)
        ## Adding the legend
        legend(x = legend.pos, legend = legend_text, pch = 19, cex = legend.cex, col = col_avail, bg = "white")
    }

    return(invisible())
}


#' @name plot.cor.scores
#'
#' @description plots the correlation scores fable
#'
#' @param cor.scores the correlation scores
#' @param main the title of the plot
#' @param col the colour vector
#' @param y.axis whether to plot the y.axis or not
#' @param plot.phylo whether to plot the phylo term or not (left empty)
plot.cor.scores <- function(cor.scores, main, col, y.axis, plot.phylo) {
    ## Empty plot
    plot(NULL, yaxt = "n", xlim = c(-1, 1), ylim = c(1, length(cor.scores)), ylab = "", xlab = "Posterior\ncorrelations", main = main)
    ## Adding the 0 line
    abline(v = 0, lty = 2, col = "grey", lwd = 0.5)
    ## Adding the y labels
    if(y.axis) {
        axis(2, at = 1:length(cor.scores), labels = names(cor.scores), las = 2)
    }
    ## Adding the lines
    for(i in 1:length(cor.scores)) {
        do_plot <- TRUE
        if(!plot.phylo) {
            if(names(cor.scores)[i] == "phylogeny") {
                do_plot <- FALSE
            }
        }

        if(do_plot) {

            ## Get the selected colour
            select_col <- col[names(cor.scores)[i]]
            ## Get the quantile
            quants <- quantile(cor.scores[[i]]$correlations, prob = c(0.025, 0.25, 0.75, 0.975))
            ## Add the lines
            lines(y = rep(i, 2), x = c(quants[c(1, length(quants))]), lty = 2, col = select_col)
            lines(y = rep(i, 2), x = c(quants[c(2, (length(quants)-1))]), lty = 1, col = select_col, lwd = 2)
            ## Add the median
            points(y = i, x = median(cor.scores[[i]]$correlations), pch = 19, col = select_col)
            ## Add the stars
            if(cor.scores[[i]]$p.value < 0.1) {
                stars <- "."
                stars <- if(cor.scores[[i]]$p.value < 0.001) {
                        "***"
                        } else {
                            if(cor.scores[[i]]$p.value < 0.01) {
                                "**"
                            } else {
                                if(cor.scores[[i]]$p.value < 0.05) {
                                    "*"
                                }
                            }
                        }
                ## Add the stars
                text(y = i + 0.1, x = median(cor.scores[[i]]$correlations) + 0.1, labels = stars, col = select_col)
            }
        }
    }
}


## Sort each level by orthogonality score
ortho.sort <- function(data) {

    return(data[order(data$orthogonality, decreasing = TRUE), , drop = FALSE])
}
## Adding stars and thingies here and there
get.signif <- function(prob) {
    if(prob >= 0.99) {
        return("***")
    } else {
        if(prob >= 0.95) {
            return("**")
        } else {
            if(prob >= 0.90) {
                return("*")
            } else {
                if(prob >= 0.80) {
                    return(".")
                } else {
                    return("")
                }
            }
        }
    }
}
add.stars <- function(data, counter, shift = 0, total, ...) {

    text(data[3] + 0.03, (total+1)-counter + shift + 0.2, labels = get.signif(data[6]), cex = 1, ...)   
}
add.line <- function(data, counter, shift = 0, total, pch = 19, lty = 1, ...) {

    lines(matrix(c(data[4:5], rep((total+1)-counter + shift, 2)), 2, 2), lty = lty, ...)
    points(data[3], (total+1)-counter + shift, pch = pch,  ...)
}
convert.spaces <- function(x, max) {

    # shifts <- max-nchar(as.character(x))
    # if(shifts > 0) {
    #     return(paste(c(x, rep(" ", shifts)), collapse = ""))
    # } else {
    #     return(x)
    # }
    return(x)
}
## Plotting orthogonality
plot.orthogonality <- function(level1, level2.1, level2.2, col) {
    ## Plotting table (with the sorted orthogonality)
    plot_table <- rbind(ortho.sort(level1),
                        ortho.sort(level2.1))
    colours <- c(rep(col[1], nrow(level1)),
                 rep(col[2], nrow(level2.1)))

    ## Set up the group names
    group_names <- paste0(plot_table$Group, " (n = ", sapply(plot_table$n, convert.spaces, max = 4), " ; sd = ", sapply(round(plot_table$`ellipse sd`, 2), convert.spaces, max = 5), ")")
    plot_order <- 1:length(group_names)

    ## Plot frame
    par(mar = c(5, 17, 4, 2))
    plot(NULL, yaxt = "n", xaxt = "n", ylab = "", xlab = "orthogonality", ylim = c(1,nrow(plot_table)), xlim = c(0,1))
    abline(v = 0.5, lwd = 0.5, col = "grey")
    abline(v = 0.75, lty = 2, col = "grey", lwd = 0.5)
    axis(2, at = nrow(plot_table):1, labels = group_names[plot_order], las = 2)
    axis(1, at = c(0, 0.5, 1), labels = c(0, 0.5, 1))
    ## Adding the results
    for(i in 1:nrow(plot_table)) {
        if(!all(is.na(plot_table[plot_order[i], c(3:8)]))) {
            add.line (plot_table[plot_order[i], ], counter = i, col = colours[plot_order[i]], total = nrow(plot_table))
            add.stars(plot_table[plot_order[i], ], counter = i, col = colours[plot_order[i]], total = nrow(plot_table))
        }
    }

    ## Adding the n2 results
    level2_plot <- level2.2[match(plot_table$Group[-c(1:nrow(level1))], level2.2$Group), ]
    # families_n2_plot <- families_n2[match(families[order(families$orthogonality, decreasing = TRUE), ]$Group, families_n2$Group), ]
    for(i in 1:nrow(level2_plot)) {
        if(!all(is.na(level2_plot[plot_order[i], c(3:8)]))) {
            add.line(level2_plot[i, ], shift = -1/3, counter = i+nrow(level1), col = adjustcolor(colours[i+nrow(level1)], alpha.f = 2/3), total = nrow(plot_table), lty = 2, pch = 21)
            add.stars(level2_plot[i, ], shift = -1/3, counter = i+nrow(level1), col = adjustcolor(colours[i+nrow(level1)], alpha.f = 2/3), total = nrow(plot_table))
        }
    }
}




## Internals from dispRity
scale.VCV <- function(VCV1, VCV2) {
    ## Getting the scaling ratio based on the major axis length
    ratio <- dist(get.one.axis(VCV1))[1]/dist(get.one.axis(VCV2))[1]

    ## Scaling VCV1
    VCV1$VCV <- VCV1$VCV/ratio^2
    return(VCV1)
}
## Get the major axis
level.ellipses <- function(level_sample, dimensions, npoints, centre, level) {
    ## Recentreing the levels
    level_sample <- recentre.levels(level_sample, centre, dimensions)

    ## Get the ellipses for the level
    return(lapply(level_sample, make.ellipse, dimensions, npoints, level))
}
recentre.levels <- function(level_sample, centre, dimensions) {
    ## Centre the ellipse
    if(is(centre, "function")) {
        ## Get the central tendency (as a function)
        centre_values <- apply(do.call(rbind, lapply(level_sample, `[[`, "loc")), 2, centre)
        ## Recentre the intercepts
        level_sample <- replace.intercept(level_sample, value = centre_values, dimensions)
    }
    if(is(centre, "numeric") || is(centre, "integer")) {
        if((diff <- length(level_sample[[1]]$loc) - length(centre)) > 0) {
            centre <- c(centre, rep(centre, diff))
        }
        ## Manually recentre the intercepts
        level_sample <- replace.intercept(level_sample, value = centre, dimensions)
    }
    return(level_sample)
}
make.ellipse <- function(one_sample, dimensions, npoints, level){
    return(ellipse::ellipse(x       = one_sample$VCV[dimensions, dimensions],
                            centre  = one_sample$loc[dimensions],
                            npoints = npoints,
                            level   = level))
}