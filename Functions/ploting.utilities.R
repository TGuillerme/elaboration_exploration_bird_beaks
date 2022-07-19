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
#' 
## Plotting the ellipses
plot.one.ellipse <- function(data, name.col, x = TRUE, y = TRUE, main = "", legend = FALSE, with.phylo = FALSE) {

    ## Get the PC %ages
    if(x || y) {
        vars <- dispRity::variances(data$matrix[[1]])
        pc_var <- round((vars/sum(vars)*100), 2)
    }

    ## Do the empty plot
    lims <- c(-1/3, 1/3) # c(-1.25, 1.25)
    plot(NULL, xlim = lims, ylim = lims, xaxt = "n", yaxt = "n", xlab = ifelse(x, paste0("PC1 (", pc_var[1], "%)"), ""), ylab = ifelse(y, paste0("PC2 (", pc_var[2], "%)"), ""), main = main)
    if(x) axis(1)
    if(y) axis(2)
    abline(v = 0, col = "grey", lwd = 0.5)
    abline(h = 0, col = "grey", lwd = 0.5)

    if(!with.phylo) {
        subsets <- names(name.col)
        cols <- name.col
    } else {
        subsets <- c("phylogeny", names(name.col))
        cols <- c("grey", name.col)
    }
    if(!with.phylo) {
        covar.plot(get.subsets(data, subsets = subsets),
               col = cols,
               ellipses = mean,
               legend = FALSE,
               points = FALSE,
               cex = 0.5, apply.to.VCV = TRUE,
               lwd = 4,
               add = TRUE,
               legend = legend)
    } else {
        covar.plot(get.subsets(data, subsets = subsets),
               col = cols,
               ellipses = mean,
               legend = FALSE,
               points = FALSE,
               cex = 0.5, apply.to.VCV = TRUE,
               lwd = 4,
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
#' 
## Plot the variation per dimension
add.dims <- function(data, name.col, n = 1000) {
    covars_cent_tend <- lapply(sample.n(get.subsets(data, subset = names(name.col))$covar, n), VCV.cent.tend, mean)
    dim_var <- apply((get.one.axis(covars_cent_tend[[1]], axis = 1, level = 0.95)), 2, dist)
    dim_var <- dim_var/max(dim_var)/10

    ## Select the quadrant
    if(colMeans(data$matrix[[1]][c(data$subsets[[names(name.col)]]$elements), c(1,2)])[1] > 0.1) {
        x_start <- -0.2
    } else {
        x_start <- 0.2
    }
    y_start <- -0.1

    ## Plot the dimensions
    buffer <- 0
    for(one_dim in 1:length(dim_var)) {
        lines(x = c(x_start, x_start+dim_var[one_dim]), y = rep(y_start+buffer, 2), lwd = 3, col = name.col)
        buffer <- buffer - 0.02
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
#' 
## Plot the projection/rejection
plot.one.proj.rej <- function(data, name.col, x = TRUE, y = TRUE, main = "") {

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
#
## Plotting the ellipses, the dimensions and the projection/rejection
wrap.plot.ellipses <- function(i, tip.colours, shapespace, results) {
    ## Select the colour and name
    name.col <- tip.colours[i]

    ## Plot the ellipse
    par(bty = "n", mar = c(1, 1, 1, 1)+0.1)
    plot.one.ellipse(shapespace, name.col, x = FALSE, y = FALSE, with.phylo = TRUE)

    ## Calculate which dimension contains the most variance
    add.dims(shapespace, name.col)
    n <- 1000
    covars_cent_tend <- lapply(sample.n(get.subsets(shapespace, subset = names(name.col))$covar, n), VCV.cent.tend, mean)
    dim_var <- apply((get.one.axis(covars_cent_tend[[1]], axis = 1, level = 0.95)), 2, dist)
    text_main <- paste0(names(name.col))
    text(0,0.3, text_main, cex = 1.2, col = name.col)

    ## Plot the elaboration/exploration
    par(mar = c(2, 1, 1, 1)+0.1)
    plot.one.proj.rej(results, name.col, x = TRUE, y = FALSE)
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
