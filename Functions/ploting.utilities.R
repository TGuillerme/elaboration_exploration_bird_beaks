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
