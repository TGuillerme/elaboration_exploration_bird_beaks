plot.base <- function(matrix, base_vector, lim) {

    if(missing(lim)) {
        lim <- ceiling(max(abs(range(matrix))))
        lim <- c(-lim, lim)
    }

    if(ncol(matrix) == 3) {
        plot3d(NULL, xlim = lim, ylim =lim, zlim = lim, xlab = "", ylab = "", zlab = "")
        segments3d(x = lim, y = c(0, 0), z = c(0, 0), col = "grey")
        text3d(x = min(lim)*0.9, y = 0, z = 0, texts = "X", col = "grey")
        segments3d(x = c(0,0), y = lim, z = c(0, 0), col = "grey")
        text3d(x = 0, y = min(lim)*0.9, z = 0, texts = "Y", col = "grey")
        segments3d(x = c(0,0), y = c(0,0), z = lim, col = "grey")
        text3d(x = 0, y = 0, z = min(lim)*0.9, texts = "Z", col = "grey")
        ## Add the original points
        text3d(matrix, texts = rownames(matrix), col = "grey")
        segments3d(base_vector, col = "grey")
    } else {
        par(bty = "n")
        plot(NULL, xlim = lim, ylim = lim, xlab = "x", ylab = "y")
        abline(v = 0, col = "grey")
        abline(h = 0, col = "grey")
        text(matrix[,1:2], labels = rownames(matrix), col = "grey", cex = 1.5)
        lines(base_vector, col = "grey", lwd = 1.5)
    }
}
plot.recentred <- function(matrix, base_vector, col = c("black", "grey")) {
    if(ncol(matrix) == 3) {
        text3d(matrix, texts = rownames(matrix), col = col[1])
        for(i in 1:nrow(matrix)) {
            segments3d(rbind(c(0,0,0), matrix[i,]), col = col[2])
        }
        segments3d(base_vector, col = col[1], lwd = 2)
    } else {
        text(matrix, labels = rownames(matrix), col = col[1], cex = 2)
        for(i in 1:nrow(matrix)) {
            lines(rbind(c(0,0), matrix[i,]), col = col[2], lwd = 2)
        }
        lines(base_vector, col = col[1], lwd = 3)
    }
}
plot.projections <- function(matrix, projections, rejections, col = c("black", "grey")) {
    if(ncol(matrix) == 3) {
        for(i in 1:nrow(matrix)) {
            ## Plot the projections
            segments3d(rbind(c(0,0,0), projections[i,]), col = "blue", lwd = 2)
            ## Plot the rejection
            segments3d(rbind(projections[i, ], rejections[i,]+projections[i, ]), col = "orange", lwd = 2)
        }

    } else {
        for(i in 1:nrow(matrix)) {
            ## Plot the projections
            lines(rbind(c(0,0), projections[i,]), col = col[1], lwd = 1)
            ## Plot the rejection
            lines(rbind(projections[i, ], rejections[i,]+projections[i, ]), col = col[2], lwd = 1.5, lty = 3)
        }        
    }
}
## Vector angle and matrix rotation from dispRity
source("~/Packaging/dispRity/R/dispRity.metric.R")

projections.covar.visualise <- function(matrix, matrix2, measure = "position", scaled = TRUE, axis = 1, level = 0.95) {

    ## Get the main axes from the VCV matrices
    # source("covar.utilities_fun.R")
    base_vector  <- get.one.axis(matrix2, axis, level, dimensions = 1:nrow(matrix2$VCV))
    projected_vector <- get.one.axis(matrix, axis, level, dimensions = 1:nrow(matrix$VCV))

    lims <- range(c(base_vector, projected_vector))
    plot(NULL, type = "l", xlim = lims*3, ylim = lims*3)

    ## Translating into projections format
    matrix <- projected_vector
    point1 <- base_vector[1,]
    point2 <- base_vector[2,]
    arrows(x0 = matrix[1, 1],
          y0 = matrix[1, 2],
          x1 = matrix[2, 1],
          y1 = matrix[2, 2],
          col = "grey", length = 0.1)
    arrows(x0 = point1[1],
          y0 = point1[2],
          x1 = point2[1],
          y1 = point2[2],
          col = "orange", length = 0.1)


    ## Moving the two axes so that there origins are the same:
    ## 1 - Get the translation vector to point1
    translation_vector <- point1 - matrix[1,]
    arrows(x0 = matrix[1, 1],
           y0 = matrix[1, 2],
           x1 = matrix[1, 1] + translation_vector[1],
           y1 = matrix[1, 2] + translation_vector[2],
           col = "grey", length = 0.1, lty = 2)

    ## 2 - Align the matrix with point1
    matrix <- rbind(matrix[1,] + translation_vector,
                    matrix[2,] + translation_vector)
    arrows(x0 = matrix[1,1],
          y0 = matrix[1,2],
          x1 = matrix[2,1],
          y1 = matrix[2,2],
          col = "blue", length = 0.1)

   ## Measure the projection
   # return(projections(matrix, point1 = point1, point2 = point2, measure = measure, scaled = scaled)[2])

    ## Visualisation continued:
    source("../tests/visualise.projections.R")
    # projections.debug(matrix[2,, drop = FALSE], point1 = point1, point2 = point2, measure = "distance", scaled = TRUE, plot.base = FALSE)

    ## All this bit below is handled by dispRity::projections
    matrix = matrix[2,, drop = FALSE]
    point1 = point1
    point2 = point2
    measure = "distance"
    scaled = TRUE
    plot.base = FALSE

    ## Get the base vector
    base_vector <- rbind(point1, point2)

    ## Get all the space (with the two last rows being the base vectors)
    space <- rbind(matrix, base_vector)

    ## Centre the matrix on point1
    if(sum(point1) != 0) {
        ## Centre all the space
        space <- space - rep(point1, rep.int(nrow(space), ncol(space)))
        ## Re-attribute the centred variables
        matrix <- space[1:nrow(matrix), , drop = FALSE]
        base_vector <- space[-c(1:nrow(matrix)), , drop = FALSE]
    }

    ## Scale the space
    if(scaled) {
        ## The scaled space
        space <- space/dist(space[-c(1:nrow(matrix)), , drop = FALSE])
    }

    ## Get the base vector axis (x) and the projection vector (former unit vector; y)
    x <- base_vector[2, ]
    y <- c(sqrt(sum(base_vector[2,]^2)), rep(0, (ncol(matrix)-1)))
    ## If the base vector and the unit vector are different...
    if(any(x != y)) {
        ## ...rotate the matrix on the x-axis
        space <- space %*% get.rotation.matrix(x, y)
    }

    ## Re-attributing the matrix and the vector
    matrix <- space[1:nrow(matrix), , drop = FALSE]
    base_vector <- space[-c(1:nrow(matrix)), , drop = FALSE]

    ## DEBUG
    plot.recentred(matrix, base_vector, col = c("orange", "blue"))

    ## Project the vectors
    projections <- t(apply(matrix, 1, geometry::dot, y = base_vector[2,], d = 2))
    angles <- t(t(apply(matrix, 1, vector.angle, base_vector[2,])))
    angles <- ifelse(is.nan(angles), 0, angles)

    ## DEBUG
    rejections <- matrix - projections
    plot.projections(matrix, projections, rejections, col = c("black", "grey"))

    return(invisible())

}


stop("DEBUG projections.covar")
for(i in 1:20) {
    n <- sample(1:750, 1)
    matrix2 <- model_phylo$MCMCglmm$covars$phylogeny[[n]]
    matrix  <- model_phylo$MCMCglmm$covars$mainland[[n]]

    matrix2$VCV <- matrix2$VCV[c(1,2) , c(1,2)]
    matrix2$Sol <- matrix2$Sol[c(1,2)]
    matrix$VCV  <- matrix$VCV[c(1,2) , c(1,2)]
    matrix$Sol  <- matrix$Sol[c(1,2)]

    projections.covar.visualise(matrix, matrix2)
    Sys.sleep(2)
}

