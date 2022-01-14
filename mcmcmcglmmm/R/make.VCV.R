#' @title Make an ellipse
#'
#' @description Generates an var-covar matrix that can be plotted as an ellipse
#'
#' @param shape the degree of roundness (1 = circle, 0 = line; default is 0.5)
#' @param covariance the covariance between dimensions (default is 0). Can be either a unique numeric value between 0 and 1 or a vector equal to the number of dimensions (see details).
#' @param rotation the rotation of the ellipse on each dimensions (expressed in degrees) (see details).
#' @param size the base radius of the circle at the base of the ellipse (default is 1).
#' @param position the ellipse centre's position (default is 0)
#' @param dimensions the number of dimensions (default is 2)
#' @param min.thick a proportional value to use a minimal thickness for the ellipse in each dimensions (default is 0). See details.
#' 
#' @details
#' Difference between \code{covariance} and \code{rotation}:
#' \itemize{
#'      \item the \code{covariance} argument adds one or multiple covariance values to the VCV matrix (i.e. its upper and lower triangles). It does not change the orientation of the matrix per se but results in the matrice's ellipse to rotate up to 45 degree and change its thickness: for example a covariance of 1 will result into a 1D ellipse (straight line) at 45 degrees.
#'      \item the \code{rotation} argument does not modify the values in the VCV matrix but outputs an additional $rotation argument that can be handled by plot.VCV. This changes the angle of the rotation matrix without changing its properties (size, shape, etc...).
#' }
#' 
#' When generating ellipses in low dimensions (e.g. < 10) or/and with extreme shape values, certain dimensions will be flat (i.e. will have a variance of 0). You can force dimensions to have a minimum thickness using \code{min.thick}. For example \code{min.thick = 0.1} will ensure that every dimensions has at least 10 percent of the required \code{size}.
#'
#' @returns
#' A list of three elements:
#' $VCV: the variance covariance matrix of this ellipse
#' $loc: the location of the centre of the ellipse
#' $rotation: the rotation matrix (is null if rotation = 0)
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
make.VCV <- function(shape = 1, covariance = 0, rotation = 0, size = 1, position = 0, dimensions = 2, min.thick = 0) {

    ## Set the output
    output <- list(VCV = NULL, loc = NULL)#, rotation = NULL)

    ## Setting the position
    if(length(position) != dimensions) {
        output$loc <- rep(position, dimensions)[1:dimensions]
    } else {
        output$loc <- position
    }

    ## Creating the variance covariance matrix
    output$VCV <- matrix(0, nrow = dimensions, ncol = dimensions)
    ## Give the shape to the ellipse
    diag(output$VCV) <- if(dimensions > 2) {
        ## Generate more than 2D
        generate.shape(dimensions = dimensions, lambda = get.lambda(shape), min.thick = min.thick)[,2]
    } else {
        ## Generate 3D but keep the two first ones
        generate.shape(dimensions = dimensions+1, lambda = get.lambda(shape), min.thick = min.thick)[1:dimensions,2]
    }

    ## Rotate the ellipse
    if(rotation != 0) {
        base <- rep(0, dimensions)
        for(one_rotation in 1:length(rotation)) {
            angle <- rotation/(180/pi)
            x_rot <- x <- base
            ## Get the axis on which to rotate
            x[one_rotation] <- 1

            # DEBUG
            # arrows(x0 = loc[1], y0 = loc[2], x1 = x[1], y1 = x[2], lwd = 2, col = "orange")

            ## Get landing vector where to rotate the matrix
            x_rot[1] <- x[1]*cos(angle) - x[2]*sin(angle)
            x_rot[2] <- x[1]*sin(angle) + x[2]*cos(angle)

            # DEBUG
            # arrows(x0 = loc[1], y0 = loc[2], x1 = x_rot[1], y1 = x_rot[2], lwd = 2, col = "blue")

            ## Rotate the matrix
            # output$rotation <- get.rotation.matrix(x, x_rot)
        }
    }

    ## Add the covariance       
    if(covariance != 0) {
        ## Adjust the covariance length
        if(length(covariance) != 1 && length(covariance) != sum(lower.tri(VCV))) {
            covariance <- rep(covariance, sum(lower.tri(VCV)))[1:sum(lower.tri(VCV))]
        }
        ## Adding the covariance component
        output$VCV[lower.tri(output$VCV, diag = FALSE)] <- output$VCV[upper.tri(output$VCV, diag = FALSE)] <- covariance
    }
    
    ## Scale the matrix with size
    diag(output$VCV) <- diag(output$VCV) * size

    # plot(ellipse::ellipse(output$VCV), type = "l", xlim = c(-3, 3), ylim =c(-3, 3))
    # arrows(x0 = loc[1], y0 = loc[2], x1 = VCV[1,1], y1 = VCV[1, 2])
    # arrows(x0 = loc[1], y0 = loc[2], x1 = VCV[2,1], y1 = VCV[2, 2])

    # lines(ellipse::ellipse(VCV_rot), col = "red")
    # arrows(x0 = loc[1], y0 = loc[2], x1 = VCV_rot[1,1], y1 = VCV_rot[1, 2], col = "red")
    # arrows(x0 = loc[1], y0 = loc[2], x1 = VCV_rot[2,1], y1 = VCV_rot[2, 2], col = "red")


    ## Output
    return(output)
}

## Calculate the vectors from a VCV matrix
vectors.VCV <- function(VCV) {
    
    ## Get the list of angles for each vector
    angles_list <- norm_list <- VCV
    diag(angles_list) <- 0
    norm_list[upper.tri(VCV) | lower.tri(VCV)] <- 0
    vector_list <- cbind(norm_list, angles_list)

    ## Rotate the vectors
    rotate.vector <- function(one_vector) {
        ## Create the base vector
        vector <- one_vector[1:(length(one_vector)/2)]
        rotation <- one_vector[-c(1:(length(one_vector)/2))]
        base <- rep(0, length(vector))

        ## Loop through each rotation
        for(i in 1:length(rotation)) {
            ## Rotate?
            if(rotation[i] != 0) {
                ## Create the unit vector
                unit_v <- ifelse(vector == 0, 0, 1)
                ## Get the unit vector scale
                scalar <- sqrt(sum(vector^2))
                ## New base
                base_i <- base
                base_i[i] <- 1
                ## Find the positions to rotate
                pos <- which(vector+base_i != 0)
                ## Get the angle of rotation
                angle <- (rotation[i]*45)/(180/pi)
                unit_v[pos[1]] <- base_i[pos[1]]*sin(angle) + base_i[pos[2]]*cos(angle)
                unit_v[pos[2]] <- base_i[pos[1]]*cos(angle) + base_i[pos[2]]*sin(angle)
                ## Scale the vector
                vector <- unit_v * scalar
            }
        }
        return(vector)
    }

    ## Vectors rotation
    return(t(apply(vector_list, 1, rotate.vector)))
}

## Internals
## This function approximates the lambda value corresponding the the correct shape
## Shape = 0: circle          -> lambda = Inf
## Shape = (0;0.5): pancake   -> lambda = (Inf; 1)
## Shape = 0.5: clean ellipse -> lambda = 1
## Shape = (0.5;1): cigar     -> lambda = (1; 0)
get.lambda <- function(shape) {
    ## Reverse x
    shape <- 1-shape
    ## Transform the extremes
    shape <- ifelse(shape == 0, -Inf, shape)
    shape <- ifelse(shape == 1, Inf, shape)
    ## Return the centred exp val of shape (scaled and centred so that 0.5 = ellipse)
    return(exp((shape*4-2)*2))
}

## This function generates the shape distribution for the lambda value (use lambda = get.lambda(shape))
generate.shape <- function(dimensions, lambda, min.thick = 0) {
    x <- seq(from = 0, to = 1, length.out = dimensions)
    y <- rev(x^lambda)
    if(min.thick != 0) {
        y <- ifelse(y < min.thick, min.thick, y)
    }
    return(cbind(x,y))
}

## Get the rotation matrix
get.rotation.matrix <- function(x, y){
    ## This magic comes from https://stackoverflow.com/questions/42520301/find-rotation-matrix-of-one-vector-to-another-using-r/42542385#42542385
    ## following: https://math.stackexchange.com/questions/598750/finding-the-rotation-matrix-in-n-dimensions
    ## Also this: http://wscg.zcu.cz/wscg2004/Papers_2004_Short/N29.pdf
    u <- x/sqrt(sum(x^2))

    v <- y-sum(u*y)*u
    v <- v/sqrt(sum(v^2))

    cost <- sum(x*y)/sqrt(sum(x^2))/sqrt(sum(y^2))
    sint <- sqrt(1-cost^2);

    return(diag(length(x)) - u %*% t(u) - v %*% t(v) + cbind(u,v) %*% matrix(c(cost,-sint,sint,cost), 2) %*% t(cbind(u,v)))
}

## Get the coordinates of one axes
get.one.axis <- function(data, axis = 1, level = 0.95, dimensions) {

    # The magic: https://stackoverflow.com/questions/40300217/obtain-vertices-of-the-ellipse-on-an-ellipse-covariance-plot-created-by-care/40316331#40316331

    ## VCVing the matrix
    if(!is(data, "list")) {
        data <- list(VCV = data)
    } else {
        if(is.null(data$VCV)) {
            data$VCV <- data
        }
    }
    ## adding a loc
    if(is.null(data$loc)) {
        data$loc <- rep(0, nrow(data$VCV))
    }

    ## Select the right dimensions
    data$VCV <- data$VCV[dimensions, dimensions, drop = FALSE]

    ## Get the data dimensionality
    dims <- length(diag(data$VCV))

    ## Create the unit hypersphere (a hypersphere of radius 1) for the scaling
    unit_hypersphere1 <- unit_hypersphere2 <- matrix(0, ncol = dims, nrow = dims)
    ## The "front" (e.g. "top", "right") units
    diag(unit_hypersphere1) <- 1
    ## The "back" (e.g. "bottom", "left") units
    diag(unit_hypersphere2) <- -1
    unit_hypersphere <- rbind(unit_hypersphere1, unit_hypersphere2)
    ## Scale the hypersphere (where level is the confidence interval)
    unit_hypersphere <- unit_hypersphere * sqrt(qchisq(level, 2))

    ## Do the eigen decomposition (symmetric - faster)
    eigen_decomp <- eigen(data$VCV, symmetric = TRUE)

    ## Re-scaling the unit hypersphere
    scaled_edges <- unit_hypersphere * rep(sqrt(abs(eigen_decomp$values)), each = dims*2)
    ## Rotating the edges coordinates
    edges <- tcrossprod(scaled_edges, eigen_decomp$vectors)

    ## Move the matrix around
    edges <- edges + rep(data$loc[dimensions, drop = FALSE], each = dims*2)

    ## Get the edges coordinates
    return(edges[c(axis, axis+dims), , drop = FALSE])
}