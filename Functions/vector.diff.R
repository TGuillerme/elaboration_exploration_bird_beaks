#' @title vector differences
#'
#' @description Comparing two vectors
#'
#' @param vector the vector to compare to the base (also a vector)
#' @param base the base vector
#' @param return what to return: c("angle", "projection", "rejection")
#' @param scale whether to scale the base vector to be the unit (TRUE)
#' 
#' @details
#' This is a modification from dispRity::projections to align the vector with the base and output all the three elements (angle, projection and rejection) rather than just one.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

# DEBUG
# vector <- level_centred_major_axes$cladegulls[[1]]
# base <- level_centred_major_axes$phylo[[1]]
# vector.diff(vector, base)

vector.diff <- function(vector, base, return = c("angle", "projection", "rejection"), scaled = TRUE) {

    # warning("DEBUG: visual test vector.diff")
    # lims <- range(c(base, vector))
    # plot(NULL, type = "l", xlim = lims, ylim = lims)

    ## Translating the inputs to correspond to dispRity::projections
    matrix <- vector
    point1 <- base[1,]
    point2 <- base[2,]

    # arrows(x0 = matrix[1, 1],
    #        y0 = matrix[1, 2],
    #        x1 = matrix[2, 1],
    #        y1 = matrix[2, 2],
    #        col = "orange", length = 0.1)

    # arrows(x0 = point1[1],
    #        y0 = point1[2],
    #        x1 = point2[1],
    #        y1 = point2[2],
    #        col = "black", length = 0.1)

    ## Get the translation vector to point1
    translation_vector <- point1 - matrix[1,]

    # arrows(x0 = 0,
    #        y0 = 0,
    #        x1 = translation_vector[1],
    #        y1 = translation_vector[2],
    #        col = "red", length = 0.1)

    ## Align the matrix with point1
    matrix <- rbind(matrix[1,] + translation_vector,
                    matrix[2,] + translation_vector)

    # arrows(x0 = matrix[1,1],
    #        y0 = matrix[1,2],
    #        x1 = matrix[2,1],
    #        y1 = matrix[2,2],
    #        col = "blue", length = 0.1)

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
        base_vector <- space[-c(1:nrow(matrix)), ]
    }

    ## Scale the space
    if(scaled) {
        ## The scaled space
        space <- space/dist(space[-c(1:nrow(matrix)),])
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
    base_vector <- space[-c(1:nrow(matrix)),]

    ## Project the vectors
    projections <- t(apply(matrix, 1, geometry::dot, y = base_vector[2,], d = 2))

    ## Prepare the output
    output <- numeric()

    ## Get the angle
    if("angle" %in% return) {
        output["angle"] <- t(t(apply(matrix, 1, vector.angle, base_vector[2,])))[2]
    }

    ## Get the projection distance
    if("projection" %in% return) {
        output["projection"] <- projections[,1][2]
    }

    ## Get the rejection distance
    if("rejection" %in% return) {
        output["rejection"] <- apply(matrix - projections, 1, function(row) sqrt(sum(row^2)))[2]
    }

    return(output)
}
    


## Rotate a matrix along one axis (y)
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
## Angle between two vectors
vector.angle <- function(v1, v2, degree = TRUE) {
    angle <- acos(geometry::dot(v1, v2, d = 1) / (sqrt(sum(v1^2))*sqrt(sum(v2^2))))
    if(degree) {
        return(angle *180/pi)
    } else {
        angle
    }
}