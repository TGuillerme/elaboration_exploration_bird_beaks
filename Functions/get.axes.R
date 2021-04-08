#' @title Get ellipses axes
#'
#' @description Get the coordinates of any axes from an nD ellipse
#'
#' @param covar A covariance matrix describing the ellipse
#' @param axis Which axis to get the coordinates from (default is 1 for the major axis).
#' @param centre The centre of the ellipse (by default this is 0).
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

get.axes <- function(covar, axis = 1, centre = 0) {

    ## Get the right size for the centre
    if((diff <- dim(covar)[1]) - length(centre) != 0) {
        centre <- c(centre, rep(centre[1], abs(diff)-1))
    }

    ## Get the major axis
    lines(eigen(covar[c(1,2), c(1,2)])$vectors)

    eigen.decomp <- eigen(covar[c(1,2), c(1,2)])


    test <- eigen.decomp$vectors %*% sqrt(eigen.decomp$values)

    lines(eigen.decomp$vectors * eigen.decomp$values)

    # https://stackoverflow.com/questions/40300217/obtain-vertices-of-the-ellipse-on-an-ellipse-covariance-plot-created-by-care

    ## Solution:
        # A <- matrix(c(20.43, -8.59,-8.59, 24.03), nrow = 2)

        # E <- eigen(A, symmetric = TRUE)  ## symmetric eigen decomposition
        # U <- E[[2]]  ## eigen vectors, i.e., rotation matrix
        # D <- sqrt(E[[1]])  ## root eigen values, i.e., scaling factor

        # r <- 1.44  ## radius of original circle
        # Z <- rbind(c(r, 0), c(0, r), c(-r, 0), c(0, -r))  ## original vertices on major / minor axes
        # Z <- tcrossprod(Z * rep(D, each = 4), U)  ## transformed vertices on major / minor axes

        # #          [,1]      [,2]
        # #[1,] -5.055136  6.224212
        # #[2,] -4.099908 -3.329834
        # #[3,]  5.055136 -6.224212
        # #[4,]  4.099908  3.329834

        # C0 <- c(-0.05, 0.09)  ## new centre
        # Z <- Z + rep(C0, each = 4)  ## shift to new centre



    ## Test from derived.stats2.R
    mat1 <- df1_mat_1
    mat2 <- df1_mat_2
    krzanowski.test(mat1, mat2, vecsA=1, vecsB=1)$angles 
    # 4.09
    eigen.vect * sqrt(eigenve.value)cos(t)

    eigen_decomp <- eigen(mat1)


    mat2


    return()
}