pseudo_inverse <- function(X) {
    MASS::ginv(t(X) %*% X) %*% t(X)
}

iprod <- function(x, y) {
    as.vector(x %*% y)
}

vnorm <- function(x) {
    as.vector(x %*% x)
}

uvec <- function(x) {
    x / sqrt(vnorm(x))
}