# https://stackoverflow.com/questions/36798301/r-compute-cross-product-of-vectors-physics/36802067#36802067

xprod <- function(...) {
  args <- list(...)

  # Check for valid arguments

  if (length(args) == 0) {
    stop("No data supplied")
  }
  len <- unique(sapply(args, FUN=length))
  if (length(len) > 1) {
    stop("All vectors must be the same length")
  }
  if (len != length(args) + 1) {
    stop("Must supply N-1 vectors of length N")
  }

  # Compute generalized cross product by taking the determinant of sub-matricies

  m <- do.call(rbind, args)
  sapply(seq(len),
         FUN=function(i) {
           det(m[,-i,drop=FALSE]) * (-1)^(i+1)
         })
}
