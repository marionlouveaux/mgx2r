#' Find coordinates of a tensor
#'
#' @param cellcenter x, y, z coordinates
#' @param tipUnit x, y, z coordinates
#' @param d norm of the vector
#' @param scale shorten or lengthen the line for visualisation
#'
#' @return
#' @export
#'
#' @examples
#' find_coords()

find_coords <- function(cellcenter, tipUnit, d, scale = 1) {
  d <- d*scale
  # Distance
  tip1 <- d / (sqrt(sum(tipUnit^2))) * tipUnit + cellcenter
  tip2 <- (-d / (sqrt(sum(tipUnit^2))) * tipUnit) + cellcenter
  tibble(x1 = tip1[1], y1 = tip1[2], z1 = tip1[3],
         x2 = tip2[1], y2 = tip2[2], z2 = tip2[3])
}
