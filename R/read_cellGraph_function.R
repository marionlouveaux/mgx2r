#' A reading .ply Function
#'
#' This function is a modified version of read.ply function from geomorph R package to specifically read MGX ply files.
#' @param cellGraphfile .ply Cell Graph file.
#' @param header_max Number of lines expected in header. Must be equal or greater to the actual number of lines in the header. Default to 30.
#' @keywords .ply, read
#' @export
#' @examples
#' @import dplyr
#' @return Object of type mesh3D
#' modified_read.ply()



modified_read.cellGraph <- function (cellGraphfile, header_max = 30)
  {

  #Read only chunk corresponding to header
  plyhead <- scan(file = cellGraphfile, what = "char", sep = "\n", strip.white = TRUE,
                  quiet = TRUE, comment.char = "", multi.line = FALSE, n = header_max)

  is.ply <- grep("ply", plyhead)
  if ((length(is.ply) == 0))
    stop("File is not a PLY file")
  format <- unlist(strsplit(grep(c("format "), plyhead, value = TRUE),
                            " "))
  if (format[2] != "ascii")
    stop("PLY file is not ASCII format: ", "format = ", format[2:length(format)])


  # Extract information from reader (about vertices and faces)
  nline_vertices <- grep(c("element vertex"), plyhead)
  xline <- unlist(strsplit( plyhead[nline_vertices] , " "))
  nvertices <- as.numeric(xline[grep(c("vertex"), xline) + 1]) #number of vertices

  nline_faces <- grep(c("element edge"), plyhead)
  yline <- unlist(strsplit( plyhead[nline_faces] , " "))
  nfaces <- as.numeric(yline[grep(c("face"), yline) + 1]) #number of (triangular) faces

  headerend <- grep(c("end_header"), plyhead) #looking for the end of the header

  ppty_vertices <- plyhead[(nline_vertices+1) : (nline_faces-1)]

  # x, y, z coordinates of the vertices
  x <- grep(c(" x"), ppty_vertices)
  y <- grep(c(" y"), ppty_vertices)
  z <- grep(c(" z"), ppty_vertices)
  # other properties of the vertices
  label <- grep(c("label"), ppty_vertices)
  Area <- grep(c("/Geometry/Area"), ppty_vertices)
  Asp_ratio_geom <- grep(c("/Geometry/Aspect Ratio"), ppty_vertices)
  Neighbors <- grep(c("Geometry/Neighbors"), ppty_vertices)
  Perimeter <- grep(c("/Geometry/Perimeter"), ppty_vertices)
  Circularity <- grep(c("/Lobyness/Circularity"), ppty_vertices)
  Signal <- grep(c("/Geometry/Signal"), ppty_vertices)
  ConvexityArea <- grep(c("/Lobyness/Convexity Area"), ppty_vertices)
  ConvexityPerim <- grep(c("/Lobyness/Convexity Perimeter"), ppty_vertices)
  LES <- grep(c("/Lobyness/Largest Empty Space"), ppty_vertices)
  Pavement <- grep(c("/Lobyness/Visibility Pavement"), ppty_vertices)
  Asp_ratio <- grep(c("/AspectRatio"), ppty_vertices)


  nface <- as.numeric(yline[grep(c("face"), yline) + 1]) #number of (triangular) faces

  plymat_vertex <-  scan(file = cellGraphfile, what = list(label = numeric(), x = numeric(), y = numeric(),
                                                  z = numeric(), Area = numeric(), Asp_ratio_geom = numeric(),
                                                  Neighbors = numeric(), Perimeter = numeric(), Signal = numeric(),
                                                  Circularity = numeric(), ConvexityArea = numeric(), ConvexityPerim = numeric(),
                                                  LES = numeric(), Pavement = numeric(), AR0 = numeric(),
                                                  AR1 = numeric(), AR2 = numeric(), AR3 = numeric(),
                                                  AR4 = numeric(), AR5 = numeric(), AR6 = numeric(),
                                                  AR7 = numeric(), AR8 = numeric(), AR9 = numeric() ),
                         #sep = "\n", #strip.white = TRUE,
                         quiet = TRUE,
                         skip = headerend, nmax = nvertices)


  plymat_face <- scan(file = cellGraphfile,
                      #sep = "\n", strip.white = TRUE,
                      what = list(cell1 = integer(), cell2 = integer()),
                      quiet = TRUE, comment.char = "", multi.line = FALSE,
                      skip = headerend + nvertices, nmax = nfaces)

}





