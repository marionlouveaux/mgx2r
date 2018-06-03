#' A reading .ply Function
#'
#' This function is a modified version of read.ply function from geomorph R package to specifically read MGX cell graph ply files.
#' @param fileCellGraph .ply Cell Graph file.
#' @param header_max Number of lines expected in header. Must be equal or greater to the actual number of lines in the header. Default to 30.
#' @keywords .ply, read
#' @export
#' @examples
#' @import dplyr
#' @return Object of type mesh3D
#' modified_read.ply()


modified_read.cellGraph <- function (fileCellGraph, header_max = 30)
{

  #Read only chunk corresponding to header
  plyhead <- scan(file = fileCellGraph, what = "char", sep = "\n", strip.white = TRUE,
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
  nfaces <- as.numeric(yline[grep(c("edge"), yline) + 1]) #number of (triangular) faces

  headerend <- grep(c("end_header"), plyhead) #looking for the end of the header


  ppty_vertices <- plyhead[(nline_vertices+1) : (nline_faces-1)]

  for (i in grep(".*/.*[[:blank:]]", ppty_vertices)){
    testSlash <- gregexpr("/", ppty_vertices[i])
    testBlank <- gregexpr("[[:blank:]]", ppty_vertices[i], perl = TRUE)
    for (b in testBlank[[1]]){
      if (b > max(testSlash[[1]])){
        substr(ppty_vertices[i], start = b, stop = b) <- "_" # only the blanks after the last slash
      }
    }
  }

  ppty_vertices <-  gsub("(?!-)[[:punct:]]", replacement = "", perl = TRUE, ppty_vertices)


  plymat_vertices <- ucharHelp(ppty = ppty_vertices,
                               file = fileCellGraph,
                               toSkip = headerend,
                               Nlines = nvertices)

  plymat_faces <- ucharHelp(ppty = plyhead[(nline_faces+1):(headerend-1)],
                           file = fileCellGraph,
                           toSkip = headerend + nvertices,
                           Nlines = nfaces)

  list(vertices = plymat_vertices, edges = plymat_faces)
}





