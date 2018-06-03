#' mgx2r-package
#'
#' Import mesh and cell graph files from MorphoGraphX software as tidy dataframes.
#' Propose visualization solutions for mesh and cell graph files from MorphoGraphX with Plotly and Shiny.
#'
#' @name mgx2r-package
#' @aliases mgx2r mgx2r-package
#' @docType package
#' @author Marion <marion.louveaux@@gmail.com>
#'
#' @section Import from MorphoGraphX (MGX)
#'
#' The function read_mgxPly imports meshes created with MorphoGraphX and saved as .ply as 3D mesh objects.
#' The function read_mgxCellGraph imports cell graphs created with MorphoGraphX and saved as .ply as list of dataframes.
#'
#' @section Visualisation and data exploration
#'
#' Examples of simple data exploration of variables stored in the cell graphs are provided in the vignettes.
#'
#' @section Export to MorphoGraphX (MGX)
#'
#' The function mesh2ply exports a 3D mesh object from R to a .ply file that can be opened in MorphoGraphX (MGX).
NULL
