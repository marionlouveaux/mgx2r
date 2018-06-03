#' Add cell contour visualisation to a plotly graph of type mesh3d
#'
#' @param p a plotly graph created with plotlyMesh()
#' @param mesh the mesh used to create this plotly graph
#'
#' @importFrom plotly add_trace layout
#' @importFrom purrr map
#' @return
#' @export
#'
#' @examples
#' show_cellcontour

show_cellcontour <- function(p, mesh) {

  edgesCoords <- map(1:ncol(myMesh$allColors$Col_label), ~
                              myMesh$vb[,myMesh$it[ which(myMesh$allColors$Col_label[,.x] != names(which(table(myMesh$allColors$Col_label[,.x])==1))), .x]]
  )

  for (i in 1:length(unique(edgesCoords))){
    edge_tmp <- unique(edgesCoords)[[i]]
    xcoord <- edge_tmp[1,]
    ycoord <- edge_tmp[2,]
    zcoord <- edge_tmp[3,]
    p <- add_trace(p, x = xcoord, y = ycoord, z = zcoord,
                   line = list(color = 'rgb(255, 255, 255)', width = 6), mode = "lines",
                   type = "scatter3d", hoverinfo = 'none') %>%
      layout(showlegend = FALSE)
  }
  p
}
