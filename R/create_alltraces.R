#' Create all traces for a time serie
#'
#' @param meshExample One or several meshes.
#' @param graphExample One or several cell graphs.
#' @param meshColors Colors for each mesh.
#' @param display Display type. 'labels', 'heatmap' or 'none'.
#' @param defaultColor Mesh color if 'none' display type is chosen (HEX color and alpha value between 0 and 1).
#' @param heatmapParam Variable (from cell graph) to display if 'heatmap' display type is selected.
#'
#' @return
#'
#' @examples

create_alltraces <- function(meshExample,
                             graphExample,
                             meshColors,
                             display,
                             defaultColor = list("#CCCCFF", 0.2),
                             heatmapParam = "GeometryArea"
){
  makeColorScale <- FALSE
  if (display == 'labels'){
    opacity <- 1
    if (is.null(meshColors)){
      meshColors <- meshExample$allColors$Col_label
    }
    if (ncol(meshColors)>1){
      color <- NULL
      for (i in 1:ncol(meshColors)){
        color[i] <-  setdiff(unique(meshColors[,i]), "#000000") #I remove black vertices
      } # to be more general, remove any color shared by two vertices
    }else{
      color <- meshColors
    }
  } else if (display == 'none'){
    color <- rep(defaultColor[[1]], ncol(meshExample$it)) # "#00FFFF"
    opacity <- defaultColor[[2]]
  } else if (display == 'heatmap'){
    if (is.numeric(meshColors) == TRUE && is.null(heatmapParam) == TRUE ){
      makeColorScale <- TRUE
      colorCut <- cut(pull(meshColors), 15,
                      labels =  matlab.like(15)
      )
      color <- as.character(colorCut)
      opacity <- 1
    }else if (is.null(heatmapParam) == FALSE){
      meshColors <- left_join(meshExample$it_label, graphExample$vertices) %>%
        select_(., heatmapParam)
      makeColorScale <- TRUE
      colorCut <- cut(pull(meshColors), 15,
                      labels =  matlab.like(15)
      )
      color <- as.character(colorCut)
      opacity <- 1
    }else{
      warning("Provide continous variable for heatmap or valid heatmap parameter.")
    }
  }


  trace2 <- list(type="mesh3d",
                 x = meshExample$vb[1,],
                 y = meshExample$vb[2,],
                 z = meshExample$vb[3,],
                 i = meshExample$it[1,]-1, # NB indices start at 0
                 j = meshExample$it[2,]-1,
                 k = meshExample$it[3,]-1,
                 facecolor = color,
                 opacity = opacity,
                 visible = FALSE
  )


  if (makeColorScale){

    trace4 <- list(x = c(100,1,200),
                   y = c(200,1,1),
                   z = c(1,500,3),
                   marker = list(
                     autocolorscale = FALSE,
                     cmax = round(max(meshColors)),#2.5,
                     cmin = round(min(meshColors)),#0,
                     color = c("#0000aa", "#99ff99", "#aa0000"),
                     colorbar = list(
                       x = 1.2,
                       y = 0.5,
                       len = 1,
                       thickness = 15,
                       tickfont = list(size = 12),
                       titlefont = list(size = 20)
                     ),
                     colorscale = purrr::map2(.x = seq(0,1, len=15),
                                              .y = matlab.like(15),
                                              ~ list(.x, .y)),
                     line = list(width = 0),
                     opacity = 0.1,
                     showscale = TRUE,
                     size = 20,
                     symbol = "circle"
                   ),
                   mode = "markers",
                   opacity = 0,
                   type = "scatter3d"
    )
  }else{
    trace4 <- NULL
  }

  meshCellcenter <- graphExample$vertices[,c("label","x", "y", "z")]

  list(trace2, trace4, meshCellcenter)
  #facecolor: one color per triangle (e.g. length(facecolor) == length(i))
}
