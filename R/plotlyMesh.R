#' A layout for plotly graphs
#'
#' This function provides a basic layout for plotly graphs used to display MGX meshes.
#' @param meshExample mesh loaded with the modified_read.ply function
#' @param meshColors to display colored on the mesh
#' @param meshCellcenter to display the center of (biological) cells on the mesh
#' @param defaultColor hex code of color to give by default to the mesh as well as percentage of opacity
#'
#' @importFrom colorRamps matlab.like
#' @importFrom dplyr pull
#' @importFrom plotly add_trace layout plot_ly
#' @importFrom purrr map2
#' @keywords
#' @export
#' @examples
#' @return plotly graph

plotlyMesh <- function(meshExample,
                       meshColors = NULL,
                       meshCellcenter = NULL,
                       defaultColor = list("#CCCCFF", 0.2)){
  makeColorScale <- FALSE
  if (!is.null(nrow(meshColors)) && ncol(meshColors)>1){
    color <- NULL
    for (i in 1:ncol(meshColors)){
      color[i] <-  setdiff(unique(meshColors[,i]), "#000000") # here I remove black vertices
    } # to be more general, remove any color shared by two vertices
    opacity <- 1
  }else{
    if (is.null(meshColors)){
      color <- rep(defaultColor[[1]], ncol(myMesh$it)) # "#00FFFF"
      opacity <- defaultColor[[2]]
    }else{
      makeColorScale <- TRUE
      colorCut <- cut(pull(meshColors), 15,
                      labels =  matlab.like(15)
      )
      color <- as.character(colorCut)
      opacity <- 1
    }
  }

  trace2 <- list(type="mesh3d",
                 x = meshExample$vb[1,],
                 y = meshExample$vb[2,],
                 z = meshExample$vb[3,],
                 i = meshExample$it[1,]-1, # NB indices start at 0
                 j = meshExample$it[2,]-1,
                 k = meshExample$it[3,]-1,
                 facecolor = color, #one color per triangle (not per biological cell)
                 opacity = opacity
  )
  #facecolor: one color per triangle (e.g. length(facecolor) == length(i))

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
                                colorscale = map2(.x = seq(0,1, len=15),
                                                         .y = matlab.like(15),
                                                         ~ list(.x, .y)),
                               # list needs to be between 0 and 1
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
  }

  layout <- list(
    scene = list(
      xaxis = list(
        backgroundcolor = "rgb(230,230,230)",
        gridcolor = "rgb(255,255,255)",
        showbackground = TRUE,
        zerolinecolor = "rgb(255,255,255)"
      ),
      yaxis = list(
        backgroundcolor = "rgb(230,230,230)",
        gridcolor = "rgb(255,255,255)",
        showbackground = TRUE,
        zerolinecolor = "rgb(255,255,255)"
      ),
      zaxis = list(
        backgroundcolor = "rgb(230,230,230)",
        gridcolor = "rgb(255,255,255)",
        showbackground = TRUE,
        zerolinecolor = "rgb(255,255,255)"
      )
    ),
    title = "My mesh",
    xaxis = list(title = "m[, 1]"),
    yaxis = list(title = "m[, 2]")
  )

  p <- plot_ly()

  p <- add_trace(p, x = trace2$x, y=trace2$y, z=trace2$z,
                 facecolor=trace2$facecolor,
                 i=trace2$i, j=trace2$j, k=trace2$k, type=trace2$type,
                 opacity=trace2$opacity,
                 hoverinfo = 'none',
                 showlegend = FALSE)

  if (makeColorScale){
    p <- add_trace(p,
                   x = meshCellcenter$x,
                   y = meshCellcenter$y,
                   z = meshCellcenter$z,
                   text = as.character(meshCellcenter$label),
                   hoverinfo = 'text',
                   marker = trace4$marker,
                   mode = trace4$mode,
                   opacity = trace4$opacity,
                   type = trace4$type,
                   showlegend = FALSE
                   )
  }
  if (!is.null(meshCellcenter)){ # show cell center
    p <- add_trace(p, x = meshCellcenter$x,
                   y = meshCellcenter$y,
                   z = meshCellcenter$z,
                   text = as.character(meshCellcenter$label),
                   hoverinfo = 'text',
                   type = "scatter3d",
                   mode = "markers",
                   marker=list(color = 'rgb(255, 255, 255)',
                               size = 10),
                   opacity = 0,
                   showlegend = FALSE
    )
  }
  p <- layout(p, scene=layout$scene, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)

  p
}
