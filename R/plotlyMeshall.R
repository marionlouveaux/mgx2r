#' A layout for plotly graphs with a time slider
#'
#' This function provides a basic layout for plotly graphs with a time sliders.
#' It can be used to display MGX meshes of a time serie.
#'
#' @inheritParams create_alltraces
#'
#' @importFrom plotly add_trace layout plot_ly
#' @importFrom purrr pmap
#' @return plotly graph
#' @export
#'
#' @examples

plotlyMesh_all <- function(meshExample,
                           graphExample,
                           meshColors,
                           display,
                           defaultColor = list("#CCCCFF", 0.2),
                           heatmapParam = "GeometryArea") {

  all.trace <- pmap(list(meshExample = meshExample,
                         graphExample = graphExample,
                         meshColors = meshColors,
                         display = display),`create_alltraces`)

  all.trace[[1]][[1]]$visible = TRUE

  layout <- list(
    scene = list(
      xaxis = list(
        backgroundcolor = "rgb(230,230,230)",
        gridcolor = "rgb(255,255,255)",
        showbackground = TRUE,
        zerolinecolor = "rgb(255,255,255"
      ),
      yaxis = list(
        backgroundcolor = "rgb(230,230,230)",
        gridcolor = "rgb(255,255,255)",
        showbackground = TRUE,
        zerolinecolor = "rgb(255,255,255"
      ),
      zaxis = list(
        backgroundcolor = "rgb(230,230,230)",
        gridcolor = "rgb(255,255,255)",
        showbackground = TRUE,
        zerolinecolor = "rgb(255,255,255"
      )
    ),
    title = "My mesh",
    xaxis = list(title = "m[, 1]"),
    yaxis = list(title = "m[, 2]")
  )

  p <- plot_ly()

  steps <- list()
  for (t in 1:length(all.trace)){
    traceCount <- 0
    p <- add_trace(p, x=all.trace[[t]][[1]]$x,
                   y=all.trace[[t]][[1]]$y,
                   z=all.trace[[t]][[1]]$z,
                   facecolor=all.trace[[t]][[1]]$facecolor,
                   i=all.trace[[t]][[1]]$i,
                   j=all.trace[[t]][[1]]$j,
                   k=all.trace[[t]][[1]]$k,
                   type=all.trace[[t]][[1]]$type,
                   visible = all.trace[[t]][[1]]$visible,
                   hoverinfo = 'none')
    traceCount <- 1

    if (!is.null(all.trace[[t]][[2]])){
      traceCount <- traceCount+1
      p <- add_trace(p,
                     x = all.trace[[t]][[3]]$x,
                     y = all.trace[[t]][[3]]$y,
                     z = all.trace[[t]][[3]]$z,
                     text = as.character(all.trace[[t]][[3]]$label),
                     hoverinfo = 'text',
                     marker = all.trace[[t]][[2]]$marker,
                     mode = all.trace[[t]][[2]]$mode,
                     opacity = all.trace[[t]][[2]]$opacity,
                     type = all.trace[[t]][[2]]$type,
                     showlegend = FALSE#,
                     # hoverinfo = 'none'
      )
    }
    if (!is.null(all.trace[[t]][[3]])){ # show cell center
      traceCount <- traceCount+1
      p <- add_trace(p, x = all.trace[[t]][[3]]$x,
                     y = all.trace[[t]][[3]]$y,
                     z = all.trace[[t]][[3]]$z,
                     text = as.character(all.trace[[t]][[3]]$label),
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

    if (traceCount == 1){
      step <- list(args = list('visible', rep(FALSE, length(all.trace))),
                   method = 'restyle')
      step$args[[2]][t] = TRUE
    }else if (traceCount == 2){ # slider takes in account all existing traces
      step <- list(args = list('visible', rep(FALSE, length(all.trace)*2)),
                   method = 'restyle')
      k <- t + (t-1)
      step$args[[2]][k] = TRUE
      step$args[[2]][k+1] = TRUE
    }else if (traceCount == 3){
      step <- list(args = list('visible', rep(FALSE, length(all.trace)*3)),
                   method = 'restyle')
      increment <- seq(0, (length(all.trace)-1)*2, 2)
      k <- t + increment[t]
      step$args[[2]][k] = TRUE
      step$args[[2]][k+1] = TRUE
      step$args[[2]][k+2] = TRUE
    }

    steps[[t]] = step
  }


  p <- p %>%
    layout(sliders = list(list(active = 3,
                               currentvalue = list(prefix = "Frame: "),
                               steps = steps)))
  p
}

