## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE, eval=FALSE-----------------------
#  library(colorRamps)
#  library(dplyr)
#  library(plotly)
#  library(purrr)
#  library(mgx2r)
#  
#  library(cellviz3d)

## ---- warning = FALSE, message = FALSE, eval=FALSE-----------------------
#  ply.dir <- system.file("extdata", "full/normalMesh/", package = "mgx2r")
#  
#  mesh.all <- map(list.files(ply.dir, recursive = TRUE, full.names = TRUE),
#                  ~ read_mgxPly(file = .x, ShowSpecimen = FALSE))
#  
#  
#  graph.dir <- system.file("extdata", "full/cellGraph/", package = "mgx2r")
#  
#  cellGraph.all <- map(list.files(graph.dir, recursive = TRUE, full.names = TRUE), ~read_mgxCellGraph(fileCellGraph = .x, header_max = 30))

## ---- warning = FALSE, message = FALSE, eval=FALSE-----------------------
#  meshColors.all <- list(NULL, NULL, NULL, NULL, NULL)
#  
#  plotlyMesh_all(meshExample = mesh.all,
#                         graphExample = cellGraph.all,
#                         meshColors = meshColors.all,
#                         display = 'heatmap')
#  

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics(system.file("img", "full/timeserie800ms.gif", package = "mgx2r"))

