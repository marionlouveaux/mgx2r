## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----lib, warning = FALSE, message = FALSE, eval=FALSE-------------------
#  library(colorRamps)
#  library(mgx2r)
#  library(magrittr)
#  library(dplyr)
#  library(plotly)
#  library(glue)
#  library(RColorBrewer)
#  
#  library(cellviz3d)

## ----myData, eval=FALSE--------------------------------------------------
#  # Choose one of the two datasets. Generating plots with the full dataset might be slow.
#  
#  ### Full datataset
#  filePly <- system.file("extdata", "full/normalMesh/2013-02-12_LTi6B_dis_A_T0_cells_minW1_normalMesh.ply", package = "mgx2r")
#  
#  fileCellGraph <- system.file("extdata",  "full/cellGraph/2013-02-12_LTi6B_dis_A_T0_cells_minW1_cellGraph.ply", package = "mgx2r")
#  
#  
#  ### Cropped dataset
#  filePly <- system.file("extdata", "cropped/normalMesh/2013-02-12_LTi6B_dis_A_T12h_cells_minW1_cropped_normalMesh.ply", package = "mgx2r")
#  
#  fileCellGraph <- system.file("extdata",  "cropped/cellGraph/2013-02-12_LTi6B_dis_A_T12h_cells_minW1_cropped_cellGraph.ply", package = "mgx2r")
#  

## ----readPly, warning = FALSE, message = FALSE, eval=FALSE---------------
#  myMesh <- read_mgxPly(file = filePly, ShowSpecimen = FALSE, addNormals = TRUE,
#                                 MatCol= 1, header_max = 30,
#                                 my_colors = c("#800000", "#FF0000", "#808000", "#FFFF00",
#                                               "#008000", "#00FF00", "#008080", "#00FFFF",
#                                               "#000080", "#0000FF", "#800080", "#FF00FF"))

## ---- eval=FALSE---------------------------------------------------------
#  rgl::shade3d(myMesh)

## ----modifColor, eval=FALSE----------------------------------------------
#  myMesh$material$color <- myMesh$allColors$Col_signal
#  rgl::shade3d(myMesh)

## ----readCellGraph, eval=FALSE-------------------------------------------
#  myCellGraph <- read_mgxCellGraph(fileCellGraph = fileCellGraph_big, header_max = 30)

## ---- eval=FALSE---------------------------------------------------------
#  meshCellcenter <- myCellGraph$vertices[,c("label","x", "y", "z")]
#  
#  # An other way to define the cell centers without using the cell graph would be:
#  vertexCellcenter <- purrr::map(1:ncol(myMesh$allColors$Col_label), ~
#    myMesh$it[ which(myMesh$allColors$Col_label[,.x] == names(which(table(myMesh$allColors$Col_label[,.x]) == 1))), .x ]
#  )

## ----map3D, eval=FALSE---------------------------------------------------
#  p1 <- plotlyMesh(meshExample = myMesh,
#                   meshColors = myMesh$allColors$Col_label,
#                   meshCellcenter = meshCellcenter) %>%
#    layout(scene =
#             list(
#               aspectmode = "data"
#             ))
#  
#  p1

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics(system.file("img", "full/p1labels.png", package = "mgx2r"))

## ----map3D2, eval=FALSE--------------------------------------------------
#  plotlyMesh(meshExample = myMesh,
#             meshColors = NULL,
#             meshCellcenter = meshCellcenter)%>%
#    layout(scene =
#             list(
#               aspectmode = "data"
#             ))

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics(system.file("img", "full/p1withoutlabels.png", package = "mgx2r"))

## ----heatmap3D, warning = FALSE, message = FALSE, eval=FALSE-------------
#  p2 <- plotlyMesh(meshExample = myMesh,
#             meshColors = left_join(myMesh$it_label, myCellGraph$vertices) %>%
#               select(., GeometryArea),
#             meshCellcenter =  meshCellcenter
#  ) %>%
#    layout(scene =
#             list(
#               aspectmode = "data"
#             ))
#  p2

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics(system.file("img", "full/p2heatmap.png", package = "mgx2r"))

## ----tensors, warning = FALSE, message = FALSE, eval=FALSE---------------
#  meshPlotly <- plotlyMesh(meshExample = myMesh,
#                  meshColors = NULL,
#                  meshCellcenter = meshCellcenter) %>%
#    layout(scene =
#             list(
#               aspectmode = "data"
#             ))
#  
#  show_tensors(p = meshPlotly,
#               cellGraph = myCellGraph,
#               tensor_name = "CellAxiscurv10microns",
#               scale = 50)

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics(system.file("img", "full/tensors.png", package = "mgx2r"))

## ----cellContours, eval=FALSE--------------------------------------------
#  meshPlotly <- plotlyMesh(meshExample = myMesh,
#             meshColors = NULL) %>%
#    layout(scene =
#             list(
#               aspectmode = "data"
#             ))
#  
#  show_cellcontour(p = meshPlotly, mesh = myMesh)
#  

## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics(system.file("img", "full/contour.png", package = "mgx2r"))

