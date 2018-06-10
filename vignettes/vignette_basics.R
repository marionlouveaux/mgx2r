## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----lib, warning = FALSE, message = FALSE, eval=TRUE--------------------
library(colorRamps)
library(mgx2r)
library(magrittr)
library(dplyr)
library(plotly)
library(glue)
library(RColorBrewer)

## ----libdev, warning = FALSE, message = FALSE, eval=FALSE----------------
#  # devtools::install_github("marionlouveaux/cellviz3d")
#  library(cellviz3d)

## ----myData, eval=TRUE---------------------------------------------------
### Full datataset
filePly <- system.file("extdata", "full/mesh/mesh_meristem_full_T0.ply", package = "mgx2r")

fileCellGraph <- system.file("extdata",  "full/cellGraph/cellGraph_meristem_full_T0.ply", package = "mgx2r")


## ---- eval=FALSE---------------------------------------------------------
#  ### Cropped dataset
#  filePly <- system.file("extdata", "cropped/mesh/mesh_meristem_cropped_T0.ply", package = "mgx2r")
#  
#  fileCellGraph <- system.file("extdata",  "cropped/cellGraph/cellGraph_meristem_cropped_T0.ply", package = "mgx2r")
#  

## ----readPly, warning = FALSE, message = FALSE, eval=TRUE----------------
mgx_palette <- c("#800000", "#FF0000", "#808000", "#FFFF00",
                "#008000", "#00FF00", "#008080", "#00FFFF",
                "#000080", "#0000FF", "#800080", "#FF00FF")

myMesh <- read_mgxPly(
  file = filePly, ShowSpecimen = FALSE, addNormals = TRUE,
  MatCol= 1, header_max = 30,
  my_colors = mgx_palette)
str(myMesh, max.level = 1)

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  saveRDS(myMesh, file = "mesh_meristem_full_T0.rds")

## ---- eval=FALSE---------------------------------------------------------
#  rgl::shade3d(myMesh)

## ----modifColor, eval=FALSE----------------------------------------------
#  myMesh$material$color <- myMesh$allColors$Col_signal
#  rgl::shade3d(myMesh)

## ----readCellGraph, eval=TRUE--------------------------------------------
myCellGraph <- read_mgxCellGraph(fileCellGraph = fileCellGraph, header_max = 30)
myCellGraph

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  saveRDS(myCellGraph, file = "cellGraph_meristem_full_T0.rds")

## ---- eval=FALSE---------------------------------------------------------
#  meshCellcenter <- myCellGraph$vertices[,c("label","x", "y", "z")]
#  
#  # An other way to define the cell centers without using the cell graph would be: ## issue with the code below
#  # vertexCellcenter <- purrr::map(1:ncol(myMesh$allColors$Col_label), ~
#  #   myMesh$it[ which(myMesh$allColors$Col_label[,.x] == names(which(table(myMesh$allColors$Col_label[,.x]) == 1))), .x ]
#  # )

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

