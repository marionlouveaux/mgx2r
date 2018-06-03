## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----lib, warning = FALSE, message = FALSE-------------------------------
library(colorRamps)
library(mgx2r)
library(magrittr)
library(dplyr)
library(plotly)
library(glue)
library(RColorBrewer)

## ----myData--------------------------------------------------------------
filePly <- system.file("extdata", "normalMesh/2013-02-12_LTi6B_dis_A_T0_cells_minW1_normalMesh.ply", package = "mgx2r")

fileCellGraph <- system.file("extdata",  "cellGraph/2013-02-12_LTi6B_dis_A_T0_cells_minW1_cellGraph.ply", package = "mgx2r")

## ----readPly, warning = FALSE, message = FALSE---------------------------
myMesh <- read_mgxPly(file = filePly, ShowSpecimen = FALSE, addNormals = TRUE,
                               MatCol= 1, header_max = 30,
                               my_colors = c("#800000", "#FF0000", "#808000", "#FFFF00",
                                             "#008000", "#00FF00", "#008080", "#00FFFF",
                                             "#000080", "#0000FF", "#800080", "#FF00FF"))

## ---- eval=FALSE---------------------------------------------------------
#  rgl::shade3d(myMesh)

## ----modifColor, eval=FALSE----------------------------------------------
#  myMesh$material$color <- myMesh$allColors$Col_signal
#  rgl::shade3d(myMesh)

## ----readCellGraph-------------------------------------------------------
myCellGraph <- read_mgxCellGraph(fileCellGraph = fileCellGraph, header_max = 30)

## ------------------------------------------------------------------------
meshCellcenter <- myCellGraph$vertices[,c("label","x", "y", "z")]

#other way to define cell centers, without the cell graph
vertexCellcenter <- purrr::map(1:ncol(myMesh$allColors$Col_label), ~ 
  myMesh$it[ which(myMesh$allColors$Col_label[,.x] == names(which(table(myMesh$allColors$Col_label[,.x]) == 1))), .x ]
)

## ----map3D---------------------------------------------------------------
p1 <- plotlyMesh(meshExample = myMesh,
           meshColors = myMesh$allColors$Col_label,
           meshCellcenter = meshCellcenter)

p1

## ----map3D2--------------------------------------------------------------
plotlyMesh(meshExample = myMesh,
           meshColors = NULL,
           meshCellcenter = meshCellcenter)

## ----heatmap3D, warning = FALSE, message = FALSE-------------------------
p2 <- plotlyMesh(meshExample = myMesh, 
           meshColors = left_join(myMesh$it_label, myCellGraph$vertices) %>%
             select(., GeometryArea),
           meshCellcenter =  meshCellcenter
)
p2

## ----heatmap3D2, warning = FALSE, message = FALSE------------------------
meshPlotly <- plotlyMesh(meshExample = myMesh, 
                meshColors = NULL,
                meshCellcenter = meshCellcenter)

show_tensors(p = meshPlotly,
             cellGraph = myCellGraph,
             tensor_name = "CellAxisPDG",
             scale = 150)

## ----cellContours--------------------------------------------------------
meshPlotly <- plotlyMesh(meshExample = myMesh,
           meshColors = NULL)

show_cellcontour(p = meshPlotly, mesh = myMesh)


