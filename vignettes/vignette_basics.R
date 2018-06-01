## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----lib, warning = FALSE, message = FALSE-------------------------------
library(colorRamps)
library(MorphoGraphX2R)
library(magrittr)
library(dplyr)
library(plotly)
library(glue)
library(RColorBrewer)

## ----myData--------------------------------------------------------------
filePly <- system.file("extdata", "normalMesh/2013-02-12_LTi6B_dis_A_T0_cells_minW1_normalMesh.ply", package = "MorphoGraphX2R")

fileCellGraph <- system.file("extdata",  "cellGraph/2013-02-12_LTi6B_dis_A_T0_cells_minW1_cellGraph.ply", package = "MorphoGraphX2R")

## ----readPly, warning = FALSE, message = FALSE---------------------------
myMesh <- modified_read.ply(file = filePly, ShowSpecimen = FALSE, addNormals = TRUE,
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
myCellGraph <- modified_read.cellGraph(fileCellGraph = fileCellGraph, header_max = 30)

## ------------------------------------------------------------------------
meshCellcenter <- myCellGraph$vertices[,c("label","x", "y", "z")]

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
p3 <- plotlyMesh(meshExample = myMesh, 
                meshColors = NULL,
                meshCellcenter = meshCellcenter)

# Find coords
find_coords <- function(cellcenter, tipUnit, d, scale = 1) {
  d <- d*scale
  # Distance
  tip1 <- d / (sqrt(sum(tipUnit^2))) * tipUnit + cellcenter
  tip2 <- (-d / (sqrt(sum(tipUnit^2))) * tipUnit) + cellcenter
  tibble(x1 = tip1[1], y1 = tip1[2], z1 = tip1[3],
         x2 = tip2[1], y2 = tip2[2], z2 = tip2[3])
}

# Test avec !!
tensor_name <- "CellAxisPDG"
grand.name <- glue("{tensor_name}.{c(1:3, 7)}")
petit.name <- glue("{tensor_name}.{c(4:6, 8)}")

tmp <- myCellGraph$vertices %>%
  rename(
    tipUnit.1 = !!grand.name[1],
    tipUnit.2 = !!grand.name[2],
    tipUnit.3 = !!grand.name[3],
    d = !!grand.name[4],
  ) %>%
  mutate(grand = purrr::pmap(list(x, y, z, tipUnit.1, tipUnit.2, tipUnit.3, d),
                             ~find_coords(cellcenter = c(..1, ..2, ..3),
                                          tipUnit = c(..4, ..5, ..6),
                                          d = ..7,
                                          scale = 150))) %>%
  tidyr::unnest(grand, .sep = ".") %>%
  select(-starts_with("tipUnit"), -d) %>%
  rename(
    tipUnit.1 = !!petit.name[1],
    tipUnit.2 = !!petit.name[2],
    tipUnit.3 = !!petit.name[3],
    d = !!petit.name[4],
  ) %>%
  mutate(petit = purrr::pmap(list(x, y, z, tipUnit.1, tipUnit.2, tipUnit.3, d),
                             ~find_coords(cellcenter = c(..1, ..2, ..3),
                                          tipUnit = c(..4, ..5, ..6),
                                          d = ..7,
                                          scale = 150))) %>%
  tidyr::unnest(petit, .sep = ".") %>%
  select(-starts_with("tipUnit"), -d) %>%
  inner_join(myCellGraph$vertices)

for (i in 1:nrow(tmp)){
  xcoord <- as.vector(t(select(tmp, x, grand.x1, grand.x2)[i,]))
  ycoord <- as.vector(t(select(tmp, y, grand.y1, grand.y2)[i,]))
  zcoord <- as.vector(t(select(tmp, z, grand.z1, grand.z2)[i,]))
  p3 <- add_trace(p3, x = xcoord, y = ycoord, z = zcoord,
                 line = list(color = 'rgb(0, 0, 0)', width = 6), mode = "lines",
                 type = "scatter3d", hoverinfo = 'none') %>%
    layout(showlegend = FALSE)
  
  xcoord <- as.vector(t(select(tmp, x, petit.x1, petit.x2)[i,]))
  ycoord <- as.vector(t(select(tmp, y, petit.y1, petit.y2)[i,]))
  zcoord <- as.vector(t(select(tmp, z, petit.z1, petit.z2)[i,]))
  p3 <- add_trace(p3, x = xcoord, y = ycoord, z = zcoord,
                 line = list(color = 'rgb(0, 0, 0)', width = 6), mode = "lines",
                 type = "scatter3d", hoverinfo = 'none') %>%
    layout(showlegend = FALSE)

}
p3

## ----cellContours--------------------------------------------------------
vertexCellcenter <- purrr::map(1:ncol(myMesh$allColors$Col_label), ~ 
  myMesh$it[ which(myMesh$allColors$Col_label[,.x] == names(which(table(myMesh$allColors$Col_label[,.x]) == 1))), .x ]
)

edgesCoords <- purrr::map(1:ncol(myMesh$allColors$Col_label), ~ 
  myMesh$vb[,myMesh$it[ which(myMesh$allColors$Col_label[,.x] != names(which(table(myMesh$allColors$Col_label[,.x])==1))), .x]]
)

p4 <- plotlyMesh(meshExample = myMesh,
           meshColors = NULL)

for (i in 1:length(unique(edgesCoords))){
  edge_tmp <- unique(edgesCoords)[[i]]
  xcoord <- edge_tmp[1,]
  ycoord <- edge_tmp[2,]
  zcoord <- edge_tmp[3,]
  p4 <- add_trace(p4, x = xcoord, y = ycoord, z = zcoord,
                 line = list(color = 'rgb(255, 255, 255)', width = 6), mode = "lines",
                 type = "scatter3d", hoverinfo = 'none') %>%
    layout(showlegend = FALSE)
}
p4

