## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE-----------------------------------
library(colorRamps)
library(dplyr)
library(plotly)
library(purrr)
library(mgx2r)

## ---- warning = FALSE, message = FALSE-----------------------------------
ply.dir <- system.file("extdata", "normalMesh/", package = "mgx2r")

mesh.all <- map(list.files(ply.dir, recursive = TRUE, full.names = TRUE),
                ~ read_mgxPly(file = .x, ShowSpecimen = FALSE))


graph.dir <- system.file("extdata", "cellGraph/", package = "mgx2r")

cellGraph.all <- map(list.files(graph.dir, recursive = TRUE, full.names = TRUE), ~read_mgxCellGraph(fileCellGraph = .x, header_max = 30))

## ---- warning = FALSE, message = FALSE-----------------------------------
meshColors.all <- list(NULL, NULL, NULL, NULL, NULL)

plotlyMesh_all(meshExample = mesh.all,
                       graphExample = cellGraph.all,
                       meshColors = meshColors.all,
                       display = 'heatmap')


