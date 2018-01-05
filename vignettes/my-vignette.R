## ----lib-----------------------------------------------------------------
library(magrittr)

## ----allData-------------------------------------------------------------
dirData <- "/Users/mlouveaux/Documents/Postdoc/Projects/MGX-R bridge/MorphoGraphX2R/inst/extdata/"

## ----myData--------------------------------------------------------------
dirPly <- "2017-02-15_ply_files"

filePly <- paste0(dirData, dirPly, "/test4.ply")
fileCellGraph <- paste0(dirData, dirPly, "/test4_cellGraph.ply")


## ----readPly-------------------------------------------------------------
myMesh <- MorphoGraphX2R::modified_read.ply(file = filePly, ShowSpecimen = TRUE, addNormals = TRUE,
                               MatCol= 1, header_max = 30,
                               my_colors = c("#800000", "#FF0000", "#808000", "#FFFF00",
                                             "#008000", "#00FF00", "#008080", "#00FFFF",
                                             "#000080", "#0000FF", "#800080", "#FF00FF"))

## ----modifColor, eval=FALSE----------------------------------------------
#  myMesh$material$color <- myMesh$allColors$Col_signal
#  #rgl::shade3d(myMesh)

## ----readCellGraph, eval=FALSE-------------------------------------------
#  myCellGraph <- MorphoGraphX2R::modified_read.cellGraph(fileCellGraph = fileCellGraph, header_max = 30)

