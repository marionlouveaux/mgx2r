
<!-- README.md is generated from README.Rmd. Please edit that file -->
mgx2r
=====

<img src="https://github.com/statnmap/marionlouveaux/blob/master/img/mgx2r-logo.png" width="20%" />

The goal of mgx2r is to ease the analysis of mesh and cell graph files created with the MorphoGraphX software. MorphoGraphX is a software for 3D visualisation and segmentation of microscopy images.

Installation
------------

You can install the released version of mgx2r from GitHub

``` r
# install.packages("devtools")
devtools::install_github("marionlouveaux/mgx2r")
```

Example
-------

### Read dataset

Some .ply demonstration data coming from my PhD thesis are attached to this package. This dataset is a timelapse recording of the development of a shoot apical meristem of the plant expressing a membrane marker. I took one 3D stack every 12h and have 5 timepoints in total. For more information regarding the generation of this dataset, see `help("mgx2r")`.

``` r
### Full datataset
filePly <- system.file("extdata", "full/mesh/mesh_meristem_full_T0.ply", package = "mgx2r")

fileCellGraph <- system.file("extdata",  "full/cellGraph/cellGraph_meristem_full_T0.ply", package = "mgx2r")
```

The mesh data are read and converted as mesh 3D using the read\_mgxPly function. They contain informatons relative to the geometry of the plant tissue.

``` r
library(mgx2r)
myMesh <- read_mgxPly(file = filePly, ShowSpecimen = FALSE, addNormals = TRUE,
                               MatCol= 1, header_max = 30,
                               my_colors = c("#800000", "#FF0000", "#808000", "#FFFF00",
                                             "#008000", "#00FF00", "#008080", "#00FFFF",
                                             "#000080", "#0000FF", "#800080", "#FF00FF"))
#> [1] "Object has 7763 faces and 4158 vertices."
```

The cell graph data are read and converted as mesh 3D using the read\_mgxCellGraph function. They contain data relative to the area of the cells and local curvature of the tissue.

``` r
myCellGraph <- read_mgxCellGraph(fileCellGraph = fileCellGraph, header_max = 30)
```

### Visualise using {cellviz3d}

The mesh and cell graph data can be visualised using the package [{cellviz3d}](https://github.com/marionlouveaux/cellviz3d):

``` r
library(cellviz3d)
meshCellcenter <- myCellGraph$vertices[,c("label","x", "y", "z")]

p1 <- plotlyMesh(meshExample = myMesh,
                 meshColors = myMesh$allColors$Col_label,
                 meshCellcenter = meshCellcenter) %>%
  plotly::layout(scene = list(aspectmode = "data"))

p1
```

<img src="https://github.com/marionlouveaux/mgx2r/blob/master/inst/img/full/p1labels.png" width="100%" />

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
