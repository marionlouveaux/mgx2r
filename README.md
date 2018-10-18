
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/marionlouveaux/mgx2r.svg?branch=master)](https://travis-ci.org/marionlouveaux/mgx2r) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/marionlouveaux/mgx2r?branch=master&svg=true)](https://ci.appveyor.com/project/marionlouveaux/mgx2r) [![DOI](https://zenodo.org/badge/136828022.svg)](https://zenodo.org/badge/latestdoi/136828022)

mgx2r
=====

<img src="https://raw.githubusercontent.com/marionlouveaux/mgx2r/master/img/mgx2r_logo.png" width="20%" />

The goal of {mgx2r} is to ease the analysis of mesh and cell graph files created with the MorphoGraphX software. [MorphoGraphX](http://www.mpipz.mpg.de/MorphoGraphX) is a software for 3D visualisation and segmentation of microscopy images.

How to cite
-----------

To cite {mgx2r}, call the R built-in command `citation("mgx2r")`.

> Marion Louveaux, & Sébastien Rochette. (2018, October 18). marionlouveaux/mgx2r: mgx2r: a R package for importing meshes and cell graph files from MorphoGraphX software (Version v0.0.2). Zenodo. <http://doi.org/10.5281/zenodo.1466047>

Installation
------------

You can install the released version of {mgx2r} from GitHub

``` r
# install.packages("devtools")
devtools::install_github("marionlouveaux/mgx2r")
# With vignette
devtools::install_github("marionlouveaux/mgx2r",
  build_vignettes = TRUE)
```

Full documentation with {pkgdown}
---------------------------------

See full documentation created with {pkgdown} at <https://marionlouveaux.github.io/mgx2r/>

Vignettes
---------

Two vignettes are available in the package. You can have access to the vignettes if you installed the package using `build_vignettes = TRUE`.

-   `vignette("vignette_basics", package = "mgx2r")`
-   `vignette("vignette_time_series", package = "mgx2r")`

Example
-------

### Read dataset

Some .ply demonstration data coming from my PhD thesis are attached to this package. This dataset is a timelapse recording of the development of a shoot apical meristem of the plant expressing a membrane marker. I took one 3D stack every 12h and have 5 timepoints in total. For more information regarding the generation of this dataset, see `help.search("mgx2r-package")`.

``` r
### Full datataset
filePly <- system.file("extdata", "full/mesh/mesh_meristem_full_T0.ply", package = "mgx2r")

fileCellGraph <- system.file("extdata",  "full/cellGraph/cellGraph_meristem_full_T0.ply", package = "mgx2r")
```

The mesh data are read and converted as mesh 3D using the read\_mgxPly function. They contain informatons relative to the geometry of the plant tissue.

``` r
library(mgx2r)
mgx_palette <- c("#800000", "#FF0000", "#808000", "#FFFF00",
                "#008000", "#00FF00", "#008080", "#00FFFF",
                "#000080", "#0000FF", "#800080", "#FF00FF")

myMesh <- read_mgxPly(
  file = filePly, ShowSpecimen = FALSE, addNormals = TRUE,
  MatCol = 1, header_max = 30, my_colors = mgx_palette)
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

<img src="https://raw.githubusercontent.com/marionlouveaux/mgx2r/master/inst/img/full/p1labels.png" width="100%" />

Acknowledgements
----------------

Many thanks to Dr. Soeren Strauss and Dr. Richard Smith from the Max Planck Institute for Breeding Research for their help and advices on the {mgx2r} package.

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
