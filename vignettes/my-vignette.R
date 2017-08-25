## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

## ----data----------------------------------------------------------------
ply_dir <- "/Users/mlouveaux/Documents/Postdoc/Projects/MGX-R bridge/2017-02-15_ply_files"

file <- paste0(ply_dir, "/test4.ply")
cellGraphfile <- paste0(ply_dir, "/test4_cellGraph.ply")

# ply_dir2 <- "/Users/mlouveaux/Documents/Postdoc/Projects/MGX-R bridge/2016-11-25_ply_files"
# file <- paste0(ply_dir2, "/plyTest.ply")


