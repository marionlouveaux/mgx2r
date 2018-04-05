## ----lib-----------------------------------------------------------------
library(magrittr)
library(dplyr)

## ----allData-------------------------------------------------------------
dirData <- "/Users/mlouveaux/Documents/Postdoc/Projects/MGX-R bridge/MorphoGraphX2R/inst/extdata"

## ----myData--------------------------------------------------------------
dirPly <- "/2017-02-15_ply_files"

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

## ----map3D, eval=FALSE---------------------------------------------------
#  

## ----map, eval=FALSE-----------------------------------------------------
#  dirData2 <- paste0(dirData, "/Louveaux_PhD/2013-02-12_LTi6B_dis_A")
#  
#  lineages <- c("/2013-02-12_LTi6B_dis_A_T0-T12h_lineage.csv",
#    "/2013-02-12_LTi6B_dis_A_T12h-T24h_lineage.csv",
#    "/2013-02-12_LTi6B_dis_A_T24h-T36h_lineage.csv",
#    "/2013-02-12_LTi6B_dis_A_T36h-T48h_lineage.csv")
#  
#  allLineages <- purrr::map(1:length(lineages), function(i){
#    lin_tmp <- tibble::as.tibble(read.csv2(paste0(dirData2, lineages[i]), header = TRUE, sep = ","))
#    lin <- dplyr::bind_cols(lin_tmp,
#                            data.frame(frameSource = rep(as.integer(i), nrow(lin_tmp)),
#                                       frameTarget = rep(as.integer(i+1), nrow(lin_tmp)))) %>%
#      dplyr::mutate(ID_Source = Parent.Label,
#                    ID_Target = Label) %>%
#      dplyr::select(ID_Source, ID_Target)
#      names(lin) <- purrr::map2_chr(names(lin), c(i, i+1), ~ paste0(.x, .y))
#      lin
#  }
#  )
#  
#  
#  tmp_store <- allLineages[[1]]
#  for (j in 2:length(lineages)){
#    tmp_store <- dplyr::full_join(tmp_store, allLineages[[j]], # full join or left join?
#                   by=setNames(paste0("ID_Source", j), paste0("ID_Target", j)))
#  }
#  
#  
#  library(plotly)
#  dirData2 <- paste0(dirData, "/Louveaux_PhD/2013-02-12_LTi6B_dis_A")
#  plyExample <- paste0(dirData2, "/normalMesh/2013-02-12_LTi6B_dis_A_T48h_cells_minW1_normalMesh.ply")
#  meshExample <- MorphoGraphX2R::modified_read.ply(file = plyExample)
#  
#  test <- dplyr::left_join(meshExample$it_label, tmp_store, by=c("label" = "ID_Target5")) %>%
#    dplyr::select(., label, ID_Source1) %>%
#    unique(.)
#  
#  n.of.desc <- table(test$ID_Source1) #nb of grandchildren/descendant for this particular cell
#  
#  link <- purrr::map( test$ID_Source1, function(x){
#    if (length(which(as.numeric(row.names(n.of.desc)) == x)) ==0){
#      NA
#    }else{
#      n.of.desc[which(row.names(n.of.desc) == x)]
#    }
#    })
#  unlist(link)
#  
#  
#  color <- NULL
#  for (i in 1:ncol(meshExample$allColors$Col_label)){
#  color[i] <-  setdiff(unique(meshExample$allColors$Col_label[,i]), "#000000") # here I remove black vertices
#  } # to be more general, remove any color shared by two vertices
#  
#  trace2 <- list(type="mesh3d",
#                  x = meshExample$vb[1,],
#                  y = meshExample$vb[2,],
#                  z = meshExample$vb[3,],
#                  i = meshExample$it[1,]-1, # NB indices start at 0
#                  j = meshExample$it[2,]-1,
#                  k = meshExample$it[3,]-1,
#                 facecolor = color
#  )
#  #facecolor: one color per triangle (e.g. length(facecolor) == length(i))
#  
#  # data <- list(trace2)
#  
#  layout <- list(
#    scene = list(
#      xaxis = list(
#        backgroundcolor = "rgb(230,230,230)",
#        gridcolor = "rgb(255,255,255)",
#        showbackground = TRUE,
#        zerolinecolor = "rgb(255,255,255"
#      ),
#      yaxis = list(
#        backgroundcolor = "rgb(230,230,230)",
#        gridcolor = "rgb(255,255,255)",
#        showbackground = TRUE,
#        zerolinecolor = "rgb(255,255,255"
#      ),
#      zaxis = list(
#        backgroundcolor = "rgb(230,230,230)",
#        gridcolor = "rgb(255,255,255)",
#        showbackground = TRUE,
#        zerolinecolor = "rgb(255,255,255"
#      )
#    ),
#    title = "My mesh",
#    xaxis = list(title = "m[, 1]"),
#    yaxis = list(title = "m[, 2]")
#  )
#  
#  p <- plot_ly()
#  p <- add_trace(p, x=trace2$x, y=trace2$y, z=trace2$z,
#                    facecolor=trace2$facecolor,
#                 i=trace2$i, j=trace2$j, k=trace2$k, type=trace2$type)
#  p <- layout(p, scene=layout$scene, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)
#  
#  p
#  
#  

