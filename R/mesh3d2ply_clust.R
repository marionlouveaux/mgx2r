#' Write a ply format for MorphoGraphX from a mesh3D R object
#'
#' original code: 2016-09-11_mesh3D_to_ply_clust
#' @param mesh a 3D (triangular) mesh object
#' @param filename the filename we want to give to the .ply file
#' @param label_it_for_mesh triangle color
#' @keywords mesh3D, ply
#' @export
#' @examples
#' @importFrom utils "write.table"
#' @return a .ply file readable by MorphoGraphX
#' mesh3D2ply_clust()

mesh3D2ply_clust <- function(mesh = mesh,
                             filename = "my_ply.ply",
                             label_it_for_mesh = mesh$label_it){

  #1. Triangles

  # checking that triangles are unique (by ordering and gluing vertex labels together)
  unique_it <- unique(unlist(purrr::map(.x = 1:ncol(mesh$it),
                                        ~ glue::collapse(sort(mesh$it[,.]), sep = "_") )))
  if(length(unique_it) < ncol(mesh$it)){
    warning("Mesh has duplicated triangles.")
  }

  # triangles_IDs <- cbind( format(rep(3, ncol(mesh$it)), scientific = FALSE) #format has to be applied column by column
  #                         , format(mesh$it[1,]-1, scientific = FALSE)
  #                         , format(mesh$it[2,]-1, scientific = FALSE)
  #                         , format(mesh$it[3,]-1, scientific = FALSE)
  #                         , format(label_it_for_mesh, scientific = FALSE)
  # )

  triangles_IDs <- t(mesh$it) %>%
    -1 %>%
    tibble::as.tibble(.) %>%
    dplyr::bind_cols(meshtype = rep(3, nrow(.)), .) %>%
    dplyr::mutate(., label = label_it_for_mesh)


  #2. Vertices

  triangles_label <- rbind( cbind( mesh$it[1,]-1, label_it_for_mesh ),
                            cbind( mesh$it[2,]-1, label_it_for_mesh ),
                            cbind( mesh$it[3,]-1, label_it_for_mesh ) )

  cl <- snow::makeCluster(parallel::detectCores() - 1)
  res <- snow::parApply(cl, triangles_label, 1, function(i){ paste(i[1], i[2], sep = "_") })
  snow::stopCluster(cl)

  # vertices with associated label
  res2 <- matrix(as.numeric(unlist(strsplit( unique(res), split = "_"))), byrow = TRUE, ncol = 2)

  # Repeted vertices will receive label -1
  rep_vb_ind <- which(table(res2[,1]) > 1)
  row_ind <- res2[,1] %in% rep_vb_ind #les elements qui sont répétés (TRUE) ou non (FALSE)
  res2[row_ind, 2] <- -1

  # Missing vertices (not associated to a triangle), will also receive label -1

  all_vertices_from_it <- unique(c( mesh$it[1,]-1,
                                    mesh$it[2,]-1,
                                    mesh$it[3,]-1))
  all_vertices_from_vb <- 1:ncol(mesh$vb)-1
  single_vb <- setdiff(all_vertices_from_vb, all_vertices_from_it)

  label_vb_tmp <- rbind(res2,
                        cbind(single_vb, rep(-1, length(single_vb))))
  label_vb <- label_vb_tmp[order(label_vb_tmp[,1]),]

  # res3 <- res2[order(res2[,1]),]
  # res3_tmp <- res3
  # res3_indices <- which(table(res3_tmp[,1]) >1) ###PB HERE: I DON'T FIND REPETED VERTICES
  # row_ind <- res3_tmp[,1] %in% res3_indices #les elements qui sont répétés (TRUE) ou non (FALSE)
  # res3_tmp[row_ind, 2] <- -1
  # res4 <- unique(res3_tmp[ , 2])



  #on veut la colonne 2, pas la 1; et uniquement les valeurs non redondantes (par rapport à la colonne 1)
  #res4 donne le label des vertex: -1 si entre 2 cellules et label sinon

  vertex_coord <- cbind( format(mesh$vb[1,], scientific = FALSE) # format has to be applied column by column
                         ,format(mesh$vb[2,], scientific = FALSE)
                         ,format(mesh$vb[3,], scientific = FALSE)
                         ,format(label_vb[,2], scientific = FALSE)
  )

  #3. Writing ply file # for the moment removed   cat("property float signal\n")
  sink(file = filename)

  cat("ply\n")
  cat("format ascii 1.0\n")
  cat("element vertex", nrow(vertex_coord), "\n")
  cat("property float x\n")
  cat("property float y\n")
  cat("property float z\n")
  cat("property int label\n")
  cat("element face", nrow(triangles_IDs), "\n") #cat("element face", length(itSUB), "\n") #
  cat("property list uchar int vertex_index\n")
  cat("property int label\n")
  cat("end_header\n")
  sink()

  write.table(x = vertex_coord, file = filename, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE) #quote = FALSE is very important since we force the format to character to avoid the scientific notation
  write.table(x = triangles_IDs, file = filename, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

}

