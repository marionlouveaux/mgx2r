#' A reading .ply Function
#'
#' This function is a modified version of read.ply function from geomorph R package to specifically read MGX ply files.
#' @param file .ply file path. The .ply file stores information relative to a triangular mesh created with MorphoGraphX.
#' @param ShowSpecimen Default to TRUE. Display the newly created mesh3D with the chosen color (defined in MatCol).
#' @param addNormals Default to TRUE.
#' @param MatCol Default to "signal". Expect either "signal" (fluorescent signal projected on the mesh), "parent" (cell label) or "label" (cell face label). To fill Material$color of mesh 3D.
#' @param header_max number of lines expected in header. Must be equal or greater to the actual number of lines in the header. Default to 30.
#' @param my_colors colors for displaying the mesh in an hexadecimal format.
#' @importFrom magrittr %>%
#' @keywords .ply, read
#' @export
#' @examples
#' @return Object of type mesh3D
#' modified_read.ply()


modified_read.ply <- function (file, ShowSpecimen = TRUE, addNormals = TRUE,
                               MatCol= "signal", header_max = 30,
                               my_colors = c("#800000", "#FF0000", "#808000", "#FFFF00",
                                              "#008000", "#00FF00", "#008080", "#00FFFF",
                                              "#000080", "#0000FF", "#800080", "#FF00FF"))
  {



  #2017-08-01: check function on different meshes (embryo + Soeren meshes)
  # try to make function more general: recognize that data are scalar, tensor, normals... (1,2,3 values)
  # with given data type: always use the same way to read the data/work with it = make it as general as possible
  # look for full 3D mesh (several meshes stuck together?)
  # document DESCRIPTION

  #### Get header ####
  # Read only chunk corresponding more or less to header, defined by header max
  plyhead <- scan(file = file, what = "char", sep = "\n", strip.white = TRUE,
                  quiet = TRUE, comment.char = "", multi.line = FALSE, n = header_max)

  is.ply <- grep("ply", plyhead)
  if ((length(is.ply) == 0))
    stop("File is not a PLY file")
  format <- unlist(strsplit(grep(c("format "), plyhead, value = TRUE),
                            " "))
  if (format[2] != "ascii")
    stop("PLY file is not ASCII format: ", "format = ", format[2:length(format)])

  #### Read header ####
  # Extract information from header (about vertices and faces)
  nline_vertices <- grep(c("element vertex"), plyhead)
  xline <- unlist(strsplit( plyhead[nline_vertices] , " "))
  nvertices <- as.numeric(xline[grep(c("vertex"), xline) + 1]) #number of vertices

  nline_faces <- grep(c("element face"), plyhead)
  yline <- unlist(strsplit( plyhead[nline_faces] , " "))
  nfaces <- as.numeric(yline[grep(c("face"), yline) + 1]) #number of (triangular) faces

  headerend <- grep(c("end_header"), plyhead) #looking for the end of the header

  ppty_vertices <- tibble::tibble( X = plyhead[(nline_vertices+1) : (nline_faces-1)] ) %>% # vertices properties in header
  tidyr::separate(sep = " ", col = X, into = c("Property", "Type", "Name"))

  plymat_vertex <- tibble::tibble( X = readr::read_lines(file = file, skip = headerend, n_max = nvertices) ) %>% # vertices matrix
    tidyr::separate(sep = " ", col = X, into = ppty_vertices$Name, convert = TRUE) %>%
    dplyr::mutate(one = rep(1, nrow(.)))


  ppty_faces <- tibble::tibble( X = plyhead[(nline_faces+1) : (headerend-1)] ) %>% # faces properties in header
    tidyr::separate(sep = " ", col = X, into = c("Property", "Type", "Name", "uchar", "Y"), fill = "right") %>%
    dplyr::mutate(Type = ifelse(Name == "uchar", uchar, Type),
           uchar =  ifelse(Name == "uchar", TRUE, FALSE),
           Name = ifelse(Name == "uchar", Y, Name)) %>%
    dplyr::select(-Y)


  if ( sum(ppty_faces$uchar) != 0 ){ #to get rid of uchar columns in ppty_faces
    ppty_faces_orig <- ppty_faces
    index_uchar <- which(ppty_faces_orig$uchar == TRUE)

    i2 <- 0
    for (i in index_uchar){

      Name_i_tmp <- ppty_faces_orig$Name[i]

      plymat_face_tmp <- tibble::tibble( X = readr::read_lines(file = file,
                                                         skip = (headerend + nvertices),
                                                         n_max = 1 ) ) %>%
        tidyr::separate(sep = " ", col = X, into = ppty_faces$Name, convert = TRUE,
                        extra = "drop")
      nb_col_tmp <- ncol(plymat_face_tmp)
      nb_col_uchar <- plymat_face_tmp[[ Name_i_tmp ]]#added 2017-08-01

      if (i+i2 != nrow(ppty_faces)){

        ppty_name_tmp <- c(ppty_faces$Name[1:(i + i2)],
                             paste(ppty_faces$Name[i + i2], 1:nb_col_uchar, sep = "."),
                             ppty_faces$Name[(i + i2 + 1):nrow(ppty_faces)])

        ppty_faces <- ppty_faces[c(1:(i + i2), rep(i + i2, nb_col_uchar),
                                   ((i + i2 + 1):nb_col_tmp)), ]

      }else{
        ppty_name_tmp <- c(ppty_faces$Name[1:(i + i2)],
                             paste(ppty_faces$Name[i + i2], 1:nb_col_tmp, sep = ".") )

        ppty_faces <- ppty_faces[c(1:(i+i2), rep(i+i2, nb_col_tmp)), ]
      }
      ppty_faces$Name <- ppty_name_tmp
      i2 <- i2 + nb_col_tmp
    }
  }

  plymat_face <- tibble::tibble( X = readr::read_lines(file = file,
                                                     skip = (headerend + nvertices),
                                                     n_max = nfaces ) ) %>% # faces matrix
    tidyr::separate(sep = " ", col = X, into = ppty_faces$Name, convert = TRUE) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("vertex_index.")), dplyr::funs(. + 1) )


  #### Mesh material and color ####
  if (yline[3] == 0) { #NB; if zero faces, no def of mesh color? -- check a mesh without faces
    print("Object has zero faces")
  }else{
    print(paste0("Object has ", nrow(plymat_face), " faces and ", nrow(plymat_vertex), " vertices."))
  }


  ##Mesh color
  #- to MAKE IT MORE GENERAL, identify all the items in ppty that could be displayed using a color code
  #- distinguish the discrete ones (fluorescent signal) from the continuous ones?
  #- give them names according to their original name in ppty -- issue with mutate: drops columns
  #- allow user to change material color according to those ppties (after mesh creation?)


  all_vertices_items <- dplyr::filter(ppty_vertices, Name != "x" & Name != "y" & Name != "z")

  for (i in 1:nrow(all_vertices_items)){
    name_vertex_item <- all_vertices_items$Name[i]
    vertex_item <- plymat_vertex[, name_vertex_item]

    if (dplyr::summarise_all(vertex_item, class) == "integer"){ # label, parent label, cell number...

      LabCol <- rep(my_colors, length.out = nrow(unique(vertex_item)) )
      LabCol[which(unique(vertex_item) == -1)] <- "#000000" # change column name each time, or they will be redundancy&PB when calling col_fluoSig below

      col_corresp <- dplyr::bind_cols(label = unique(vertex_item), LabCol = LabCol)
      plymat_vertex <- dplyr::left_join(plymat_vertex, col_corresp, by = name_vertex_item)

      plymat_face <- dplyr::mutate_at(plymat_face, dplyr::vars(dplyr::contains("vertex_index.")), dplyr::funs( item = plymat_vertex$LabCol[.] ) )
      colnames(plymat_face) <- gsub(colnames(plymat_face), pattern = "_item", replacement = paste0("_", name_vertex_item))

    } else { # fluorescent signal

      col_fluoSig <- rgb(dplyr::bind_cols(vertex_item, vertex_item, vertex_item), maxColorValue = max(vertex_item))
      plymat_vertex <- dplyr::bind_cols(plymat_vertex, col_fluoSig = col_fluoSig) # change column name each time, or they will be redundancy&PB when calling col_fluoSig below
      plymat_face <- dplyr::mutate_at(plymat_face, dplyr::vars(dplyr::contains("vertex_index.")), dplyr::funs( item = plymat_vertex$col_fluoSig[.] ) )
      colnames(plymat_face) <- gsub(colnames(plymat_face), pattern = "_item", replacement = paste0("_", name_vertex_item))
    }
  }


  orig_ID_it <- seq(1, nfaces)


	#### Mesh creation ####
  material <- NULL
    material$specular <- "gray25"

    if (MatCol == "label"){
      material$color <- t(plymat_face2[ , c("vertex_index.1", "vertex_index.2", "vertex_index.3")])

    }else if (MatCol == "signal"){
        material$color <- plymat_face[ , c("it_sig.1", "it_sig.2", "it_sig.3")]
    }else if (MatCol == "parent"){
        material$color <- plymat_face[ , c("it_par.1", "it_par.2", "it_par.3")]
    }

  mesh <- list(vb = t(dplyr::select(plymat_vertex, x, y, z, one)),
               it = t(dplyr::select(plymat_face, dplyr::contains("vertex_index.") )),
               primitivetype = "triangle",
               material = material,
               label = plymat_vertex$label, # make it more general
               signal = plymat_vertex$signal,
               parent = plymat_vertex$parent,
               cell_nb = plymat_vertex$cellNr,
               label_it = plymat_face$label,
               #label_it2 = label_it2,
               #parent_it = parent_it,
               #cell_nb_it = cell_nb_it,
               #it_lab = plymat_face$it_lab,
               #it_sig = plymat_face[ , c("it_sig.1", "it_sig.2", "it_sig.3")],
               # MatPar = MatPar,
               orig_ID_it = orig_ID_it)
    #label, signal, parent: on vertices
    #label_it, MatLab, MatSig and MatPar: on triangles (label_it and MatLab give the same info, but MatLab is in RGB hexa code)

    class(mesh) <- c("mesh3d", "shape3d")
    if (addNormals == TRUE) {
        mesh <- rgl::addNormals(mesh)
    }

	#### Mesh display (with rgl library) ####
    if (ShowSpecimen == TRUE) {
        rgl::clear3d()
        if (nfaces == 0) {
            rgl::dot3d(mesh) # needs to look for vertex color only
        }
        if (length(material) != 0) {
            rgl::shade3d(mesh)
        }else{
          # rgl::shade3d(mesh, color = "gray") # should I include a default case without color?
          rgl::shade3d(mesh, color = MatCol)
        }
    }

    return(mesh)
}
