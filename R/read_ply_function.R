#' A reading .ply Function
#'
#' This function is a modified version of read.ply function from geomorph R package to specifically read MGX ply files.
#' @param file .ply file path. The .ply file stores information relative to a triangular mesh created with MorphoGraphX.
#' @param ShowSpecimen Default to TRUE. Display the newly created mesh3D with the chosen color (defined in MatCol).
#' @param addNormals Default to TRUE
#' @param MatCol Default to "signal". Expect either "signal" (fluorescent signal projected on the mesh), "parent" (cell label) or "label" (cell face label). To fill Material$color of mesh 3D.
#' @param my_colors colors for displaying the mesh in an hexadecimal format
#' @import dplyr
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
  # when function seems clean and work without problem on these meshes, save without date and check git
  # try to make function more general: recognize that data are scalar, tensor, normals... (1,2,3 values)
  # with given data type: always use the same way to read the data/work with it = make it as general as possible
  # look for full 3D mesh (several meshes stuck together?)
  # create .Rmd (Vignette)
  # document DESCRIPTION

  #Read only chunk corresponding to header
  plyhead <- scan(file = file, what = "char", sep = "\n", strip.white = TRUE,
                  quiet = TRUE, comment.char = "", multi.line = FALSE, n = header_max)

  is.ply <- grep("ply", plyhead)
  if ((length(is.ply) == 0))
    stop("File is not a PLY file")
  format <- unlist(strsplit(grep(c("format "), plyhead, value = TRUE),
                            " "))
  if (format[2] != "ascii")
    stop("PLY file is not ASCII format: ", "format = ", format[2:length(format)])


  # Extract information from reader (about vertices and faces)
  nline_vertices <- grep(c("element vertex"), plyhead)
  xline <- unlist(strsplit( plyhead[nline_vertices] , " "))
  nvertices <- as.numeric(xline[grep(c("vertex"), xline) + 1]) #number of vertices

  nline_faces <- grep(c("element face"), plyhead)
  yline <- unlist(strsplit( plyhead[nline_faces] , " "))
  nfaces <- as.numeric(yline[grep(c("face"), yline) + 1]) #number of (triangular) faces

  headerend <- grep(c("end_header"), plyhead) #looking for the end of the header

  ppty_vertices <- tibble::tibble( X = plyhead[(nline_vertices+1) : (nline_faces-1)] ) %>%
  tidyr::separate(sep = " ", col = X, into = c("Property", "Type", "Name"))

  # nface <- as.numeric(yline[grep(c("face"), yline) + 1]) #number of (triangular) faces #### duplicated line

  plymat_vertex <- tibble::tibble( X = readr::read_lines(file = file, skip = headerend, n_max = nvertices) ) %>%
    tidyr::separate(sep = " ", col = X, into = ppty_vertices$Name, convert = TRUE) %>%
    dplyr::mutate(one = rep(1, nrow(.)))


  ppty_faces <- tibble::tibble( X = plyhead[(nline_faces+1) : (headerend-1)] ) %>%
    tidyr::separate(sep = " ", col = X, into = c("Property", "Type", "Name", "uchar", "Y"), fill = "right") %>%
    dplyr::mutate(Type = ifelse(Name == "uchar", uchar, Type),
           uchar =  ifelse(Name == "uchar", TRUE, FALSE),
           Name = ifelse(Name == "uchar", Y, Name)) %>%
    dplyr::select(-Y)


  if ( sum(ppty_faces$uchar) != 0 ){ #to get rid of uchar columns
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
                                                     n_max = nfaces ) ) %>%
    tidyr::separate(sep = " ", col = X, into = ppty_faces$Name, convert = TRUE)


  #### Mesh material ####
  material <- NULL
  if (yline[3] == 0) { #NB; if zero faces, no def of mesh color?
    print("Object has zero faces")
  }else{
    print(paste0("Object has ", nrow(plymat_face), " faces and ", nrow(plymat_vertex), " vertices."))
    #label_it <- dplyr::select(plymat_face, label) # cell face label
    # face <- dplyr::select(plymat_face, dplyr::contains("vertex_index.") ) #id of vertices describing a given face (here faces are triangles)
    # face = face + 1 #face contains triangle ID; ID starts at 0 in C++ and 1 in R
  }


  ##Mesh color

  if (length(grep(names(plymat_vertex), pattern = "label")) !=0){

    LabCol <- rep(my_colors, length.out = max(plymat_vertex$label+2) ) #as much colors as label level

    # label color per vertex
    plymat_vertex <- plymat_vertex %>%
      dplyr::mutate(vb_lab = LabCol[label+2]) %>% #vb_lab: color for each vertex
      dplyr::arrange(-label)

    # label color per face
    #LabCol <- rep(my_colors, length.out = max(plymat_face$label+2) ) #as much colors as label level
    #in theory, same labels for vertices vb and triangles it
    plymat_face <- plymat_face %>%
      dplyr::mutate(it_lab = LabCol[label+2]) %>% #it_lab: color for each triangle
      dplyr::arrange(-label)
  }


  if (length(grep(names(plymat_vertex), pattern = "signal")) !=0){
    # fluorescence signal per vertex
    fluoSig <- plymat_vertex$signal

    plymat_vertex <- plymat_vertex %>%
      dplyr::mutate(vb_fluo = grey(fluoSig/max(fluoSig)))

    plymat_face %>% #in which vertex index is replaced by fluoSig
      dplyr::mutate(it_fluo.1 = plymat_vertex$vb_fluo[vertex_index.1+1],
                    it_fluo.2 = plymat_vertex$vb_fluo[vertex_index.2+1],
                    it_fluo.3 = plymat_vertex$vb_fluo[vertex_index.3+1])
  }

    if (length(grep(names(plymat_vertex), pattern = "parent")) !=0){

        fluoPar_tmp <- plymat_vertex$parent
        ParCol <- rep(my_colors, length.out = max(plymat_vertex$parent+2) ) #as much colors as label level

        # parent label color per vertex
        plymat_vertex <- plymat_vertex %>%
          dplyr::mutate(vb_par = ParCol[parent+2]) %>% #vb_lab: color for each vertex
          dplyr::arrange(-parent)

        # parent label color per face
        # plymat_face <- plymat_face %>%
        #   dplyr::mutate(it_par = ParCol[parent+2]) # does not work: missing parent label per face
    }

    if (length(grep(names(plymat_vertex), pattern = "cellNr")) !=0){
        cell_nb_tmp <- plymat_vertex$cellNr
        #cell_nb_it <-
    }


  ########
  orig_ID_it <- seq(1, nfaces)


	#### Mesh creation ####
    material$specular <- "gray25"

    if (MatCol == "label"){
        material$color <- plymat_face$it_lab
    }
    # }else if (MatCol == "signal"){
    #     material$color <- MatSig
    # }else if (MatCol == "parent"){
    #     material$color <- MatPar
    # }

  mesh <- list(vb = t(dplyr::select(plymat_vertex, x, y, z, one)),
               it = t(dplyr::select(plymat_face, dplyr::contains("vertex_index.") )+1), # C++ starts at index 1
               primitivetype = "triangle",
               material = material,
               label = plymat_vertex$label,
               signal = plymat_vertex$signal,
               parent = plymat_vertex$parent,
               cell_nb = plymat_vertex$cellNr,
               label_it = plymat_face$label,
               #label_it2 = label_it2,
               #parent_it = parent_it,
               #cell_nb_it = cell_nb_it,
               MatLab = plymat_face$it_lab,
               # MatSig = MatSig,
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
            rgl::dot3d(mesh)
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
