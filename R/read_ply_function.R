#' A reading .ply Function
#'
#' This function is a modified version of read.ply function from geomorph R package to specifically read MGX mesh ply files.
#' @param file .ply file path. The .ply file stores information relative to a triangular mesh created with MorphoGraphX.
#' @param ShowSpecimen Default to TRUE. Display the newly created mesh3D with the chosen color (defined in MatCol).
#' @param addNormals Default to TRUE.
#' @param MatCol Integer. Default to 1, e.g. first element the header which is not x, y or z. To fill Material$color of mesh 3D.
#' @param header_max Number of lines expected in header. Must be equal or greater to the actual number of lines in the header. Default to 30.
#' @param my_colors Colors for displaying the mesh in an hexadecimal format.
#'
#' @importFrom dplyr tibble mutate mutate_at vars contains funs filter summarise_all bind_cols left_join select
#' @importFrom glue glue
#' @importFrom grDevices rgb
#' @importFrom purrr map
#' @importFrom readr read_lines
#' @importFrom rgl addNormals clear3d dot3d shade3d
#' @importFrom tidyr separate
#' @keywords .ply, read
#' @export
#' @examples
#' @return Object of type mesh3D
#' modified_read.ply()


modified_read.ply <- function (file, ShowSpecimen = TRUE, addNormals = TRUE,
                               MatCol= 1, header_max = 30,
                               my_colors = c("#800000", "#FF0000", "#808000", "#FFFF00",
                                             "#008000", "#00FF00", "#008080", "#00FFFF",
                                             "#000080", "#0000FF", "#800080", "#FF00FF"))
{

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

  ppty_vertices <- tibble( X = plyhead[(nline_vertices+1) : (nline_faces-1)] ) %>% # vertices properties in header
    separate(sep = " ", col = X, into = c("Property", "Type", "Name"))

  plymat_vertex <- tibble( X = read_lines(file = file, skip = headerend, n_max = nvertices) ) %>% # vertices matrix
    separate(sep = " ", col = X, into = ppty_vertices$Name, convert = TRUE) %>%
    mutate(one = rep(1, nrow(.)))


  plymat_face <- ucharHelp(ppty = plyhead[(nline_faces+1) : (headerend-1)],
                           file = file,
                           toSkip = headerend + nvertices,
                           Nlines = nfaces)

  plymat_face <- plymat_face %>%
    mutate_at(vars(contains("vertex_index.")), funs(. + 1) )


  #### Mesh material and color ####
  if (yline[3] == 0) { #NB; if zero faces, no def of mesh color? -- check a mesh without faces
    print("Object has zero faces")
  }else{
    print(paste0("Object has ", nrow(plymat_face), " faces and ", nrow(plymat_vertex), " vertices."))
  }


  all_vertices_items <- filter(ppty_vertices, Name != "x" & Name != "y" & Name != "z")

  for (i in 1:nrow(all_vertices_items)){
    name_vertex_item <- all_vertices_items$Name[i]
    vertex_item <- plymat_vertex[, name_vertex_item]

    if (summarise_all(vertex_item, class) == "integer"){ # label, parent label, cell number...

      itemCol <- rep(my_colors, length.out = nrow(unique(vertex_item)) ) # repeat my_colors accross the unique occurences of items
      itemCol[which(unique(vertex_item) == -1)] <- "#000000" # not sure it is necessary

      col_corresp <- bind_cols(label = unique(vertex_item), itemCol = itemCol)
      plymat_vertex <- left_join(plymat_vertex, col_corresp)

    } else { # fluorescent signal or other continuous signal

      itemCol <- rgb(bind_cols(vertex_item, vertex_item, vertex_item), maxColorValue = max(vertex_item))
      plymat_vertex <- bind_cols(plymat_vertex, itemCol = itemCol) # change column name each time, or they will be redundancy&PB when calling itemCol below

    }

    # Associate colors to the triangles
    plymat_face <- mutate_at(plymat_face,
                                    vars(contains("vertex_index.")),
                                    funs( item = plymat_vertex$itemCol[.] ) )

    w.item <- grep(".*_item", colnames(plymat_face))
    colnames(plymat_face)[w.item] <- paste0("Col_", name_vertex_item, ".", 1:length(w.item))

    # Rename columns with item name
    colnames(plymat_vertex) <- gsub(colnames(plymat_vertex), pattern = "itemCol", replacement = paste0("Col_", name_vertex_item))

  }

  orig_ID_it <- seq(1, nfaces)


  #### Mesh creation ####
  if (MatCol > nrow(all_vertices_items) | MatCol < 1){
    warning(glue("MatCol must be an integer between 1 and {nrow(all_vertices_items)}"))
  }

  material <- NULL
  material$specular <- "gray25"

  colColors <- grep(glue("Col_", all_vertices_items$Name[MatCol], "."), colnames(plymat_face))
  material$color <- t(plymat_face[ , colColors])

  # iterate on all items toget all colors as a list to store in allcolors
  res.allColors <- map(.x = 1:length(all_vertices_items$Name), .f = function(.x){
    colColors <- grep(glue("Col_", all_vertices_items$Name[.x], "."), colnames(plymat_face))
    t(plymat_face[ , colColors])
  })
names(res.allColors) <- paste0("Col_", all_vertices_items$Name)


  mesh <- list(vb = t(select(plymat_vertex, x, y, z, one)),
               it = t(select(plymat_face, contains("vertex_index.") )),
               primitivetype = "triangle",
               material = material,
               allColors = res.allColors,
               it_label = select(plymat_face,label)
  )

  class(mesh) <- c("mesh3d", "shape3d")
  if (addNormals == TRUE) {
    mesh <- addNormals(mesh)
  }

  #### Mesh display (with rgl library) ####
  if (ShowSpecimen == TRUE) {
    clear3d()
    if (nfaces == 0) {
      dot3d(mesh) # needs to look for vertex color only
    }
    if (length(material) != 0) {
      shade3d(mesh)
    }else{
      # shade3d(mesh, color = "gray") # should I include a default case without color?
      shade3d(mesh, color = MatCol)
    }
  }

  return(mesh)
}

