#' Identify and sort uchar in the properties of vertices and faces
#'
#' @param ppty Lines containing the properties info in the header of the .ply.
#' @param file Ply file.
#' @param toSkip To skip when reading the file (either headerend or headerend+nvertices if reading the face ppty).
#' @param Nlines Number of face or vertices.
#'
#' @importFrom dplyr tibble mutate select
#' @importFrom purrr map map_dfr
#' @importFrom tidyr separate
#' @importFrom readr read_lines
#' @export
#' @return A tibble.

ucharHelp <- function(ppty,
                      file = file,
                      toSkip = toSkip,
                      Nlines = Nlines){

  ppty <- tibble( X = ppty ) %>% # faces properties in header
    separate(sep = " ", col = X, into = c("Property", "Type", "Name", "uchar", "Y"), fill = "right") %>%
    mutate(Type = ifelse(Name == "uchar", uchar, Type),
           uchar =  ifelse(Name == "uchar", TRUE, FALSE),
           Name = ifelse(Name == "uchar", Y, Name)) %>%
    select(-Y)

  if ( sum(ppty$uchar) != 0 ){ #to get rid of uchar columns in ppty
    ppty_orig <- ppty
    index_uchar <- which(ppty_orig$uchar == TRUE)

    i2 <- 0
    for (i in index_uchar){

      Name_i_tmp <- ppty_orig$Name[i]

      plymat_face_tmp <- tibble( X = read_lines(file = file,
                                                skip = toSkip,
                                                n_max = 1 ) ) %>%
        map_dfr(~trimws(.)) %>% # to remove blank spaces before and after a line
        separate(sep = " ", col = X, into = ppty$Name, convert = TRUE,
                 extra = "drop")
      nb_col_tmp <- ncol(plymat_face_tmp)
      nb_col_uchar <- plymat_face_tmp[[ Name_i_tmp ]]

      if (i+i2 != nrow(ppty)){

        ppty_name_tmp <- c(ppty$Name[1:(i + i2)],
                           paste(ppty$Name[i + i2], 1:nb_col_uchar, sep = "."),
                           ppty$Name[(i + i2 + 1):nrow(ppty)])

        ppty <- ppty[c(1:(i + i2), rep(i + i2, nb_col_uchar),
                       ((i + i2 + 1):nb_col_tmp)), ]

      }else{
        ppty_name_tmp <- c(ppty$Name[1:(i + i2)],
                           paste(ppty$Name[i + i2], 1:nb_col_uchar, sep = ".") )

        ppty <- ppty[c(1:(i+i2), rep(i+i2, nb_col_uchar)), ]
      }
      ppty$Name <- ppty_name_tmp
      i2 <- i2 + nb_col_tmp
    }
  }

  plymat <- tibble( X = read_lines(file = file,
                                          skip = toSkip,
                                          n_max = Nlines ) ) %>%
    map_dfr(~trimws(.)) %>% # to remove blank spaces before and after a line
    separate(sep = " ", col = X, into = ppty$Name, convert = TRUE)

  return(plymat)
}
