#' @title A matrix melting function
#' @description This function melts a matrix into a format that can then be read in igraph or visNetwork for data visualization
#' @param x dataset to be melted
#' @keywords melt, matrix
#' @export
#' @examples
#' meltme(avian)
meltme <- function(x){
  colnames(x) <- c("names", 1:nrow(x))
  x <- dplyr::select(x, -names)
  x <- as.matrix(x)
  x[lower.tri(x, diag = TRUE)] <- NA
  x <- reshape2::melt(x)
  x <- stats::setNames(x, c('ind1', 'ind2', 'values'))
  x <- dplyr::filter(x, !is.na(values))
  return(x)
}
#'
#' @title Convert meltme product to igraph format
#' @description This function converts a melted dataset (ideally from the meltme function) into an igraph framework
#' @param z igraph dataset to be converted into visNetwork
#' @keywords igraph, melt
#' @export
#' @examples
#' igraphme(meltme_product)
igraphme <- function(z){
  convert <- igraph::graph_from_data_frame(z, directed = FALSE, vertices = NULL)
  return(convert)
}
