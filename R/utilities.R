#' Create new site from template
#'
#' This function creates a new site from a template stored within the package.
#'
#' @param directory_name The directory where the new site should be located.
#' @return The output is a directory containing the template version of a site.
#' @export
#' @examples \dontrun{
#' directory_name("test_location")
#' }
#'

initial_build <- function(directory_name){
  if (dir.exists(directory_name)){
    stop(paste("Error: the directory -", directory_name, "- already exists."))

  }
  directory_to_copy <- system.file("template", package = "nms4r")
  dir.create(directory_name)
  file.copy(directory_to_copy, directory_name, recursive = T)
}
