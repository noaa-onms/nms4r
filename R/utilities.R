#' Create new site from template
#'
#' This function creates a new website from a template stored within the package.
#'
#' @param dir_path The directory path where the new site should be located.
#' @return The output is a directory containing the template version of a site.
#' @export
#' @examples \dontrun{
#' directory_name("test_location")
#' }
#'

create_website <- function(dir_path, open = rlang::is_interactive()){
  # dir_path <- here("vignettes/MyFirstInfographiq"); open = T
  # unlink(dir_path, recursive = T)

  if (dir.exists(dir_path)){
    stop(paste("Error: the directory -", dir_path, "- already exists."))

  }
  dir_template <- system.file("template_website", package = "nms4r")

  file.copy(dir_template, dirname(dir_path), recursive = T)
  dir_tmp <- file.path(dirname(dir_path), basename(dir_template))
  file.rename(dir_tmp, dir_path)

  if (open)
    servr::httd(dir_path)
}
