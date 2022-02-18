.onLoad <- function(libname, pkgname) {
  csfd_scraper_list <<- scraper_list_build()
  invisible()
}
