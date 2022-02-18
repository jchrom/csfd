#' Perform A Scraper Request
#'
#' Download a CSFD page and return a scraper object.
#'
#' @param url A CSFD URL, even a partial one, e.g. `"/film/9834-star-trek-film"`
#' @param quiet If `TRUE`, request status will not be printed on the console.
#'
#' @return An object of class [csfd_scraper].
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Partial URL works.
#' trek <- csfd_fetch("/film/9834-star-trek-film/prehled/")
#' trek
#'
#' # Even shorter:
#' trek <- csfd_fetch("film/9834")
#' trek
#'
#' # Extract data like you would extract elements from a list.
#' trek$summary
#' trek$reviews
#' trek$ratings
#' }
csfd_fetch <- function(url, quiet = FALSE) {

  url <- httr2::url_parse(url)

  req <- "https://www.csfd.cz" %>%
    httr2::request() %>%
    httr2::req_user_agent(pkg_ver(c("csfd", "httr2"))) %>%
    httr2::req_throttle(10/60) %>%
    httr2::req_method("get")

  if (length(url$path)) {
    req <- httr2::req_url_path(req, url$path)
  }

  if (length(url$query)) {
    req <- rlang::inject(httr2::req_url_query(req, !!!url$query))
  }

  cat_status(req, quiet)

  resp <- httr2::req_perform(req)

  cat_status(resp, quiet)

  csfd_scraper(resp)
}

pkg_ver <- function(pkg) {
  paste(pkg, utils::installed.packages()[pkg, "Version"], sep = "/", collapse = " ")
}

cat_status <- function(x, quiet) {

  if (quiet) return()

  now <- format(Sys.time(), format = "%H:%M:%S")

  url <- substr(x$url, 20, 200)

  if (inherits(x, "httr2_request")) {
    cat_line(inline("{now} [ requesting ] {url}"))
    return()
  }

  cat_line(clear())
  cat_line(rn(inline("{now} [ {.field {httr2::resp_status_desc(x)}} ] {url}")))
}

cat_line <- function(x) {
  cat(x, sep = "")
}

clear <- function() {
  paste0("\r", stringr::str_pad("", cli::console_width()))
}

rn <- function(x) {
  c("\r", x, "\n")
}

inline <- function(...) {
  cli::ansi_strtrim(cli::format_inline(..., .envir = rlang::caller_env()))
}
