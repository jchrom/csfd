#' Issue A Scrape Request
#'
#' Download a page from the [Czech and Slovak Film Database](https://www.csfd.cz)
#' (CSFD).
#'
#' @details
#'
#' This function fetches HTML from a single URL. The resulting object contains
#' a set of fields for scraping different parts of the page. For all available
#' scrapers, see [csfd_scraper_list].
#'
#' Note that rate is throttled at 15 requests per minute.
#'
#' @param url A CSFD URL, incl. a partial one, e.g. `"/film/9834-star-trek-film"`
#' @param quiet If `TRUE`, request status will not be printed on the console.
#'
#'   This indicates whether the request is being executed or has been completed
#'   and with what status. It also prints out the response URL which may be
#'   different from the request URL, because the server always returns a complete
#'   URL rather than just the resource identifier.
#'
#' @return An object of class [csfd_scraper].
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Partial URL works:
#' trek79 <- csfd_fetch("/film/9834-star-trek-film/prehled/")
#' trek79
#'
#' # Even shorter:
#' trek79 <- csfd_fetch("film/9834")
#' trek79
#'
#' # Extract data like you would extract elements from a list:
#' trek79$details
#' trek79$reviews
#' trek79$ratings
#'
#' # Careful with TV show seasons, they have two different identifiers - you
#' # want the second one. For example:
#' # /film/68990-star-trek-hluboky-vesmir-devet/494010-serie-1/prehled/
#' ds9 <- csfd_fetch("film/494010")
#' }
csfd_fetch <- function(url, quiet = FALSE) {

  url <- httr2::url_parse(url)

  req <- csfd_request_new("GET")

  if (length(url$path)) {
    req <- httr2::req_url_path(req, url$path)
  }

  if (length(url$query)) {
    req <- httr2::req_url_query(req, !!!url$query)
  }

  resp <- csfd_request_perform(req, quiet = quiet)

  csfd_scraper(resp)
}
