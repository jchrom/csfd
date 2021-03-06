#' Issue A Scrape Request
#'
#' Download a single CSFD page and return a scraper object. Rate is throttled
#' at 10 requests per minute.
#'
#' @param url A CSFD URL, incl. a partial one, e.g. `"/film/9834-star-trek-film"`
#' @param quiet If `TRUE`, request status will not be printed on the console.
#'
#'   This indicates whether the request is being executed or has been completed
#'   and with what status. It also prints out the response URL, which may be
#'   different from the request URL, because the server always returns a complete
#'   URL rather than its short version (e.g. `tvurce/453`).
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
