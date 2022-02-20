#' Initialize A New Scraper
#'
#' Create a new `<csfd_scraper>` object. It is best to use [csfd_fetch()] instead
#' of initializing directly.
#'
#' @details
#' The scraper object includes a set of scraper functions, each of which returns
#' a data frame. Trigger scraping by indexing into the object as you would into
#' a list.
#'
#' The selection of scraper functions will vary, depending on the type of page.
#' For example, user pages don't have _Tags_ but film pages often do,
#' so the `tags` scraper will be available if you downloaded a film page but not
#' if you downloaded a user page.
#'
#' A scraper may sometimes return a zero-row data frame. For example,
#' while appropriate for films, _Tags_ section may still be missing even from
#' a film page, if no tag has been added yet to that particular film.
#'
#' @param resp An object of class `<httr2_response>`.
#'
#' @return A `<csfd_scraper>`.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' trek <- csfd_fetch("/film/9834-star-trek-film/prehled/")
#' trek
#'
#' # Extract data
#' trek$summary
#' trek$ratings
#' }
csfd_scraper <- function(resp) {

  page <- csfd_path_parse(resp)

  env <- new_scraper_env(resp)

  fun <- scraper_list_select(page$type, page$path)

  scraper_list_allocate(env, fun)
}

new_scraper_env <- function(resp) {

  check_response(resp)

  scraper <- rlang::env()

  # Make the scraper available to itself (and to the active binding `html`).
  scraper$.enclos <- rlang::env()
  scraper$.enclos$scraper <- scraper

  scraper$body <- read_body(resp$body)

  scraper$path <- substr(resp$url, 20, nchar(resp$url))

  scraper$date <- date_parse(resp$headers$date)

  makeActiveBinding(
    sym = "html",
    fun = rlang::set_env(html, scraper$.enclos),
    env = scraper
  )

  lockBinding("html", scraper)

  structure(scraper, class = "csfd_scraper")
}

read_body <- function(body) {

  if (is.raw(body)) {
    return(body)
  }

  rlang::inform(paste("Reading `body` from disk:", body))

  body <- paste(readLines(body), collapse = "")

  charToRaw(body)
}

check_response <- function(resp) {

  if (!inherits(resp, "httr2_response")) {
    rlang::abort("`resp` must inherit from <httr2_response>")
  }

  allowed <- paste0("www.csfd.", c("cz", "sk"))

  if (!any(httr2::url_parse(resp$url)$hostname %in% allowed)) {
    rlang::abort("`resp` must originate in CSFD https://www.csfd.cz or .sk")
  }

  NULL
}

scraper_list_select <- function(type, path) {

  by_type <- scraper_list_build()[[type]]

  by_path <- by_type[vapply(names(by_type), grepl, FALSE, x = path)]

  unlist(unname(by_path), recursive = FALSE, use.names = TRUE)
}

scraper_list_allocate <- function(env, bindings) {

  for (sym in names(bindings)) {

    fun <- bindings[[sym]]

    environment(fun) <- env

    # Ensure the active binding won't allow passing a value.
    formals(fun) <- NULL

    makeActiveBinding(sym = sym, fun = fun, env = env)
    lockBinding(sym = sym, env = env)
  }

  env
}

html <- function() {

  if (is_valid_pointer(scraper$.html$node)) {
    return(scraper$.html)
  }

  scraper$.html <- xml2::read_html(scraper$body)
  scraper$.html
}

is_valid_pointer <- function(x) {

  if (!identical(typeof(x), "externalptr")) {
    return(FALSE)
  }

  empty <- methods::new("externalptr")

  attributes(empty) <- attributes(x)

  !identical(x, empty)
}

date_parse <- function(x) {

  locale_old <- Sys.getlocale("LC_TIME")

  if (.Platform$OS.type == "unix") {
    suppressWarnings(Sys.setlocale("LC_TIME", "en_US.UTF-8"))
  } else {
    Sys.setlocale("LC_TIME", "English")
  }

  out <- as.POSIXct(x, format = "%a, %e %b %Y %H:%M:%S", tz = "GMT")

  Sys.setlocale("LC_TIME", locale_old)

  # In case this didn't not work on some exotic OS.
  if (is.na(out)) return(x)

  # No need to display the date in GMT.
  attr(out, "tzone") <- NULL

  out
}
