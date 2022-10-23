#' Initialize A New Scraper
#'
#' Create a new `<csfd_scraper>` object. This is mostly for debugging purposes,
#' normally it is best to use [csfd_fetch()].
#'
#' @details
#' The scraper object includes a set of scraper functions, each of which returns
#' a data frame. Trigger scraping by indexing into the object as you would into
#' a list.
#'
#' The selection of scraper functions will vary, depending on what page you
#' have fetched. For example, user pages don't have tags but film pages often
#' do. As a result, `tags` scraper will be available if you downloaded a `/film`
#' page, but not if you downloaded an `/uzivatel` page.
#'
#' A scraper may return a data frame with no rows. For example, while appropriate
#' for films, tags may still be missing even from a film page, if no tag has
#' been added yet to that particular film.
#'
#' @param resp An object of class [httr2::response].
#'
#' @return A `<csfd_scraper>`.
#' @export
csfd_scraper <- function(resp) {

  check_response(resp)

  scraper_funs <- scraper_select(resp)
  scraper_env  <- scraper_create(resp)

  for (name in names(scraper_funs)) {

    binding <- scraper_funs[[name]]

    # Ensure the active binding won't allow passing a value.
    formals(binding) <- NULL

    makeActiveBinding(
      sym = name,
      fun = rlang::set_env(binding, scraper_env),
      env = scraper_env
    )

    lockBinding(sym = name, env = scraper_env)
  }

  scraper_env
}

scraper_create <- function(resp) {

  env <- rlang::env()

  # Make the scraper available to itself (and to the active binding `html`).
  env$.enclos <- rlang::env()
  env$.enclos$scraper <- env

  env$.resp <- resp
  env$.date <- date_parse(resp$headers$date)

  makeActiveBinding(
    sym = "html",
    fun = rlang::set_env(html, env$.enclos),
    env = env
  )

  lockBinding("html", env)

  structure(env, class = "csfd_scraper")
}

check_response <- function(resp) {

  if (!inherits(resp, "httr2_response")) {
    rlang::abort("`resp` must inherit from <httr2_response>")
  }

  permitted <- paste0("www.csfd.", c("cz", "sk"))
  incoming  <- httr2::url_parse(resp$url)$hostname

  if (!any(incoming %in% permitted)) {
    rlang::abort("`resp` must originate in CSFD (www.csfd.cz or .sk)")
  }

  NULL
}

scraper_select <- function(resp) {

  path <- httr2::url_parse(resp$url)$path

  # Extract e.g. "/film", "/tvurce", "/zebricky/filmy", "/podrobne-vyhledavani".
  type <- str_extract(path, "^/[^/]+(/[^0-9/\\?]+)?")

  if (identical(type, "/film")) {
    type <- resp %>% httr2::resp_body_html() %>% film_type_scrape()
  }

  # Extracts a single category of scrapers matching `type`, e.g. "/film",
  # "/vyhledavani", "/zebricky".
  scrapers <- csfd_scraper_list[vapply(
    X = names(csfd_scraper_list),
    FUN = grepl,
    FUN.VALUE = FALSE,
    type,
    perl = TRUE
  )][[1]]

  # Extracts one or more subcategories of scrapers matching `path`, e.g. ".*",
  # "/(serialy|filmy)", "/(prehled|recenze)".
  matching <- scrapers[vapply(
    X = names(scrapers),
    FUN = grepl,
    FUN.VALUE = FALSE,
    path,
    perl = TRUE
  )]

  matching %>% unname() %>% unlist(recursive = FALSE)
}

html <- function() {

  if (is_valid_pointer(scraper$.html$node)) {
    return(scraper$.html)
  }

  scraper$.html <- xml2::read_html(scraper$.resp$body)
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

  # In case this didn't work on some exotic OS.
  if (is.na(out)) return(x)

  # No need to display the date in GMT.
  attr(out, "tzone") <- NULL

  out
}
