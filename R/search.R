#' Advanced Search
#'
#' Execute [advanced search](https://www.csfd.cz/podrobne-vyhledavani) with your
#' own parameters.
#'
#' @details
#' Searching for creators and tags is a bit cumbersome, because they require
#' special formatting. To search for a creator, use `id:First Last (nar. YYYY, Country)`, where:
#'
#' * `id` is a numerical identifier (e.g. `453` in `tvurce/453-leonard-nimoy`)
#' * `First Last` should be a full name, e.g. `"Robert Downey Jr."`
#' * `YYYY` is a year of birth
#' * `Country` is a country of origin, e.g. `"USA"`
#'
#' To search a tag, use `id:Name`, where:
#'
#' * `id` is a numerical identifier
#' * `name` is a full name of the tag, e.g. `"Star Trek"`
#'
#' This information is included on creator and film pages, and can be scraped
#' using [csfd_fetch()].
#'
#' Examples:
#'
#' ```
#' fields = list(
#'   actor = c(
#'     "453:Leonard Nimoy (nar. 1931, USA)",
#'     "486:William Shatner (nar. 1931, Kanada)"
#'   ),
#'   tag = c("17:budoucnost", "800:Star Trek")
#' )
#'
#' csfd_search(checkbox = "film", fields = fields)
#' ```
#'
#' To see all available options, see [entities]. For example, to get a list
#' of available checkboxes, call `entities$checkbox`.
#'
#' @param checkbox A vector of checkbox names to check.
#' @param genre,origin A named list of choices.
#'
#'   * use `list(any = c("Fantasy", "Film-Noir"))` to get Fantasy or Film-Noir
#'   * use `list(all = c("Fantasy", "Film-Noir"))` to get films which are both
#'     Fantasy AND Film-Noir
#'   * use `list(exact = c("Fantasy", "Film-Noir"))` for films which are exactly
#'     Fantasy AND Film-Noir and nothing else
#'   * use `list(not = c("Fantasy", "Film-Noir"))` to exclude Fantasy and Film-Noir
#'     from the selection. Useful in combination with `any/all`.
#'
#' Same logic applies to origin.
#'
#' @param rating,year A list of 2 numbers.
#'
#'   * use `c(50, 80)` to get films rated between 50 and 80
#'   * use `2020` to get films released in 2020 or later
#'   * for `rating`, numbers between 0 and 1 also work, e.g. `c(0.75, 0.9)` is
#'     equivalent to `c(75, 90)`
#'
#' @param fields A named list of entity identifiers. See **Details.**
#' @param page Which page to retrieve.
#' @param sort Results sorting.
#' @param quiet If `TRUE`, request status will not be printed on the console.
#'
#' @return An object of class [csfd_scraper].
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Search for films and TV shows made in the US only, either drama or sci-fi,
#' # rated at least 80 and made between 2000 and 2022 (included).
#' search <- csfd_search(
#'   checkbox = c("film", "tv_show"),
#'   origin = list(exact = "USA"),
#'   genre = list(any = c("Drama", "Sci-Fi")),
#'   rating = 80,
#'   year = c(2000, 2022)
#' )
#'
#' search$results
#' }
csfd_search <- function(checkbox = NULL, genre = NULL, origin = NULL, rating = NULL,
                        year = NULL, fields = NULL, page = 1,
                        sort = c("rated", "rating_average", "rating_average_asc",
                                 "fanclub_count", "year", "year_asc"),
                        quiet = FALSE) {

  sort <- rlang::arg_match(sort)

  form <- list(
    checked = form_option_checkbox(checkbox),
    year    = form_option_year(year),
    rating  = form_option_rating(rating),
    genre   = form_option_genre(genre),
    origin  = form_option_origin(origin),
    fields  = form_option_fields(fields)
  )

  flat <- unlist(unname(form), recursive = FALSE, use.names = TRUE)

  submission <- c(flat, list(`_do` = "filmsForm-submit"))

  csfd_form_submit(submission, query = list(page = page, sort = sort),
                   quiet = quiet)
}

csfd_form_submit <- function(body, query, quiet = FALSE) {

  req_search <- csfd_request_new("POST") %>%
    httr2::req_url_path_append("/podrobne-vyhledavani") %>%
    httr2::req_body_form(list(default = NULL)) %>%
    httr2::req_options(
      # Without the cookie, the server will not provide useful redirect URL. This
      # is set by default in httr 1.4.2 but not in httr2 0.1.1.
      cookie = "_nss=1",
      # Redirection is stopped to perform the final search manually - this is to
      # enable pagination, which can only be done on the redirect URL.
      followlocation = 0)

  # In httr2 0.1.1, duplicate names are silently dropped. This is a problem,
  # because the checkboxes on the search page do not have unique names. So let's
  # inject the data directly.
  req_search$body$data <- body

  # Get the redirect to the results page.
  resp <- csfd_request_perform(req_search, quiet = quiet)

  location <- httr2::resp_header(resp, "location")

  req_redirect <- csfd_request_new("GET") %>%
    httr2::req_url(location) %>%
    httr2::req_url_query(!!!query) %>%
    # Applying the rate limit to the first request was probably enough.
    httr2::req_throttle(1)

  resp <- csfd_request_perform(req_redirect, quiet = quiet)

  csfd_scraper(resp)
}

form_option_checkbox <- function(checkbox = NULL) {

  if (is.null(checkbox)) return()

  if (!is.character(checkbox)) {
    rlang::abort("`checkbox` must be a character vector of checkbox labels")
  }

  form_option_check("checkbox", checkbox)

  out <- entities$checkbox[checkbox]

  names(out)[names(out) %in% names(entities$type)] <- "type[]"

  names(out)[names(out) %in% names(entities$conditions)] <- "conditions[]"

  out
}

form_option_rating <- function(rating = NULL) {

  if (is.null(rating)) return()

  if (!is.numeric(rating) || length(rating) > 2 || any(rating > 100)) {
    rlang::abort("`rating` must be a vector of one or two percentages, either 1 to 100 or 0 to 1")
  }

  # This is to allow percentage as proportions, i.e. 0.5 would equal to 50%.
  if (all(rating < 1)) rating <- rating * 100

  names(rating) <- c("rating_from", "rating_to")[seq_along(rating)]

  lapply(rating, as.integer)
}

form_option_year <- function(year = NULL) {

  if (is.null(year)) return()

  if (!is.numeric(year) || length(year) > 2 || any(nchar(year) != 4)) {
    rlang::abort("`year` must be a vector of one or two four-digit years")
  }

  names(year) <- c("year_from", "year_to")[seq_along(year)]

  lapply(year, as.integer)
}

form_option_genre <- function(genre = NULL) {

  if (is.null(genre)) return()

  out <- map2(genre, names(genre), form_option_pick, what = "genre")

  unlist(out, recursive = FALSE, use.names = TRUE)
}

form_option_origin <- function(origin) {

  if (is.null(origin)) return()

  out <- map2(origin, names(origin), form_option_pick, what = "origin")

  unlist(out, recursive = FALSE, use.names = TRUE)
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

form_option_pick <- function(vals, what = c("origin", "genre"),
                             mode = c("any", "all", "not", "exact")) {

  what <- rlang::arg_match(what)

  mode <- rlang::arg_match(mode)

  form_option_check(what, vals)

  label <- if (identical(mode, "not")) "[exclude][]" else "[include][]"

  picks <- unique(entities[[what]][vals])

  names(picks) <- rep(paste0(what, label), length(picks))

  if (identical(mode, "not")) {
    return(picks)
  }

  type <- list(what = list(exact = "1", all = "2", any = "3")[[mode]])

  names(type) <- paste0(what, "[type]")

  c(type, picks)
}

form_option_fields <- function(fields) {

  if (is.null(fields)) return()

  form_option_check("fields", names(fields))

  form_option_vals <- function(vals) {

    if (!is.character(vals)) {
      rlang::abort("`fields` must be comprised of character vectors")
    }

    paste(vapply(vals, curl_escape2, ""), ",", sep = "", collapse = "")
  }

  lapply(fields, form_option_vals)
}

curl_escape2 <- function(x) {
  # Need to escape everything except for the first colon, to comply with requested
  # format on CSFD. I don't even.
  sub("%3A", ":", curl::curl_escape(x), fixed = TRUE)
}

form_option_check <- function(what, nms) {

  unknown <- setdiff(nms, names(entities[[what]]))

  if (length(unknown)) {

    options <- paste(unknown, collapse = ', ')
    accepts <- paste('"', names(entities[[what]]), '"', collapse = ', ', sep = "")

    if (nchar(accepts) > 500) {
      accepts <- glue::glue("see `entities${what}`")
    }

    error <- "unknown `{what}` option: \"{options}\""
    infor <- "accepted values are: {accepts}"

    rlang::abort(c(glue::glue(error), i = glue::glue(infor)))
  }

  invisible()
}
