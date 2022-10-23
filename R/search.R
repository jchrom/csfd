#' Advanced Search
#'
#' @description
#'
#' Search pages on CSFD.
#'
#' * use `csfd_search_titles()` to search for [titles](https://www.csfd.cz/podrobne-vyhledavani/)
#' * use `csfd_search_creators()` to search for [creators](https://www.csfd.cz/podrobne-vyhledavani/tvurci/)
#' * use `csfd_search_ranks()` to search [rankings](https://www.csfd.cz/zebricky/vlastni-vyber/)
#' * use `csfd_search_options()` to review search parameters
#'
#' @section Results:
#'
#' The result of the search call is a [csfd_scraper] object with two items:
#'
#' 1. `results`: A data frame with search results, including unique identifier.
#' 2. `paginator`: A data frame with `next_url` column, which redirects
#'    to the next page of results; if a missing value, it indicates there are
#'    no more results.
#'
#' @param type,additional A vector of checkbox names/dropdown options.
#' @param genre,origin For `csfd_search_ranks()`, a string, e.g. `"Polsko"`
#'   or `"tv_show"`.
#'
#'   For `csfd_search_titles()`, a named list. Examples:
#'
#'   * use `list(any = c("Fantasy", "Film-Noir"))` to get Fantasy or Film-Noir
#'   * use `list(all = c("Fantasy", "Film-Noir"))` to get films which are both
#'     Fantasy AND Film-Noir
#'   * use `list(exact = c("Fantasy", "Film-Noir"))` for films which are exactly
#'     Fantasy AND Film-Noir and nothing else
#'
#'   If you use either `any` or `all`, you can combine it with `not` like so:
#'
#'   * use `list(not = c("Horor", "Sci-fi"))` to exclude Horror and Sci-fi
#'     genres from the selection.
#'
#' @param released,rating A vector of 1 to 2 integers (or `NULL`). Examples:
#'
#'   * use `2020` to get films released in 2020 or later
#'   * use `c(50, 80)` to get films rated between 50% and 80%
#'
#' @param born,died A vector of date strings. Examples:
#'
#'   * use `c("01.01.1950", "31.12.2022)` to search creators born between
#'     these dates
#'   * use `"500 BC"` to search creators born 500 BC - useful for creators such
#'     as Euripides
#'
#' @param born_in,died_in A single country name.
#' @param fields A named list of numerical identifiers. Examples:
#'
#'  * use `list(actor = c(483, 486), tag = 800)` to search for titles starring
#'    Leonard Nimoy (`tvurce/453`) and William Shatner (`tvurce/486`), tagged
#'    "Star Trek" (tag id `800`).
#'
#' @param gender One of: `"male"`, `"female"`, `1`, `2` or `NULL`.
#' @param actor,director A vector of integer identifiers, obtained from creator's
#'   page URL.
#' @param page Which page to retrieve.
#' @param sort How to sort results.
#' @param quiet If `TRUE`, request status will not be printed on the console.
#' @param domain,element,choices Useful for displaying options for search forms.
#'   Example:
#'
#'   * use `csfd_search_options("title", "genre", Inf)` for a complete list
#'     of valid genres
#'   * use `csfd_search_options("title", "origin", "USA")` to see if `"USA"`
#'     is a valid origin
#' @param single If `TRUE`, only vectors of length 1 will be accepted.
#'
#' @return An object of class [csfd_scraper] or, in case of `csfd_search_options()`,
#'   a list.
#' @examples
#'
#' \dontrun{
#'
#' # Search for films and TV shows made in the US only, either drama or sci-fi
#' # (and possibly other things except horror), rated at least 80 and made
#' # between 2000 and 2022 (included).
#' re <- csfd_search_titles(
#'   type = c("film", "tv_show"),
#'   origin = list(exact = "USA"),
#'   genre = list(any = c("Drama", "Sci-Fi"), not = c("horor")),
#'   rating = 80,
#'   released = c(2000, 2022)
#' )
#'
#' re$results
#'
#' # As there are more results than would fit a single page, retrieve the next
#' # page URL and use csfd_fetch() to get those.
#' re$paginator$next_url %>% csfd_fetch()
#'
#' # Search rankings of movies filmed in Asia between 1980-2022, starring Donnie
#' # Yen and Michelle Yeoh.
#' re <- csfd_search_ranks(
#'   type = "film",
#'   origin = "Asie",
#'   released = c(1980, 2022),
#'   # This vector does not have to be named, but it's nice for clarity.
#'   actor = c("Donnie Yen" = 39, "Michelle Yeoh" = 1818),
#'   genre = "Akční"
#' )
#'
#' re$results
#' }
#' @name csfd_search
NULL

#' @export
#' @rdname csfd_search
csfd_search_titles <- function(type = NULL,
                               additional = NULL,
                               genre = NULL,
                               origin = NULL,
                               released = NULL,
                               rating = NULL,
                               fields = NULL,
                               page = 1,
                               sort = c("rated",
                                        "rating_average",
                                        "rating_average_asc",
                                        "fanclub_count",
                                        "year",
                                        "year_asc"),
                               quiet = FALSE) {


  genre$mode <- setdiff(names(genre), "not")
  genre$values <- c(genre$any, genre$all, genre$exactly)

  origin$mode <- setdiff(names(origin), "not")
  origin$values <- c(origin$any, origin$all, origin$exactly)

  released <- c(
    year_from = released[1],
    year_to = released[2]
  )

  rating <- c(
    rating_from = rating[1],
    rating_to = rating[2]
  )

  opts <- function(...) {
    csfd_search_options("title", ...)
  }

  subm <- c(

    "type[]"            = opts("type", type),
    "conditions[]"      = opts("additional", additional),
    "genre[type]"       = opts("mode",  genre$mode),
    "genre[include][]"  = opts("genre", genre$values),
    "genre[exclude][]"  = opts("genre", genre$not),
    "origin[type]"      = opts("mode",   origin$mode),
    "origin[include][]" = opts("origin", origin$values),
    "origin[exclude][]" = opts("origin", origin$not),

    drop_na(released),
    drop_na(rating),

    vapply(fields, collapse, ""),

    "_do" = "filmsForm-submit"
  )

  # `c()` hates duplicate names, it will append numbers to them.
  names(subm) <- names(subm) %>% str_remove("[0-9]+$")

  csfd_form_submit(
    form  = as.list(subm),
    query = list(page = page, sort = rlang::arg_match(sort)),
    path  = "/podrobne-vyhledavani",
    quiet = quiet
  )
}

#' @export
#' @rdname csfd_search
csfd_search_creators <- function(type = NULL,
                                 additional = NULL,
                                 born = NULL,
                                 born_in = NULL,
                                 died = NULL,
                                 died_in = NULL,
                                 gender = NULL,
                                 page = 1,
                                 sort = c("fanclub_count",
                                          "birth_date_asc",
                                          "birth_date"),
                                 quiet = FALSE) {

  born <- c(birth_from = born[1], birth_to = born[2])
  died <- c(death_from = died[1], death_to = died[2])

  opts <- function(...) {
    csfd_search_options("creator", ...)
  }

  subm <- c(
    "type[]"           = opts("type", type),
    "conditions[]"     = opts("additional", additional),
    "gender"           = opts("gender", gender),
    "birth_country_id" = opts("origin", born_in),
    "death_country_id" = opts("origin", died_in),

    drop_na(born),
    drop_na(died),

    "_do" = "creatorsForm-submit"
  )

  names(subm) <- names(subm) %>% str_remove("[0-9]+$")

  csfd_form_submit(
    form  = as.list(subm),
    query = list(page = page, sort = rlang::arg_match(sort)),
    path  = "/podrobne-vyhledavani/tvurci",
    quiet = quiet
  )
}

#' @export
#' @rdname csfd_search
csfd_search_ranks <- function(type = NULL,
                              origin = NULL,
                              released = NULL,
                              actor = NULL,
                              director = NULL,
                              genre = NULL,
                              page = 1,
                              quiet = FALSE) {

  released <- c(
    year_from = released[1],
    year_to = released[2]
  )

  opts <- function(...) {
    csfd_search_options("rank", ...)
  }

  subm <- c(
    "type"     = opts("type",   type),
    "origin"   = opts("origin", origin),
    "genre[]"  = opts("genre",  genre),

    "director" = collapse(director),
    "actor"    = collapse(actor),

    drop_na(released),

    "_do" = "specificSelectionForm-submit"
  )

  names(subm) <- names(subm) %>% str_remove("[0-9]+$")

  csfd_form_submit(
    form  = as.list(subm),
    query = list(page = page),
    path  = "/zebricky/vlastni-vyber",
    quiet = quiet
  )
}

# Collapse lists of numerical identifiers (to search for specific creators).
collapse <- function(x) {
  if (!length(x)) return()
  # Not sure what regex magic happens over there on the server side,
  # but appending ":.," after each numerical identifier seems required to make
  # this work.
  paste(x, ":.,", sep = "", collapse = "")
}

#' @export
#' @rdname csfd_search
csfd_search_options <- function(domain, element, choices = Inf, single = FALSE) {

  if (!length(choices)) return()

  opts <- .opts[[c(domain, element)]]

  if (identical(choices, Inf)) return(opts)

  if (single && length(choices > 1)) {
    rlang::abort("`choices` must be a single string, because of `single=TRUE`")
  }

  vals <- tolower(choices)
  miss <- setdiff(vals, names(opts))

  if (length(miss)) {

    msg <- sprintf("the following are not valid values for `%s`:", element)
    names(miss) <- rep("i", length(miss))

    rlang::abort(c(msg, dQuote(miss, FALSE)))
  }

  unname(opts[vals])
}

csfd_form_submit <- function(form, query, path, quiet = FALSE) {

  req_form <- csfd_request_new("POST") %>%
    httr2::req_url_path_append(path) %>%
    httr2::req_body_form(!!!form) %>%
    httr2::req_options(
      # Without the cookie, the server will not provide useful redirect URL. This
      # is not a problem in httr 1.4.2 as it preserves cookies between requests.
      # But httr2 doesn't (as of 0.2.1) so it has to be set directly.
      cookie = "_nss=1",
      # Redirection is stopped to perform the final search manually - this is to
      # enable pagination, which can only be done on the redirect URL.
      followlocation = 0)

  # Get the redirect to the results page.
  resp <- csfd_request_perform(req_form, quiet = quiet)

  location <- httr2::resp_header(resp, "location")

  if (is.null(location)) {
    rlang::abort("redirect location is `NULL` (invalid search request?)")
  }

  req_results <- csfd_request_new("GET") %>%
    httr2::req_url(location) %>%
    httr2::req_url_query(!!!query) %>%
    # Applying the rate limit to the first request was probably enough.
    httr2::req_throttle(1)

  resp <- csfd_request_perform(req_results, quiet = quiet)

  csfd_scraper(resp)
}
