#' Advanced Search
#'
#' @description
#'
#' Search pages on CSFD.
#'
#' * use `csfd_search_titles()` to search for [titles](https://www.csfd.cz/podrobne-vyhledavani/)
#' * use `csfd_search_creators()` to search for [creators](https://www.csfd.cz/podrobne-vyhledavani/tvurci/)
#' * use `csfd_search_ranks()` to search [rankings](https://www.csfd.cz/zebricky/vlastni-vyber/)
#'
#' Browse available form fields using [csfd_form_browse()], e.g. `csfd_form_browse("titles")`.
#'
#' @section Results:
#'
#' The result of the search call is a scraper object with two items:
#'
#' 1. `results`: A data frame with search results, including unique identifier.
#' 2. `paginator`: A data frame with `next_url` column, which redirects
#'    to the next page of results; if a missing value, it indicates there are
#'    no more results.
#'
#' @param form Which list of form fields to show.
#' @param type,conditions A vector of checkbox names/dropdown options.
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
#'   * use `list(not = c("Fantasy", "Film-Noir"))` to exclude Fantasy and Film-Noir
#'     from the selection. Useful in combination with `any/all`.
#'
#' @param released A vector of 1 to 2 numbers. Examples:
#'
#'   * use `2020` to get films released in 2020 or later
#'   * use `c(1940, 1960)` to get creators born between 1940 and 1960
#'
#' @param born,died A vector of date strings. Examples:
#'
#'   * use `c("01.01.1950", "31.12.2022)` to search creators born between
#'     these dates
#'   * use `"500 BC"` to search creators born 500 BC - useful for authors such
#'     as Euripides
#'
#' @param born_in,died_in A single country name.
#' @param rating A vector of two numbers.
#'
#'   * use `c(50, 80)` to get films rated between 50 and 80
#'   * for `rating`, numbers between 0 and 1 also work, e.g. `c(0.75, 0.9)` is
#'     equivalent to `c(75, 90)`
#'
#' @param fields A named list of numerical identifiers. Examples:
#'
#'  * use `list(actor = c(483, 486), tag = 800)` to search for titles starring
#'    Leonard Nimoy (`tvurce/453`) and William Shatner (`tvurce/486`), tagged
#'    "Star Trek" (tag id `800`).
#'
#' @param gender One of: `"male"`, `"female"`, `1`, `2` or `NULL`.
#' @param page Which page to retrieve.
#' @param sort Results sorting.
#' @param quiet If `TRUE`, request status will not be printed on the console.
#'
#' @return An object of class [csfd_scraper].
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
#' csfd_fetch(re$paginator$next_url)
#' }
#' @name csfd_search
NULL

#' @export
#' @rdname csfd_search
csfd_search_titles <- function(type = NULL, conditions = NULL, genre = NULL,
                               origin = NULL, rating = NULL, released = NULL,
                               fields = NULL,
                               page = 1,
                               sort = c("rated",
                                        "rating_average",
                                        "rating_average_asc",
                                        "fanclub_count",
                                        "year",
                                        "year_asc"),
                               quiet = FALSE) {

  subm <- list(

    option_simple(type, "type[]", opts_titles),

    option_simple(conditions, "conditions[]", opts_titles),

    genre %>%
      map2(names(.), option_nested, field = "genre",  form = opts_titles) %>%
      unname() %>%
      unlist(recursive = FALSE),

    origin %>%
      map2(names(.), option_nested, field = "origin",  form = opts_titles) %>%
      unname() %>%
      unlist(recursive = FALSE),

    list(rating_from = rating[1], rating_to = rating[2]) %>% na_drop(),

    list(year_from = released[1], year_to = released[2]) %>% na_drop(),

    option_string(fields),

    "_do" = "filmsForm-submit"
  )

  csfd_form_submit(
    form  = compact(unlist(subm, recursive = FALSE)),
    query = list(page = page, sort = rlang::arg_match(sort)),
    path  = "/podrobne-vyhledavani",
    quiet = quiet
  )
}

#' @export
#' @rdname csfd_search
csfd_search_creators <- function(type = NULL, conditions = NULL, born = NULL,
                                 born_in = NULL, died = NULL, died_in = NULL,
                                 gender = NULL,
                                 page = 1,
                                 sort = c("fanclub_count",
                                          "birth_date_asc",
                                          "birth_date"),
                                 quiet = FALSE) {

  subm <- list(

    option_simple(type, "type[]", opts_creators),

    option_simple(conditions, "conditions[]", opts_creators),

    option_simple(gender, "gender", opts_creators),

    option_simple(born_in, "birth_country_id", opts_creators),

    list(birth_from = born[1], birth_to = born[2]) %>% na_drop(),

    option_simple(died_in, "death_country_id", opts_creators),

    list(death_from = died[1], death_to = died[2]) %>% na_drop(),

    "_do" = "creatorsForm-submit"
  )

  csfd_form_submit(
    form  = compact(unlist(subm, recursive = FALSE)),
    query = list(page = page, sort = rlang::arg_match(sort)),
    path  = "/podrobne-vyhledavani/tvurci",
    quiet = quiet
  )
}

#' @export
#' @rdname csfd_search
csfd_search_ranks <- function(type = NULL, origin = NULL, released = NULL,
                              fields = NULL, genre = NULL,
                              page = 1,
                              quiet = FALSE) {

  subm <- list(

    option_simple(type, "type", opts_ranks),

    option_simple(origin, "origin", opts_ranks),

    list(year_from = released[1], year_to = released[2]) %>% na_drop(),

    option_string(fields),

    option_simple(genre, "genre[]", opts_ranks),

    "_do" = "specificSelectionForm-submit"
  )

  csfd_form_submit(
    form  = compact(unlist(subm, recursive = FALSE)),
    query = list(page = page),
    path  = "/zebricky/vlastni-vyber",
    quiet = quiet
  )
}

#' @export
#' @rdname csfd_search
csfd_form_browse <- function(form = c("titles", "creators", "ranks")) {

  switch(rlang::arg_match(form), titles = opts_titles, creators = opts_creators,
         ranks = opts_ranks)
}

option_simple <- function(input, field, form) {

  if (is.null(input)) return()

  if (is.character(input)) {

    opts <- form[[field]][input]

    names(opts) <- rep(field, length(opts))

    return(opts)
  }

  csfd_abort("`{substitute(input)}` must be a character vector")
}

option_nested <- function(input, mode, field, form) {

  if (is.null(input)) return()

  if (!mode %in% c("any", "all", "exact", "not")) {
    csfd_abort('`mode` must be one of: "any", "all", "exact", "not"')
  }

  label <- if (identical(mode, "not")) "[exclude][]" else "[include][]"

  vals <- form[[field]][input]

  names(vals) <- paste0(field, rep(label, length(input)))

  type <- list()

  type[[paste0(field, "[type]")]] <- list(exact = "1", all = "2", any = "3")[[mode]]

  c(type, vals)
}

option_string <- function(input, field, form) {

  if (is.null(input)) return()

  if (all(rlang::is_named(input), is.list(input), vapply(input, is.numeric, FALSE))) {

    # The server needs these extra bit after every numerical identifier, or it
    # won't return results. I guess there is a regex going on there.
    out <- lapply(input, paste, ":.,", sep = "", collapse = "")
    return(out)
  }

  csfd_abort("`{substitute(input)}` must be a named list of integer identifiers")
}

csfd_form_submit <- function(form, query, path, quiet = FALSE) {

  req_form <- csfd_request_new("POST") %>%
    httr2::req_url_path_append(path) %>%
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
  req_form$body$data <- form

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
