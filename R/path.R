# Determine the type of page, so we know what scrapers to allocate. Example
# output: list(type = "film", path = "/prehled"). This understands internal types,
# i.e. /film may become "tv_show" etc.
csfd_path_parse <- function(resp) {

  if (!inherits(resp, "httr2_response")) {
    rlang::abort("`resp` must inherit from <httr2_response>")
  }

  path <- httr2::url_parse(resp$url)$path

  pattern <- "^/([^/]+)(/[0-9]+-[^/]+)?(/[0-9]+-[^/]+)?(/.+)?"

  matches <- stringr::str_match_all(path, pattern)[[1]]

  matches[is.na(matches)] <- ""

  page <- list(
    type = matches[, 2, drop = TRUE],
    path = matches[, 5, drop = TRUE] %>% stringr::str_remove("/$")
  )

  if (identical(page$type, "film")) {
    page$type <- csfd_film_subtype_parse(resp)
  }

  page
}

csfd_film_subtype_parse <- function(resp) {

  film_subtype <- resp %>%
    httr2::resp_body_html() %>%
    html_element(".film-header-name > span[class='type']") %>%
    html_text2() %>%
    # Replace non-ascii characters to appease R-CMD-check.
    stringr::str_replace_all("[^[:ascii:]]", ".")

  if (is.na(film_subtype)) {
    film_subtype <- "(film)"
  }

  c("(epizoda)" = "tv_show_episode",
    "(s.rie)"   = "tv_show_season",
    "(s.ria)"   = "tv_show_season", # bratia Slovaci oziju!
    "(seri.l)"  = "tv_show",
    "(film)"    = "film"
  )[[film_subtype]]
}
