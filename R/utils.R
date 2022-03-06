html_int <- function(node) {
  html_text2(node) %>% stringr::str_remove_all("[^0-9]") %>% as.integer()
}

# Star rating (1-5), with "trash" converted to zero.
html_class_stars_int <- function(node) {
  node %>%
    html_attr("class") %>%
    sub(pattern = "trash", replacement = "stars-0", fixed = TRUE) %>%
    substr(13, 13) %>%
    as.integer()
}

# Extract page id from a link.
html_href_id <- function(node) {
  html_attr(node, "href") %>% str_extract_id()
}

# Extract page id from a canonical link in the header.
csfd_canonical_id <- function(html) {
  html %>%
    html_element("head > link[rel='canonical']") %>%
    html_href_id()
}

# Extract identifiers from a vector of paths. Examples:
# - /film/70788-star-trek/459309-chleb-a-hry becomes film/459309
# - /film/335282-star-trek-do-neznama/prehled/ becomes film/335282
str_extract_id <- function(text) {

  path <- sub("(https*://)?www\\.csfd\\.(cz|sk)", "", text)

  bare <- sub("(\\?.*)$", "", path)

  out <- bare %>%
    stringr::str_match_all("/([^0-9]+)/?([0-9]+)?(-[^/]+)?/?([0-9]+)?") %>%
    lapply(`[`, , -c(1, 4)) %>%
    lapply(na_drop) %>%
    vapply(outer_paste, "")

  out[is.na(bare) | bare %in% c("/", "")] <- NA_character_

  out
}

# Extract percentage, which may look like "74%" or "74,5%" and return a number
# between 0 and 1.
str_extract_percent <- function(x) {

  dbl <- x %>%
    stringr::str_extract("[0-9]+,?[0-9]*(?=%)") %>%
    stringr::str_replace(",", ".") %>%
    as.double()

  dbl / 100
}

str_extract_date <- function(x, format = "%d.%m.%Y") {
  stringr::str_extract(x, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}") %>% as.Date(format)
}

na_drop <- function(x) x[!is.na(x)]

na_replace <- function(x, replacement) {
  x[is.na(x)] <- replacement
  x
}

na_fill_down <- function(x) {

  fill_down <- function(lhs, rhs) {
    if (is.na(rhs)) lhs else rhs
  }

  Reduce(fill_down, x = x, accumulate = TRUE)
}

outer_paste <- function(x) {
  paste0(x[1], if (length(x) > 1) x[length(x)])
}

parentheses_outer_strip <- function(x) {
  stringr::str_remove_all(x, "(^[\\(]|[\\)]$)")
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE, USE.NAMES = TRUE)
}

compact <- function(x) Filter(length, x)

csfd_abort <- function(msg, ..., .envir = parent.frame()) {

  i <- unlist(list(...), use.names = FALSE)

  if (length(i)) {
    names(i) <- rep("i", length(i))
  }

  rlang::abort(vapply(c(msg, i), glue::glue, "", .envir = .envir))
}

globalVariables(".")
