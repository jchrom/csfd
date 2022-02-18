globalVariables(c(".", ".html", ".resp"))

csfd_date <- function(x, format = "%d.%m.%Y") {
  stringr::str_extract(x, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}") %>% as.Date(format)
}

csfd_int <- function(node) {
  html_text2(node) %>% stringr::str_remove_all("[^0-9]") %>% as.integer()
}

csfd_stars <- function(x) {
  sub("trash", "stars-0", x, fixed = TRUE) %>% substr(13, 100) %>% as.integer()
}

# Percentage may look like "74%" or "74,5%". Convert this into a double between
# 0 and 1 by setting an appropriate value of `prop`.
csfd_dbl <- function(x, prop = 1) {
  as.double(stringr::str_remove_all(x, "[^0-9]")) * prop
}

csfd_href <- function(node) {
  html_attr(node, "href") %>% csfd_id()
}

csfd_page_id <- function(html) {

  html %>%
    html_element("head > link[rel='canonical']") %>%
    html_attr("href") %>%
    substr(20, 200) %>%
    csfd_id()
}

# Extract identifiers from a vector of paths.
#
# Examples:
#
# * /film/70788-star-trek/459309-chleb-a-hry becomes film/459309
# * /film/335282-star-trek-do-neznama/prehled/ becomes film/335282
csfd_id <- function(path) {

  out <- path %>%
    stringr::str_match_all("/([^0-9]+)/?([0-9]+)?(-[^/]+)?/?([0-9]+)?") %>%
    lapply(`[`, , -c(1, 4)) %>%
    lapply(na_drop) %>%
    vapply(outer_paste, "")

  out[is.na(path)] <- NA_character_

  out
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
