# GLOBAL VARS ------------------------------------------------------------------

globalVariables(".")

# MISC -------------------------------------------------------------------------

fill_na <- function(x) {
  Reduce(
    f = function(l, r) if (is.na(r)) l else r,
    x = x,
    accumulate = TRUE
  )
}

drop_na <- function(x) {
  x[!is.na(x)]
}

# REGEX ------------------------------------------------------------------------

# Integers on CSFD might be any of "123", "12 345", "(52)".
regx_extract_int <- function(text) {
  text %>%
    str_extract("[0-9 ]+") %>%
    str_remove(" ") %>%
    as.integer()
}

# The challenge: Some URLs use two CSFD identifiers in one href, and not all
# href strings contain CSFD identifiers.
regx_extract_id <- function(text) {
  text %>%
    # STEP #1: Extract all identifiers if present, and return unchanged input
    # string otherwise. Examples:
    # /film/70788-star-trek/459309-chleb-a-hry   -> "/film/70788` /459309"
    # /film/335282-star-trek-do-neznama/prehled/ -> "/film/335282` "
    # https://www.ceskatelevize.cz               -> left as is
    str_replace(
      pattern = ".*(film/|uzivatel/|tvurce/)([0-9]+)[^/]+(/[0-9]+)?.*",
      replacement = "\\1\\2`\\3"
    ) %>%
    # STEP #2: Remove the middle identifier (if any) ending with "`" (chosen
    # because ` is an illegal URL character, so it won't occur in URLs without
    # CSFD identifiers).
    str_remove("([0-9]+`/|`$)")
}

# Extract percentage, which may look like "74%" or "74,5%" and return a number
# between 0 and 100.
regx_extract_percent <- function(text) {
  text %>%
    str_extract("[0-9]+,?[0-9]*(?=%)") %>%
    str_replace(",", ".") %>%
    as.double()
}
