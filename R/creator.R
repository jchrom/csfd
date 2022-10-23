creator_details_scrape <- function(html) {

  pattern <- "(nar|zem)\\. ([0-9\\.]+)[\n\t]*(.[0-9]+ let.)?[\n\t]*([[:alpha:], ]+)"

  info <- html %>%
    html_elements(".creator-profile-content > p") %>%
    html_text() %>%
    str_match_all(pattern) %>%
    lapply(`[`, , c(3, 5), drop = TRUE) %>%
    unlist(use.names = FALSE)

  tibble::tibble(

    id = html %>%
      html_element("head > link[rel='canonical']") %>%
      html_attr("href") %>%
      regx_extract_id(),

    name = html %>%
      html_element("h1") %>%
      html_text2(),

    origin = info[2],
    born   = info[1] %>% as.Date("%d.%m.%Y"),
    died   = info[3] %>% as.Date("%d.%m.%Y"),

    rank = html %>%
      html_element(".ranking > a") %>%
      html_text2() %>%
      regx_extract_int()
  )
}

creator_filmography_scrape <- function(html) {

  tr <- html %>%
    html_elements("div.creator-filmography > section.box") %>%
    html_elements("tr:not(.tr-banner-desktop):not(.tr-banner-mobile)")

  a = html_element(tr, "td.name > h3 > a, td.episode > h3 > a")

  out <- tibble::tibble(

    year = tr %>%
      html_element("td.year") %>%
      html_text2() %>%
      as.integer() %>%
      fill_na(),

    type = tr %>%
      html_element("th") %>%
      html_text2() %>%
      fill_na(),

    title = a %>% html_text2(),

    title_id = a %>%
      html_attr("href") %>%
      str_extract("film/[0-9]+"),

    note = tr %>%
      html_element("td.name > h3 > span.film-title-info > span") %>%
      html_text2() %>%
      str_remove("^\\(") %>%
      str_remove("\\)$")
  )

  # Some records are unfortunately populated by ajax scripts, and therefore
  # missing from the source.
  if (any(is.na(out$title))) {
    message(
      "Records populated by ajax scripts may be missing from source.",
      "\n(Spotted ", sum(is.na(out$title)), " missing values.)"
    )
  }

  out[!is.na(out$title), ]
}

creator_bio_scrape <- function(html) {

  div <- html_element(html, ".creator-tabs")

  tibble::tibble(

    text = div %>%
      html_elements(".article-content > p:not(.signature)") %>%
      html_text2() %>%
      paste0(collapse = "\n"),

    autor_nick = div %>%
      html_element(".article-content > p.signature") %>%
      html_text2(),

    autor_user = div %>%
      html_element(".article-content > p.signature > a") %>%
      html_attr("href")
  )
}
