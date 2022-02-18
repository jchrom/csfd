creator_summary_scrape <- function(html) {

  info <- html %>%
    html_elements(".creator-profile-content > p") %>%
    html_text2() %>%
    paste(collapse = "")

  tibble::tibble(

    id = csfd_page_id(html),

    name = html %>%
      html_element("h1") %>%
      html_text2(),

    born = info %>%
      stringr::str_extract("nar\\. [0-9\\.]+") %>%
      csfd_date(),

    died = info %>%
      stringr::str_extract("zem\\. [0-9\\.]+") %>%
      csfd_date(),

    rank = html %>%
      html_element(".ranking > a") %>%
      csfd_int()
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
      na_fill_down(),

    type = tr %>%
      html_element("th") %>%
      html_text2() %>%
      na_fill_down(),

    title = a %>% html_text2(),

    title_id = a %>% csfd_href(),

    note = tr %>%
      html_element("td.name > h3 > span.film-title-info > span") %>%
      html_text2() %>%
      parentheses_outer_strip()
  )

  # Some records are unfortunately populated by ajax scripts, and therefore are
  # missing from the source.
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
