search_results_scrape <- function(html) {

  tr <- html %>%
    html_element("table") %>%
    html_elements("tr")

  tibble::tibble(

    title = tr %>%
      html_elements("h3 > a") %>%
      html_text2(),

    title_id = tr %>%
      html_elements("h3 > a") %>%
      html_href_id(),

    released = tr %>%
      html_elements("h3 > span") %>%
      html_int(),

    genre = tr %>%
      html_elements("td.genre") %>%
      html_text2(),

    origin = tr %>%
      html_elements("td.origin") %>%
      html_text2()
  )
}

search_paginator_scrape <- function(html) {

  tibble::tibble(

    page_n = html %>%
      html_element("head > link[rel='canonical']") %>%
      html_attr("href") %>%
      stringr::str_extract("(?<=[?&]page=)[0-9]+") %>%
      as.integer(),

    prev_url = html %>%
      html_element("a.page-prev") %>%
      html_attr("href"),

    next_url = html %>%
      html_element("a.page-next") %>%
      html_attr("href")
  )
}

