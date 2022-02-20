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
      csfd_href(),

    released = tr %>%
      html_elements("h3 > span") %>%
      csfd_int(),

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

    page_n = csfd_page_id(html),

    page_prev = html %>%
      html_element("a.page-prev") %>%
      html_attr("href"),

    page_next = html %>%
      html_element("a.page-next") %>%
      html_attr("href")
  )
}

