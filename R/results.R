search_titles_scrape <- function(html) {

  tr <- html %>%
    html_element("table > tbody") %>%
    html_elements("tr")

  tibble::tibble(

    title = tr %>%
      html_element("h3 > a") %>%
      html_text2(),

    title_id = tr %>%
      html_element("h3 > a") %>%
      html_attr("href") %>%
      str_extract("film/[0-9]+"),

    released = tr %>%
      html_element("h3 > span") %>%
      html_text2() %>%
      str_extract("[:digit:]{4}") %>%
      as.integer(),

    genre = tr %>%
      html_element("td.genre") %>%
      html_text2(),

    origin = tr %>%
      html_element("td.origin") %>%
      html_text2()
  )
}

search_creators_scrape <- function(html) {

  tr <- html %>%
    html_element("table > tbody") %>%
    html_elements("tr")

  tibble::tibble(

    creator = tr %>%
      html_element("td > a") %>%
      html_text2(),

    creator_id = tr %>%
      html_element("td > a") %>%
      html_attr("href") %>%
      str_extract("tvurce/[0-9]+"),

    born = tr %>%
      html_element("td.author-birthday") %>%
      html_text2() %>%
      str_extract("[:digit:]{4}") %>%
      as.integer(),

    job = tr %>%
      html_element("td.author-dos") %>%
      html_text2()
  )
}

search_paginator_scrape <- function(html) {

  tibble::tibble(

    prev_url = html %>%
      html_element("a.page-prev") %>%
      html_attr("href"),

    next_url = html %>%
      html_element("a.page-next") %>%
      html_attr("href")
  )
}

