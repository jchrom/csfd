user_details_scrape <- function(html) {

  tibble::tibble(

    nick = html %>%
      html_element("h1") %>%
      html_text2(),

    id = html %>%
      html_element("head > link[rel='canonical']") %>%
      html_attr("href") %>%
      regx_extract_id(),

    info = html %>%
      html_element(".user-profile-content > p") %>%
      html_text2(),

    rank = html %>%
      html_element(".ranking") %>%
      html_text2() %>%
      regx_extract_int(),

    points = html %>%
      html_element(".ranking-points") %>%
      html_text2() %>%
      regx_extract_int(),

    joined = html %>%
      html_element(".user-profile-footer-left") %>%
      html_text2() %>%
      str_extract("[[:digit:]\\.]{10}") %>%
      as.Date("%d.%m.%Y")
  )
}

user_genres_scrape = function(html) {

  li <- html_elements(html, ".genres-switch > li")

  tibble::tibble(
    genre = html_text2(li),
    percent = user_stats_percent(li)
  )
}

user_types_scrape = function(html) {

  li <- html_elements(html, ".types-switch > li")

  tibble::tibble(
    type = html_text2(li),
    percent = user_stats_percent(li)
  )
}

user_origins_scrape = function(html) {

  li <- html_elements(html, ".origins-switch > li")

  tibble::tibble(
    origin = html_text2(li),
    percent = user_stats_percent(li)
  )
}

user_stats_percent <- function(node) {
  node %>%
    html_element("div > span") %>%
    html_attr("style") %>%
    regx_extract_percent()
}
