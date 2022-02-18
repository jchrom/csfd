user_summary_scrape <- function(html) {

  tibble::tibble(

    nick = html %>%
      html_element("h1") %>%
      html_text2(),

    id = csfd_page_id(html),

    info = html %>%
      html_element(".user-profile-content > p") %>%
      html_text2(),

    rank = html %>%
      html_element(".ranking") %>%
      csfd_int(),

    points = html %>%
      html_element(".ranking-points") %>%
      csfd_int(),

    joined = html %>%
      html_element(".user-profile-footer-left") %>%
      html_text2() %>%
      csfd_date()
  )
}

user_genres_scrape = function(html) {

  li <- html_elements(html, ".genres-switch > li")

  tibble::tibble(
    genre = html_text2(li),
    percent = csfd_percent(li)
  )
}

user_types_scrape = function(html) {

  li <- html_elements(html, ".types-switch > li")

  tibble::tibble(
    type = html_text2(li),
    percent = csfd_percent(li)
  )
}

user_origins_scrape = function(html) {

  li <- html_elements(html, ".origins-switch > li")

  tibble::tibble(
    origin = html_text2(li),
    percent = csfd_percent(li)
  )
}

csfd_percent <- function(node) {
  node %>%
    html_element("div > span") %>%
    html_attr("style") %>%
    csfd_dbl(0.01)
}
