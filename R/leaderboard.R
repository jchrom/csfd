leaderboard_titles_scrape <- function(html) {

  div <- html_elements(html, "article > div")

  span <- div %>%
    html_element("p > span") %>%
    html_text2() %>%
    stringr::str_match_all("(.+), ([^,]+)") %>%
    lapply(`[`, , -1, drop = TRUE)

  df <- tibble::tibble(

    title = div %>%
      html_element("header > h3 > a") %>%
      html_text2(),

    title_id = div %>%
      html_element("header > h3 > a") %>%
      html_href_id(),

    rank = div %>%
      html_element("header > h3 > span.film-title-user") %>%
      html_int(),

    year = div %>%
      html_element("header > h3 > span.film-title-info") %>%
      html_int(),

    origin = vapply(span, `[[`, "", 1),

    genre = vapply(span, `[[`, "", 2),

    rating = div %>%
      html_element("div.rating-average") %>%
      html_text2() %>%
      str_extract_percent(),

    rated = div %>%
      html_element("div.rating-total") %>%
      html_int()
  )

  cast <- html_nested_extract(div, id = df$title_id, element = "p:contains('\tHr')")

  names(cast) <- c("title_id", "cast")

  direction <- html_nested_extract(div, id = df$title_id, element = "p:contains('\tRe')")

  names(direction) <- c("title_id", "direction")

  df %>%
    merge(direction, by = "title_id") %>%
    merge(cast, by = "title_id") %>%
    tibble::as_tibble()
}

html_nested_extract <- function(div, id, element) {

  times <- div %>%
    html_element(element) %>%
    xml2::xml_length()

  a <- div %>%
    html_elements(element) %>%
    html_elements("a")

  df <- tibble::tibble(
    creator = html_text2(a),
    creator_id = html_href_id(a)
  )

  df_split <- split(df, rep(id, times = times))

  tibble::tibble(
    id = names(df_split),
    data = df_split
  )
}

leaderboard_creators_scrape <- function(html) {

  cols <- html_elements(html, "div.column-half, div.column-third")

  h2 <- cols %>% html_element("h2") %>% html_text2()

  html_leaderboard_extract %>%
    mapply(cols, h2, SIMPLIFY = FALSE) %>%
    do.call(what = rbind)
}

html_leaderboard_extract <- function(column, id) {

  tibble::tibble(

    leaderboard = id,

    creator = column %>%
      html_elements("header > h3 > a") %>%
      html_text2(),

    creator_id = column %>%
      html_elements("header > h3 > a") %>%
      html_href_id(),

    rank = column %>%
      html_elements("header > h3 > span.user-title-position") %>%
      html_int(),

    fans = column %>%
      html_elements("p.p-rating > a") %>%
      html_int(),

    origin = column %>%
      html_elements("p > span.info") %>%
      html_text2(),
  )
}
