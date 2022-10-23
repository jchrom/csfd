leaderboard_titles_scrape <- function(html) {

  div <- html_elements(html, "article > div")

  span <- div %>%
    html_element("p > span") %>%
    html_text2() %>%
    str_match_all("(.+), ([^,]+)") %>%
    lapply(`[`, , -1, drop = TRUE)

  id <- div %>%
    html_element("header > h3 > a") %>%
    html_attr("href") %>%
    str_extract("film/[0-9]+")

  cast <- scrape_nested_creators(
    div = div,
    id = id,
    element = unicode_unescape("p:contains('\tHraj\u00ed:')")
  )

  names(cast) <- c("title_id", "cast")

  direction <- scrape_nested_creators(
    div = div,
    id = id,
    element = unicode_unescape("p:contains('Re\u017eie:')")
  )

  names(direction) <- c("title_id", "direction")

  df <- tibble::tibble(

    title = div %>%
      html_element("header > h3 > a") %>%
      html_text2(),

    title_id = id,

    rank = div %>%
      html_element("header > h3 > span.film-title-user") %>%
      html_text2() %>%
      str_extract("[:digit:]+") %>%
      as.integer(),

    year = div %>%
      html_element("header > h3 > span.film-title-info") %>%
      html_text2() %>%
      str_extract("[:digit:]+") %>%
      as.integer(),

    origin = vapply(span, `[[`, "", 1),

    genre = vapply(span, `[[`, "", 2),

    rating = div %>%
      html_element("div.rating-average") %>%
      html_text2() %>%
      regx_extract_percent(),

    rated = div %>%
      html_element("div.rating-total") %>%
      html_text2() %>%
      regx_extract_int()
  )

  out <- df %>%
    merge(direction, by = "title_id", all.x = TRUE) %>%
    merge(cast, by = "title_id", all.x = TRUE) %>%
    tibble::as_tibble()

  out[order(out$rank), ]
}

unicode_unescape <- function(x) {
  Encoding(x) <- "Unicode"
  x
}

scrape_nested_creators <- function(div, id, element) {

  times <- div %>%
    html_element(element) %>%
    xml2::xml_length()

  a <- div %>%
    html_elements(element) %>%
    html_elements("a")

  df <- tibble::tibble(
    creator = html_text2(a),
    creator_id = a %>%
      html_attr("href") %>%
      regx_extract_id()
  )

  df_split <- split(df, rep(id, times = times))

  tibble::tibble(
    id = names(df_split),
    data = df_split
  )
}

# Scrape leaderboard with fan counts.
leaderboard_fans_scrape <- function(html) {

  cols <- html_elements(
    x = html,
    css = "div.column-half:contains(Nej), div.column-third:contains(Nej)"
  )

  out <- lapply(cols, function(column) {

    id <- html_element(column, "h2") %>% html_text2()
    df <- scrape_leaderboard_column(column, id)

    tibble::add_column(
      df, fans = column %>%
        html_elements("p.p-rating > a") %>%
        html_text2() %>%
        regx_extract_int()
    )
  })

  do.call(rbind.data.frame, out)
}


# Scrape leaderboard with average ratings.
leaderboard_ratings_scrape <- function(html) {

  cols <- html_elements(
    x = html,
    css = "div.column-half:contains(filmy), div.column-third:contains(filmy)"
  )

  out <- lapply(cols, function(column) {

    id <- html_element(column, "h2") %>% html_text2()
    df <- scrape_leaderboard_column(column, id)

    tibble::add_column(
      df, rating = column %>%
        html_elements("p.p-rating > strong") %>%
        html_text2() %>%
        regx_extract_percent()
    )
  })

  do.call(rbind.data.frame, out)
}

scrape_leaderboard_column <- function(column, id) {

  out <- tibble::tibble(

    leaderboard = id,

    creator = column %>%
      html_elements("header > h3 > a") %>%
      html_text2(),

    creator_id = column %>%
      html_elements("header > h3 > a") %>%
      html_attr("href") %>%
      regx_extract_id(),

    rank = column %>%
      html_elements("header > h3 > span.user-title-position") %>%
      html_text2() %>%
      regx_extract_int(),

    origin = column %>%
      html_elements("p > span.info") %>%
      html_text2()
  )
}
