film_summary_scrape <- function(html) {

  title <- html %>%
    html_element(".film-header-name > h1") %>%
    html_text2()

  id <- csfd_canonical_id(html)

  genre <- html %>%
    html_element(".film-info-content > .genres") %>%
    html_text2()

  details <- html %>%
    html_element(".film-info-content > .origin") %>%
    html_text2() %>%
    film_details_parse()

  tibble::as_tibble(c(title = title, id = id, genre = genre, details))
}

film_details_parse <- function(x) {

  origin <- "([^,]+)"
  spacer <- "[^0-9]*"
  num    <- "([0-9]+)"
  dash   <- "[^[:ascii:]]"

  released  <- num
  ended     <- num

  time_min <- "([0-9]+(?! ))"
  time_max <- "([0-9]+(?= min))"
  time_alt <- num

  pattern <- "{origin}{spacer}{released}{dash}?{ended}?{spacer}{num}? ?(h|x)? ?{num}?{spacer}{time_min}?{spacer}{time_max}?{spacer}{time_alt}?"

  matches <- stringr::str_match_all(x, glue::glue(pattern))[[1]]

  m <- matches[, -1, drop = TRUE]

  out <- list(

    origin   = m[1],
    released = m[2],
    ended    = m[3],

    time_min = if (is.na(m[7])) m[6] else m[7],
    time_max = if (is.na(m[8])) m[6] else m[8],
    time_alt = m[9],

    time_total = switch(
      m[5],
      x = prod(as.integer(m[c(4, 6)])),
      h = as.integer(m[4]) * 60 + as.integer(m[6]),
      m[4]
    )
  )

  out[2:6] <- lapply(out[2:6], as.integer)

  out
}

film_titles_scrape <- function(html) {

  tibble::tibble(

    main = html %>%
      html_element(".film-header-name > h1") %>%
      html_text2(),

    title = html %>%
      html_elements(".film-header-name > ul > li") %>%
      html_text2() %>%
      stringr::str_remove(" \\([^\\)]+\\)$"),

    origin = html %>%
      html_elements(".film-header-name > ul > li > img") %>%
      html_attr("title")
  )
}

film_plots_scrape <- function(html) {

  plots <- html %>%
    html_element(".body--plots") %>%
    html_elements("div.plot-full, div.plots > div")

  tibble::tibble(

    text = plots %>%
      html_elements("p") %>%
      html_text2() %>%
      # Trailing author name in parentheses.
      stringr::str_remove(" ?(\\.{3})? ?\\([^\\)]+\\)$"),

    author = plots %>%
      html_element("em") %>%
      html_text2() %>%
      parentheses_outer_strip(),

    author_id = plots %>%
      html_element("em > a") %>%
      html_href_id()
  )
}

film_ranks_scrape <- function(html) {

  tibble::tibble(

    rated = html %>%
      html_element(".ratings-btn > a > span") %>%
      html_int(),

    rating = html %>%
      html_element(".box-rating > .rating-average") %>%
      html_text2() %>%
      str_extract_percent(),

    fans = html %>%
      # Ensure element is not selected if greyed out (return NA instead).
      html_element("li[class='tab-nav-item fans-btn'] > a > span") %>%
      html_int(),

    ranking = html %>%
      html_element(".box-rating > .ranking > a") %>%
      html_int(),
  )
}

film_jobs_scrape <- function(html) {

  div <- html_elements(html, ".film-info-content > .creators > div")

  times_div <- div %>%
    lapply(html_elements, "a:not([href='#'])") %>%
    lengths()

  tibble::tibble(

    job_title = div %>%
      html_element("h4") %>%
      html_text2() %>%
      stringr::str_remove(":$") %>%
      rep(times_div),

    creator = div %>%
      html_elements("a:not([href='#'])") %>%
      html_text2(),

    creator_id = div %>%
      html_elements("a:not([href='#'])") %>%
      html_href_id()
  )
}

film_cast_scrape <- function(html) {

  articles <- html %>%
    html_element("section.box-film-creator > div.box-content") %>%
    html_elements("article")

  ul <- html_element(articles, "div.nostyle > ul")

  h3 <- html_elements(ul, "li > h3")

  rep_along_times <- function(x, node) {
    rep(x, xml2::xml_length(node))
  }

  tibble::tibble(

    actor = articles %>%
      html_element("h3 > a") %>%
      html_text2() %>%
      rep_along_times(ul),

    actor_id = articles %>%
      html_element("h3 > a") %>%
      html_href_id() %>%
      rep_along_times(ul),

    origin = articles %>%
      html_element("p > span.info") %>%
      html_text2() %>%
      rep_along_times(ul),

    best_titles = h3 %>%
      html_element("a.film-title-name") %>%
      html_text2(),

    best_id = h3 %>%
      html_element("a.film-title-name") %>%
      html_href_id(),

    best_year = h3 %>%
      html_element("span > span.info") %>%
      html_int()
  )
}

film_releases_scrape = function(html) {

  li <- html %>%
    html_element(".box-premieres > .box-content > ul") %>%
    html_elements("li")

  tibble::tibble(

    where = li %>%
      html_element("p") %>%
      html_text2() %>%
      stringr::str_remove_all("( od$)"),

    date = li %>%
      html_element("span[title]") %>%
      html_text2() %>%
      str_extract_date(),

    distributor = li %>%
      html_element("span[title]") %>%
      html_text2() %>%
      stringr::str_remove("^[^ ]+ ")
  )
}

film_tags_scrape <- function(hmtl) {

  tags <- html %>%
    html_element("section.box > div:contains(Tagy)") %>%
    xml2::xml_parent() %>%
    html_elements("a")

  tibble::tibble(
    tag = html_text2(tags),
    tag_id = html_attr(tags, "href") %>% stringr::str_remove("^/")
  )
}

tv_show_episodes_scrape <- function(html) {

  tr <- html %>%
    html_element("div.movie-profile--tab-episodes") %>%
    html_elements("tr")

  tv_show_name <- html %>% html_element("h1") %>% html_text2()

  se <- html_episode_info(tr)

  tibble::tibble(

    title = tr %>%
      html_elements("h3 > a") %>%
      html_text2() %>%
      stringr::str_remove(paste0(tv_show_name, " - ")),

    title_id = tr %>%
      html_elements("h3 > a") %>%
      html_href_id(),

    season = se$season,

    episode = se$episode,

    rating = tr %>%
      html_element("td.td-rating") %>%
      html_text2() %>%
      str_extract_percent()
  )
}

tv_show_season_episodes_scrape <- function(html) {

  ul <- html_element(html, "div.film-episodes-list > ul")

  se <- html_episode_info(ul)

  tibble::tibble(

    title = ul %>%
      html_elements("h3 > a") %>%
      html_text2(),

    title_id = ul %>%
      html_elements("h3 > a") %>%
      html_href_id(),

    season = se$season,

    episode = se$episode
  )
}

html_episode_info <- function(node) {

  se <- node %>%
    html_elements("span.film-title-info > span.info") %>%
    html_text2()

  season = se %>%
    stringr::str_match_all("S?([0-9]+)?E([0-9]+)") %>%
    vapply(`[`, "", , 2, drop = TRUE)

  season[is.na(season)] <- 1

  episode <- se %>%
    stringr::str_match_all("S?([0-9]+)?E([0-9]+)") %>%
    vapply(`[`, "", , 3, drop = TRUE)

  list(season = as.integer(season), episode = as.integer(episode))
}

