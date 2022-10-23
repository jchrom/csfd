film_details_scrape <- function(html) {

  title <- html %>%
    html_element(".film-header-name > h1") %>%
    html_text2()

  id <- html %>%
    html_element("head > link[rel='canonical']") %>%
    html_attr("href") %>%
    regx_extract_id()

  genre <- html %>%
    html_element(".film-info-content > .genres") %>%
    html_text2()

  details <- html %>%
    html_element(".film-info-content > .origin") %>%
    html_text2() %>%
    film_details_parse()

  img <- html %>%
    html_element(".film-about > .film-posters") %>%
    html_element("img")

  # If multiple image sources available, get the largest one.
  if (xml2::xml_has_attr(img, "srcset")) {
    srcset <- img %>%
      html_attr("srcset") %>%
      strsplit(split = " ([0-9]+x,? ?)") %>%
      unlist()
    poster <- paste0("https:", srcset[length(srcset)])
  } else {
    poster <- paste0("https:", html_attr(img, "src"))
  }

  h3_count <- html_element(html, ".box-header > h3:contains(Epizody)")

  if (!is.na(h3_count)) {
    parts <- h3_count %>%
      xml2::xml_find_first("span[last()]") %>%
      html_text2() %>%
      str_extract("[0-9]+") %>%
      as.integer()
  } else  {
    # The old way of specifying number of episodes might be used, e.g.:
    # "Česko, 2007, 286x60 min" in the info field. If even that is missing,
    # return NA.
    parts <- html %>%
      html_element(".film-info-content > .origin") %>%
      html_text2() %>%
      str_extract(", ([0-9]+x[0-9]+ min)") %>%
      str_extract("[0-9]+") %>%
      as.integer()
  }

  tibble::as_tibble(c(
    title  = title,
    id     = id,
    genre  = genre,
    poster = poster,
    parts  = parts,
    details
  ))
}

# Unfortunately this information only exists in the form or semi-structured
# text string, so getting it out is a lot of work. It is not pretty.
film_details_parse <- function(text) {

  # Note the use of \u2013 and \u2014 - unicodes for "en dash" and "em dash"
  # because CSFD cannot be trusted with using those systematically.
  origin <- str_match(text, "(.+), \\(?[0-9]{4}")[, 2]
  years  <- str_match(text, ".*, \\(?([0-9]{4})[\u2013\u2014\\-]?([0-9]{4})?")

  released <- as.integer(years[, 2])
  ended <- as.integer(years[, 3])

  time_min     <- NA_integer_
  shortest_min <- NA_integer_
  longest_min  <- NA_integer_

  if (grepl("Minut", text)) {

    # This extracts episode and total duration from records such as:
    #
    # "Česko, (2008–2011), 22 h 38 min (Minutáž: 25–29 min)" (film/241931)
    # "Československo, 1980, 6 h 17 min (Minutáž: 29 min)" (film/71926)
    # "Československo, 1966, 59 min (Minutáž: 7–9 min)" (film/230273)

    h <- str_match(text, " ([0-9]+) h ")[, 2] %>% as.integer()
    m <- str_match(text, " ([0-9]+) min ")[, 2] %>% as.integer()

    time_min <- (h * 60) + m

    shortest_min <- as.integer(
      str_match(text, ": ([0-9]+)(?=[\u2013\u2014\\-]| min\\))")[, 2]
    )

    longest_min  <- as.integer(str_match(text, "([0-9]+) min\\)")[, 2])

  } else if (grepl("[0-9]+x[0-9]+ min", text)) {

    # TV show records like "Česko, 2007, 286x60 min" (film/226809),
    # "Československo, 1963, 49x3-7 min" (film/291738)
    number_x_time <- as.integer(
      str_match(text, "([0-9]+)x([0-9]+)([\u2013\u2014\\-])?([0-9]+)? min")[, c(2:3, 5)]
    )

    shortest_min <- number_x_time[2]
    longest_min  <- number_x_time[3]

    if (is.na(longest_min)) {
      longest_min <- shortest_min
      time_min <- prod(number_x_time[1], longest_min) %>% as.integer()
    } else {
      # Cannot calculate sum if the duration is not fixed.
      time_min <- NA_integer_
    }

  } else if (grepl("x?[0-9]+[\u2013\u2014\\-][0-9]+ min", text)) {

    # TV show records like: "Česko, 2005, 40–60 min" (film/215263)
    times <- str_match(text, "([0-9]+)[\u2013\u2014\\-]([0-9]+) min *$")[, 2:3]

    shortest_min <- times[1] %>% as.integer()
    longest_min  <- times[2] %>% as.integer()

  } else if (grepl("[0-9]+ min *$", text)) {

    # Movie/episode records like: "USA / Německo, 2009, 127 min" (film/222972)
    time_min <- str_match(text, "([0-9]+) min *$")[, 2] %>% as.integer()
  }

  list(
    # Should exist for any tv show, tv show season, episode, movie.
    origin = origin,             # country/countries of origin (string)
    released = released,         # release year (int)

    # Not always present.
    ended = ended,               # end year (int)
    time_min = time_min,         # total time in minutes (int)

    # Only for TV shows, and even there not always present.
    shortest_min = shortest_min, # shortest episode duration in minutes (int)
    longest_min = longest_min    # longest episode duration in minutes (int)
  )
}

film_titles_scrape <- function(html) {

  tibble::tibble(

    main = html %>%
      html_element(".film-header-name > h1") %>%
      html_text2(),

    title = html %>%
      html_elements(".film-header-name > ul > li") %>%
      html_text2() %>%
      str_remove(" \\([^\\)]+\\)$"),

    language = html %>%
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
      str_remove(" ?(\\.{3})? ?\\([^\\)]+\\)$"),

    author = plots %>%
      html_element(".span-more-small") %>%
      html_text2() %>%
      # Unfortunately, there is no better way than to get the author name
      # from the text, where it is wrapped in parentheses.
      str_remove_all("(^\\(|\\)$)"),

    author_id = plots %>%
      html_element(".span-more-small > a") %>%
      html_attr("href") %>%
      str_replace(".*(uzivatel/[0-9]+).*", "\\1")
  )
}

film_ranks_scrape <- function(html) {

  tibble::tibble(

    rated = html %>%
      html_element(".ratings-btn > a > span") %>%
      html_text2() %>%
      regx_extract_int(),

    rating = html %>%
      html_element(".box-rating > .film-rating-average") %>%
      html_text2() %>%
      regx_extract_percent(),

    fans = html %>%
      # Ensure element is not selected if greyed out (return NA instead).
      html_element("li[class='tab-nav-item fans-btn'] > a > span") %>%
      html_text2() %>%
      regx_extract_int(),

    best = html %>%
      html_element(".box-rating-withtabs") %>%
      html_element(".film-ranking > a:contains(nejlep)") %>%
      html_text2() %>%
      regx_extract_int(),

    worst = html %>%
      html_element(".box-rating-withtabs") %>%
      html_element(".film-ranking > a:contains(nejhor)") %>%
      html_text2() %>%
      regx_extract_int(),

    favourite = html %>%
      html_element(".box-rating-withtabs") %>%
      html_element(".film-ranking > a:contains(nejob)") %>%
      html_text2() %>%
      regx_extract_int(),
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
      str_remove(":$") %>%
      rep(times_div),

    creator = div %>%
      html_elements("a:not([href='#'])") %>%
      html_text2(),

    creator_id = div %>%
      html_elements("a:not([href='#'])") %>%
      html_attr("href") %>%
      regx_extract_id()
  )
}

film_cast_scrape <- function(html) {

  articles <- html %>%
    html_element("section.box-film-creator > div.box-content") %>%
    html_elements("article")

  ul <- html_element(articles, "div.nostyle > ul")

  ul_times <- xml2::xml_length(ul)

  h3 <- html_elements(ul, "li > h3")

  tibble::tibble(

    actor = articles %>%
      html_element("h3 > a") %>%
      html_text2() %>%
      rep(ul_times),

    actor_id = articles %>%
      html_element("h3 > a") %>%
      html_attr("href") %>%
      regx_extract_id() %>%
      rep(ul_times),

    origin = articles %>%
      html_element("p > span.info") %>%
      html_text2() %>%
      rep(ul_times),

    best_titles = h3 %>%
      html_element("a.film-title-name") %>%
      html_text2(),

    best_id = h3 %>%
      html_element("a.film-title-name") %>%
      html_attr("href") %>%
      regx_extract_id(),

    best_year = h3 %>%
      html_element("span > span.info") %>%
      html_text2() %>%
      regx_extract_int()
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
      str_remove_all("( od$)"),

    date = li %>%
      html_element("span[title]") %>%
      html_text2() %>%
      str_extract("[[:digit:]\\.]{10}") %>%
      as.Date("%d.%m.%Y"),

    distributor = li %>%
      html_element("span[title]") %>%
      html_text2() %>%
      str_remove("^[^ ]+ ")
  )
}

film_tags_scrape <- function(hmtl) {

  tags <- html %>%
    html_element("section.box > div:contains(Tagy)") %>%
    xml2::xml_parent() %>%
    html_elements("a")

  tibble::tibble(
    tag = html_text2(tags),
    tag_id = html_attr(tags, "href") %>% str_remove("^/")
  )
}

show_episodes_scrape <- function(html) {

  tr <- html %>%
    html_element("div.movie-profile--tab-episodes") %>%
    html_elements("tr")

  tv_show_name <- html %>% html_element("h1") %>% html_text2()

  se <- html_episode_info(tr)

  tibble::tibble(

    title = tr %>%
      html_elements("h3 > a") %>%
      html_text2() %>%
      str_remove(paste0(tv_show_name, " - ")),

    title_id = tr %>%
      html_elements("h3 > a") %>%
      html_attr("href") %>%
      regx_extract_id(),

    season = se$season,
    episode = se$episode,

    rating = tr %>%
      html_element("td.td-rating") %>%
      html_text2() %>%
      regx_extract_percent()
  )
}

season_episodes_scrape <- function(html) {

  ul <- html_element(html, "div.film-episodes-list > ul")

  se <- html_episode_info(ul)

  tibble::tibble(

    title = ul %>%
      html_elements("h3 > a") %>%
      html_text2(),

    title_id = ul %>%
      html_elements("h3 > a") %>%
      html_attr("href") %>%
      regx_extract_id(),

    season = se$season,

    episode = se$episode
  )
}

html_episode_info <- function(node) {

  se <- node %>%
    html_elements("span.film-title-info > span.info") %>%
    html_text2()

  season = se %>%
    str_match_all("S?([0-9]+)?E([0-9]+)") %>%
    vapply(`[`, "", , 2, drop = TRUE)

  season[is.na(season)] <- 1

  episode <- se %>%
    str_match_all("S?([0-9]+)?E([0-9]+)") %>%
    vapply(`[`, "", , 3, drop = TRUE)

  list(season = as.integer(season), episode = as.integer(episode))
}

film_type_scrape <- function(html) {

  type <- html %>%
    html_element(".film-header-name > span[class='type']") %>%
    html_text2() %>%
    # Replace non-ASCII characters "é", "á" with dots for easier handling.
    str_replace_all("[^[:ascii:]]", ".")

  switch(
    type,
    "(epizoda)" = "/episode",
    "(s.rie)"   = "/season",
    "(s.ria)"   = "/season", # handle csfd.sk version as well
    "(seri.l)"  = "/show",
    "/film"
  )
}
