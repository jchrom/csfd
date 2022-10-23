misc_reviews_scrape <- function(html) {

  article <- html %>%
    html_elements("#snippet--comments, .user-reviews > .box-content, .box-reviews") %>%
    html_elements("article")

  out <- tibble::tibble(

    author = article %>%
      html_element("h3 > a") %>%
      html_text2(),

    author_id = article %>%
      html_element("h3 > a") %>%
      html_attr("href") %>%
      str_extract("(uzivatel|film)/[0-9]+"),

    stars = article %>%
      html_element(".star-rating > span") %>%
      html_attr("class") %>%
      str_replace("trash", "stars-0") %>%
      substr(13, 13) %>%
      as.integer(),

    submitted = article %>%
      html_element("time") %>%
      html_text2() %>%
      as.Date("%d.%m.%Y"),

    text = article %>%
      html_element("p") %>%
      html_text2()
  )

  # Let's make this applicable for reviews located either on /film pages
  # or /uzivatel pages.
  if (is_user_profile(html)) {
    names(out) <- str_replace(names(out), "^author", "title")
  }

  out
}

misc_ratings_scrape <- function(html) {

  li <- html_elements(
    html, ".others-rating > ul > li, #snippet--ratings > table > tbody > tr"
  )

  out <- tibble::tibble(

    title = li %>%
      html_element("a") %>%
      html_text2(),

    title_id = li %>%
      html_element("a") %>%
      html_attr("href") %>%
      str_extract("(uzivatel|film)/[0-9]+"),

    stars = li %>%
      html_element(".stars") %>%
      html_attr("class") %>%
      str_replace("trash", "stars-0") %>%
      substr(13, 13) %>%
      as.integer(),
  )

  # Let's make this applicable for reviews located either on /film pages
  # or /uzivatel pages. The (un-intuitive) switcheroo is:
  # /uzivatel -> rates titles
  # /title    -> is rated by a user
  if (is_user_profile(html)) {

    submitted = li %>%
      html_element("span[title], .date-only") %>%
      html_text2() %>%
      str_extract("[[:digit:]\\.]{10}") %>%
      as.Date("%d.%m.%Y")

  } else {

    names(out) <- str_replace(names(out), "^title", "user")

    submitted = li %>%
      html_element("span[title], .date-only") %>%
      html_attr("title") %>%
      str_extract("[[:digit:]\\.]{10}") %>%
      as.Date("%d.%m.%Y")
  }

  # Some rating are not submitted but calculated as an average from other ratings,
  # e.g. TV show rating may be an average of episode ratings. In this case,
  # the `submitted` date will be missing.
  tibble::add_column(out, submitted = submitted)
}

is_user_profile <- function(html) {
  !is.na(html_element(html, ".user-main"))
}

misc_fanclub_scrape <- function(html) {

  li <- html %>%
    html_element(".fan-club-content, .fans") %>%
    html_elements("ul > li")

  tibble::tibble(

    user = li %>%
      html_element("a") %>%
      html_text2(),

    user_id = li %>%
      html_element("a") %>%
      html_attr("href") %>%
      str_extract("uzivatel/[0-9]+")
  )
}

