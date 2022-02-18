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
      csfd_href(),

    stars = article %>%
      html_element(".star-rating > span") %>%
      html_attr("class") %>%
      csfd_stars(),

    submitted = article %>%
      html_element("time") %>%
      html_text2() %>%
      csfd_date(),

    text = article %>%
      html_element("p") %>%
      html_text2()
  )

  if (!is.na(html_element(html, ".user-main"))) {
    names(out) <- stringr::str_replace(names(out), "^author", "title")
  }

  out
}

misc_ratings_scrape <- function(html) {

  li <- html_elements(
    html, ".others-rating > ul > li, #snippet--ratings > table > tbody > tr"
  )

  csfd_attr_or_text <- function(node, name) {

    if(any(xml2::xml_has_attr(node, name))) {
      return(html_attr(node, name))
    }

    html_text2(node)
  }

  tibble::tibble(

    user = li %>%
      html_element("a") %>%
      html_text2(),

    user_id = li %>%
      html_element("a") %>%
      csfd_href(),

    stars = li %>%
      html_element(".stars") %>%
      html_attr("class") %>%
      csfd_stars(),

    submitted = li %>%
      html_element("span[title], .date-only") %>%
      csfd_attr_or_text("title") %>%
      csfd_date()
  )
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
      csfd_href()
  )
}

