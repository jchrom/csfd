kino_releases_scrape = function(html) {

  year <- html %>%
    html_element("#frm-cinemaFilterForm-cinemaFilterForm-year") %>%
    html_element("option[selected]") %>%
    html_attr("value")

  tbody <- html_elements(html, ".table-container > table > tbody > tr")

  tibble::tibble(

    title = tbody %>%
      html_element(".name > h3") %>%
      html_text2() %>%
      str_remove(" \\([0-9]{4}\\)$"),

    title_id = tbody %>%
      html_element(".name > h3 > a") %>%
      html_attr("href") %>%
      str_extract("film/[0-9]+"),

    premiere = tbody %>%
      html_element(".name > h3 > span") %>%
      html_text2() %>%
      regx_extract_int(),

    premiere_cz = tbody %>%
      html_element(".date-only") %>%
      html_text2() %>%
      fill_na() %>%
      paste0(year) %>%
      as.Date("%d.%m.%Y"),

    distributor = tbody %>%
      html_element(".dist") %>%
      html_text2()
  )
}
