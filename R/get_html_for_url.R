get_html_for_url <- function(url, sleep=0){

  remDr <- get_selenium_session()

  remDr$navigate(url)

  if(sleep > 0){
    Sys.sleep(sleep)
  }

  remDr$getPageSource() |> dplyr::first() |> xml2::read_html()
}
