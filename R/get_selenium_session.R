get_selenium_session_impl <- function() {
  sess <- NULL

  function (force_new = FALSE){
    if(!is.null(sess) && !force_new){
      return(sess)
    }

    remDr <- RSelenium::remoteDriver(
      remoteServerAddr = "localhost",
      port = 4444L,
      browserName = "chrome",
      extraCapabilities = list(
        chromeOptions = list(
          args = list(
            "--headless=new",
            "--user-agent=Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36",
            "--disable-gpu",
            "--disable-dev-shm-usage",
            "--disable-extensions")))
    )

    remDr$open()

    sess <<- remDr

    return(sess)
  }
}

get_selenium_session <- get_selenium_session_impl()
