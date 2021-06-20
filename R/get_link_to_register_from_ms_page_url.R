# Manual fallback - generally you can just use https://business.senedd.wales/mgRofI.aspx?UID=
get_link_to_register_from_ms_page_url <- function(ms_page_url){
  ms_page <- read_html(ms_page_url)
  ms_page %>% html_node(xpath='//a[contains(text(), "View Register")]') %>% html_attr("href")
}