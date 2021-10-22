format_phone_number <- function(pn) {
  pn %>% 
    str_replace("^\\(?\\+?44\\)?0?", "0") %>%
    str_replace_all("\\s","") %>%
    str_replace("^(0\\d00)(\\d{0,3})(\\d*)$", "\\1 \\2 \\3") %>%
    str_replace("^(\\d{5})(\\d{0,3})(\\d*)$", "\\1 \\2 \\3")
}
