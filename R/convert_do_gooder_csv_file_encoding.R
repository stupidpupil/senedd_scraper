convert_do_gooder_csv_file_encoding <- function(dgcsv_path){
  readLines(dgcsv_path) %>%
    # HACK : Some Welsh characters can't be represented in ISO-8859-1
    str_replace_all("ŵ", "w") %>% 
    str_replace_all("ŷ", "y") %>% 
    iconv(from="UTF-8", to="ISO-8859-1//TRANSLIT") %>%
    writeLines(dgcsv_path)
}