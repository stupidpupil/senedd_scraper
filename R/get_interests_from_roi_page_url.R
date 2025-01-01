get_interests_from_roi_page_url = function(roi_page_url){

  roi_page <- get_html_for_url(roi_page_url)
  roi_tables <- roi_page %>% html_nodes(".mgInterestsTable")

  ret <- tibble(
    SeneddID = character(0),
    CategoryCaption = character(0),
    CategoryID = character(0),
    EntryInRespectOf = character(0),
    EntrySubject = character(0)
  )

  if(length(roi_tables) != 12){
    # Then the page probably doesn't currently exist
    return(ret)
  }

  senedd_uid <- roi_page %>% html_node("a[href^=mgUserInfo]") %>%
    html_attr("href") %>% str_match("UID=(\\d+)$")

  senedd_uid <- senedd_uid[,2]

  for (i in 1:length(roi_tables)) {
    roi_table_el <- roi_tables[[i]]
    category_caption = roi_table_el %>% html_node('caption') %>% html_text() %>% str_trim()

    roi_table <- roi_table_el %>% html_table()

    if(colnames(roi_table) %>% length() == 2){
      colnames(roi_table) <- c('EntryInRespectOf', 'EntrySubject')

    }else{
      colnames(roi_table) <- c('EntrySubject')
    }

    roi_table <- roi_table %>% 
      mutate(CategoryCaption = category_caption) %>%
      mutate_all(as.character)

    ret <- ret %>% bind_rows(roi_table)
  }

  ret %>% mutate(
    SeneddID = senedd_uid,
    CategoryID = CategoryCaption %>% str_extract("^\\d+")
  )

}