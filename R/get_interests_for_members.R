get_interests_for_members <- function(members){
  ret <- tibble()

  for (mi in 1:nrow(members)) {
    print(paste0("Fetching interests for ", members[[mi, 'Name']], "…"))
    ret <- ret %>% bind_rows(get_interests_from_roi_page_url(members[[mi, 'RegisterURL']]))
  }

  ret
}