get_senedd_members <- function(extra_info=FALSE){
  ret <- tibble()

  member_list_html <- NULL
  attempt <- 0

  while(is.null(member_list_html) && attempt <= 5){
    attempt <- attempt + 1

    if(attempt > 1){
      message("Retrying after a delay of three seconds...")
      Sys.sleep(3)
    }

    try(
      member_list_html <- read_html("https://senedd.wales/find-a-member-of-the-senedd/?VW=Table&PageSize=10000&Page=1&Culture=en-GB&IsSubSearch=False&IsPostcodeCrossConstituency=False&Postcode=&Name=&ShowAll=true&Region=&Constituency=&Constituency=&Constituency=&Constituency=&Constituency=&PartyFilterType=party&PoliticalParty=&PoliticalPartyGroup=&partyValueName=") 
    )
  }

  member_els <- member_list_html %>% 
    html_nodes(".person-search-result-item")

  clean_up_party <- function(party){
    party %>% 
      str_replace_all("Welsh Labour and Co-operative Party", "Welsh Labour") %>%
      str_replace_all(" Party$", "")
  }

  for(me in member_els){
    item_text_nodes <- me %>% html_nodes(".person-search-result-item__text")


    ret <- ret %>% bind_rows(tibble(
      Name = (item_text_nodes |> (\(x) x[[1]])() %>% html_text() %>% str_match("^(.+?) MS"))[,2],
      Party = item_text_nodes |> (\(x) x[[3]])() %>% html_text() %>% clean_up_party,
      ConstituencyOrRegion = item_text_nodes |> (\(x) x[[2]])() %>% html_text(),
      PhotographURL = me %>% html_node("img") %>% html_attr("src"),
      SeneddID = (PhotographURL %>% str_match("/Info(\\d{8})/"))[,2] %>% as.integer %>% as.character(),
      RegisterURL = paste0("https://business.senedd.wales/mgRofI.aspx?UID=", SeneddID),
      LinkURLPath = me %>% html_node("a") %>% html_attr("href"),
      LinkURL = paste0("https://senedd.wales", LinkURLPath),
      LinkURLWelsh = paste0("https://senedd.cymru", LinkURLPath %>% str_replace_all("/people/", "/pobl/")  %>% str_replace_all("-ms/", "-as/"))
    ))
  }

  ret <- ret %>% mutate(
    Constituency = if_else(ConstituencyOrRegion %in% senedd_constituencies()$SeneddConstituencyName, ConstituencyOrRegion, NA_character_),
    Region = if_else(ConstituencyOrRegion %in% senedd_constituencies()$SeneddRegionName, ConstituencyOrRegion, NA_character_)
  )

  members <- ret %>% select(SeneddID, Name, LinkURL, LinkURLWelsh, PhotographURL, RegisterURL, Party, Constituency, Region)

  if(!extra_info){
    return(members)
  }

  for (mi in 1:nrow(members)) {
    print(paste0("Fetching extra info for ", members[[mi, 'Name']], "…"))

    inf <- NULL
    attempt <- 0

    while(is.null(inf) && attempt <= 5){
      attempt <- attempt + 1

      if(attempt > 1){
        message("Retrying after a delay of three seconds...")
        Sys.sleep(3)
      }

      try(
        inf <- get_info_from_ms_page_url(members[[mi, 'LinkURL']], members[[mi, 'LinkURLWelsh']]) 
      )
    }

    stopifnot(!is.null(inf))
    
    inf_fields <- c(
      'Titles', 'TitlesWelsh',
      'SeneddEmail', 'SeneddPhone', 'OfficePhone',
      'IsMinister', 'WelshGovernmentURL',
      'TwitterURL', 'FacebookURL', 'OtherWebsiteURLs',
      'CrossPartyGroups', 'CrossPartyGroupsChairs',
      'Committees', 'CommitteesChairs',
      'PersonalHistory', 'ProfessionalBackground', 'PoliticalHistory'
      )

    for(fl in inf_fields){

      vl <- inf[[fl]]

      if(!is.null(inf[[fl]])){
        if(length(vl) > 1){
          vl <- paste0(vl, collapse="\n")
        }

        members[[mi, fl]] <- vl
      } else{
        members[[mi, fl]] <- NA_character_
      }
    }
  }

  return(members)
}