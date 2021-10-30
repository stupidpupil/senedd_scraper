get_senedd_members <- function(extra_info=FALSE){
  table_el <- read_html("https://business.senedd.wales/mgMemberIndex.aspx?VW=TABLE&PIC=1") %>%
    html_node("#mgTable1")

  ret <- table_el %>% html_table()

  ret <- ret %>% 
    rename(Party = `Political party`) %>%
    mutate(
      Name = (`Member of the Senedd` %>% str_match("^(.+?) MS\r"))[,2],
      Party = (Party %>% str_match("^(.+?)\\("))[,2] %>% str_replace_all("Welsh Labour and Co-operative Party", "Welsh Labour"),
      Region = Region %>% str_replace_all("[\\(\\)]", "")
      )

  ret$LinkURLPath <- table_el %>% html_nodes("a[href^=mgUserInfo]") %>% html_attr('href')
  ret$PhotographURL <- table_el %>% html_nodes("img") %>% html_attr('src')

  ret <- ret %>%
    mutate(
      LinkURL = paste0("https://business.senedd.wales/", LinkURLPath),
      LinkURLWelsh = paste0("https://busnes.senedd.cymru/", LinkURLPath),
      PhotographURL = paste0("https://business.senedd.wales/", PhotographURL),
      SeneddID =  (LinkURL %>% str_match("UID=(\\d+)$"))[,2],
      RegisterURL = paste0("https://business.senedd.wales/mgRofI.aspx?UID=", SeneddID)
      )

  members <- ret %>% select(SeneddID, Name, LinkURL, LinkURLWelsh, PhotographURL, RegisterURL, Party, Constituency, Region)

  if(!extra_info){
    return(members)
  }

  for (mi in 1:nrow(members)) {
    print(paste0("Fetching extra info for ", members[[mi, 'Name']], "…"))

    inf <- get_info_from_ms_page_url(members[[mi, 'LinkURL']], members[[mi, 'LinkURLWelsh']]) 
    
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