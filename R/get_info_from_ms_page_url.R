get_info_from_ms_page_url <- function(ms_page_url, ms_welsh_page_url=NULL){
  ms_page <- get_html_for_url(ms_page_url)

  ret_info <- list()

  if(!is.null(ms_welsh_page_url)){
    ret_info$TitlesWelsh <- NA_character_
    tryCatch({
      welsh_info <- get_info_from_ms_page_url(ms_welsh_page_url)
      ret_info$TitlesWelsh <- welsh_info$Titles
    },
    error=function(err){}
    )
  }

  # Generally you can just use https://business.senedd.wales/mgRofI.aspx?UID=
  ret_info$RegisterURL <- ms_page %>% 
    html_node(xpath='//a[contains(text(), "View Register")]') %>% html_attr("href")


  ret_info$NameWithMSSuffix <- ms_page %>%
    html_node(".person-details-basic-info--name") %>% html_text() %>% str_trim()

  ret_info$Name <- (ret_info$NameWithMSSuffix %>% str_match("^(.+?) MS$"))[,2]

  ret_info$Constituency <- ms_page %>%
    html_node(".person-details-basic-info--constituency") %>% html_text() %>% str_trim()

  ret_info$Region <- ms_page %>%
    html_node(".person-details-basic-info--region") %>% html_text() %>% str_trim()

  ret_info$Titles <- ms_page %>%
    html_nodes(".person-details-basic-info--title") %>% html_text() %>% str_trim()

  ret_info$Titles <- if_else(ret_info$Titles == "", NA_character_, ret_info$Titles)

  ret_info$WelshGovernmentURL <- ms_page %>% # For ministers
    html_node(xpath='//div[contains(@class, "person-details-details-container__key-info")]//a[contains(@href, "gov.wales")]') %>% html_attr("href")

  ret_info$Emails <- ms_page %>%
    html_nodes(xpath=paste0('//a[contains(@title, "Send email to ', ret_info$NameWithMSSuffix,'")]')) %>% html_attr("href") %>%
      str_replace("^mailto:", "")

  ret_info$SeneddEmail <-
    ret_info$Emails[ret_info$Emails %>% str_detect(regex("senedd\\.wales$", ignore_case=TRUE))] %>% first()

  ret_info$Phones <- ms_page %>%
    html_nodes(xpath=paste0('//a[contains(@title, "Call ', ret_info$NameWithMSSuffix,'")]')) %>% html_attr("href") %>%
      str_replace("^tel:", "") %>% format_phone_number()

  ret_info$SeneddPhone <-
    ret_info$Phones[ret_info$Phones %>% str_detect("^0300")] %>% last()

  ret_info$OfficePhone <-
    ret_info$Phones[!(ret_info$Phones %>% str_detect("^0300"))] %>% first()

  ret_info$SocialURLs <- ms_page %>%
    html_nodes(".person-details-social-item__text") %>% html_attr("href")

  ret_info$TwitterURL <-
    ret_info$SocialURLs[ret_info$SocialURLs %>% str_detect("^https?://(www\\.)?twitter\\.com")] %>% first()

  ret_info$FacebookURL <-
    ret_info$SocialURLs[ret_info$SocialURLs %>% str_detect("^https?://(www\\.)?facebook\\.com")] %>% first()

  ret_info$OtherWebsiteURLs <- 
    ret_info$SocialURLs[!(ret_info$SocialURLs %in% c(ret_info$TwitterURL, ret_info$FacebookURL))]

  ret_info$TermsOfOffice <- ms_page %>%
    html_nodes("#termsOfOfficeContent ol li") %>% html_text() %>% str_trim()

  ret_info$Biography <- ms_page %>%
    html_node('#biographyContent') %>% html_text() %>% str_replace_all("\\r\\n","\\\n")

  ret_info$PersonalHistory <- ret_info$Biography %>%
    str_extract(regex("(?<=Personal[\n\\s]history).+(?=^Professional[\n\\s]background)", multiline=TRUE, dotall=TRUE)) %>%
    str_replace_all("[\\n\\s]+", " ") %>% str_trim()

  ret_info$ProfessionalBackground <- ret_info$Biography %>%
    str_extract(regex("(?<=Professional[\n\\s]background).+(?=^Political[\n\\s]history)", multiline=TRUE, dotall=TRUE)) %>%
    str_replace_all("[\\n\\s]+", " ") %>% str_trim()

  ret_info$PoliticalHistory <- ret_info$Biography %>%
    str_extract(regex("(?<=Political[\n\\s]history).+?(?=(^Register[\n\\s]of|\\Z))", multiline=TRUE, dotall=TRUE)) %>%
    str_replace_all("[\\n\\s]+", " ") %>% str_trim()

  ret_info$IsMinister = (str_detect(ret_info$Titles, "Minister") | str_detect(ret_info$TitlesWelsh, "(Gw|W)einidog"))


  #
  # Cross Party Groups
  #

  ret_info$CrossPartyGroups <- c()
  ret_info$CrossPartyGroupsChairs <- c()

  ob_lis <- ms_page %>% html_nodes("#outsideBodiesContent ul li")

  if(!is.null(ob_lis)){
    for(ob_li in ob_lis){
      group_name <- html_node(ob_li, "a") %>% html_text() %>%
        str_trim() %>%
        str_replace("^(Cross Party )?(.+?)( Group)?$", "\\2")

      ret_info$CrossPartyGroups <- c(ret_info$CrossPartyGroups, group_name)

      if(ob_li %>% html_text() %>% str_detect("\\(Chair\\)")){
        ret_info$CrossPartyGroupsChairs <- c(ret_info$CrossPartyGroupsChairs, group_name)
      }

    }

    ret_info$CrossPartyGroups <- ret_info$CrossPartyGroups %>% unique()
    ret_info$CrossPartyGroupsChairs <- ret_info$CrossPartyGroupsChairs %>% unique()
  }

  #
  # Committees
  #

  ret_info$Committees <- c()
  ret_info$CommitteesChairs <- c()

  ob_lis <- ms_page %>% html_nodes("#membershipContent ul li")

  if(!is.null(ob_lis)){
    for(ob_li in ob_lis){
      group_name <- html_node(ob_li, "a") %>% html_text() %>%
        str_trim() %>%
        str_replace("^((Special )?(Purpose )?Committee (for|on) (the )?)?(.+?)( Committee)?$", "\\6")

      if(group_name %in% c('Plenary', "Chairs' Forum")){
        next
      }

      ret_info$Committees <- c(ret_info$Committees, group_name)

      if(ob_li %>% html_text() %>% str_detect("\\(Chair\\)")){
        ret_info$CommitteesChairs <- c(ret_info$CommitteesChairs, group_name)
      }

    }

    ret_info$Committees <- ret_info$Committees %>% unique()
    ret_info$CommitteesChairs <- ret_info$CommitteesChairs %>% unique()
  }



  for(key in names(ret_info)){
    if(identical(ret_info[[key]], character(0))){
      ret_info[key] <- NA_character_
    }
  }

  return(ret_info)
}