do_gooder_table_from_senedd_members <- function(senedd_members){
  senedd_members %>%
    left_join(
      senedd_constituencies() %>% select(SeneddRegionName, SeneddConstituencyName), 
      by=c(Region = "SeneddRegionName")
      ) %>%
    mutate(
      IsRegional = !is.na(SeneddConstituencyName),
      SeneddConstituencyName = if_else(!IsRegional, Constituency, SeneddConstituencyName),
      DoGooderExtraInfo = 
              paste0(Party, " ",
                if_else(IsRegional,
                  paste0("regional MS for ", Region),
                  paste0("MS for ", Constituency)
                )
              )
      ) %>%
    left_join(senedd_constituencies(), by="SeneddConstituencyName") %>%
    mutate(
      DoGooderSeneddConstituency = 
        paste0(
          SeneddConstituencyName,
          if_else(SeneddConstituencyName != SeneddConstituencyNameEnglish, paste0(" / ", SeneddConstituencyNameEnglish), ""),
          if_else(SeneddConstituencyName != SeneddConstituencyNameWelsh,   paste0(" / ", SeneddConstituencyNameWelsh), "")
        )
      ) %>%
    arrange(
      DoGooderSeneddConstituency,
      IsRegional
    ) %>%
    rename(
      `Search by` = DoGooderSeneddConstituency,
      `Search for` = Name,
      `Email` = SeneddEmail,
      `Photo` = PhotographURL,
      `Extra Information to Display (optional but good)` = DoGooderExtraInfo
      ) %>%
    select(
      `Search by`,
      `Search for`,
      `Email`,
      `Photo`,
      `Extra Information to Display (optional but good)`
    ) 
}
