get_senedd_members <- function(){
  table_el <- read_html("https://business.senedd.wales/mgMemberIndex.aspx?VW=TABLE&PIC=1") %>%
    html_node("#mgTable1")

  ret <- table_el %>% html_table()

  ret <- ret %>% 
    rename(Party = `Political party`) %>%
    mutate(
      Name = (`Member of the Senedd` %>% str_match("^(.+?) MS\r"))[,2],
      Party = (Party %>% str_match("^(.+?)\\("))[,2] %>% str_replace_all("Welsh Labour and Co-operative Party", "Welsh Labour")
      )

  ret$LinkURL <- table_el %>% html_nodes("a[href^=mgUserInfo]") %>% html_attr('href')
  ret$PhotographURL <- table_el %>% html_nodes("img") %>% html_attr('src')

  ret <- ret %>%
    mutate(
      LinkURL = paste0("https://business.senedd.wales/", LinkURL),
      PhotographURL = paste0("https://business.senedd.wales/", PhotographURL),
      SeneddID =  (LinkURL %>% str_match("UID=(\\d+)$"))[,2],
      RegisterURL = paste0("https://business.senedd.wales/mgRofI.aspx?UID=", SeneddID)
      )

  ret %>% select(SeneddID, Name, LinkURL, PhotographURL, RegisterURL, Party, Constituency, Region)

}