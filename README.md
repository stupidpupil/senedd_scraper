```
devtools::load_all()
members <- get_senedd_members()
interests <- get_interests_for_members(members)
interests %>% group_by(SeneddID) %>% count %>% left_join(members %>% select(SeneddID, Name)) %>% arrange(SeneddID)
```
