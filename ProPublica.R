library(robotstxt)
library(tidyverse)
library(rvest)
library(stringr)

paths_allowed("https://projects.propublica.org/recovery/")


states <- data.frame("state" = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                                "Delaware", "District-of-Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
                                "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New-Hampshire", 
                                "New-Jersey", "New-Mexico", "New-York", "North-Carolina", "North-Dakota", "Ohio", 
                                "Oklahoma", "Oregon", "Pennsylvania", "Rhode-Island", "South-Carolina", "South-Dakota", 
                                "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West-Virginia", 
                                "Wisconsin", "Wyoming"))

states$link <- tolower(states$state)
states$link <- paste0("https://projects.propublica.org/recovery/locale/", states$link)
statelinks <- states$link
statenames <- states$state

state_funding <- NULL
for(i in 1:51) {
countieslinks <- curl::curl(statelinks[i]) %>%
  read_html() %>% 
  html_nodes("#county-data") %>%
  html_nodes("tr") %>%
  html_nodes("td a") %>%
  html_attr("href")
countieslinks <- paste0("https://projects.propublica.org", countieslinks)

  for (j in 1:length(countieslinks)) {
  funding <- curl::curl(countieslinks[j]) %>%
    read_html() %>%
    html_nodes("#recovery-body > div > div > table") %>%
    html_table()
  funding <- funding[[1]]
  funding$link <- countieslinks[[j]]
  state_funding <- rbind(state_funding, funding)
  }

assign(statenames[i], state_funding)
state_funding <- NULL
}


total_funding <- rbind(Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, 
                       Delaware, `District-of-Columbia`, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, 
                       Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, 
                       Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, `New-Hampshire`, 
                       `New-Jersey`, `New-Mexico`, `New-York`, `North-Carolina`, `North-Dakota`, Ohio, 
                       Oklahoma, Oregon, Pennsylvania, `Rhode-Island`, `South-Carolina`, `South-Dakota`, 
                       Tennessee, Texas, Utah, Vermont, Virginia, Washington, `West-Virginia`, 
                       Wisconsin, Wyoming)

total_funding$link <- str_remove(total_funding$link, "https://projects.propublica.org/recovery/locale/")
total_funding[, 7:8] <- str_split_fixed(total_funding$link, "/", 2)
total_funding <- total_funding %>%
  rename(State = link) %>%
  rename(County = V2)
total_funding$State <- str_to_title(total_funding$State)
total_funding$County <- str_to_title(total_funding$County)
total_funding$State <- str_replace(total_funding$State, "-", " ")
total_funding$County <- str_replace(total_funding$County, "-", " ")
write.csv(total_funding, "stimulus_plan.csv", row.names = F)

BIP_funding <- total_funding %>%
  filter(str_detect(Description, "Broadband Initiatives Program"))
write.csv(BIP_funding, "BIP_funding.csv", row.names = F)

