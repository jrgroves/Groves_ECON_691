#Script for In-Class exercies in Economics 691

#Jeremy R. Groves
#November 4, 2026

rm(list=ls())

library(tidyverse)
library(tidycensus)

#Read in Data
  e.data <- read.csv(file = "./Build/Input/ge2024cty.csv")
  
  
#Wrangle Data
  e.data.2 <- e.data %>%
    select(OfficeName, County, Votes, PartyName, BallotGroup)
    group_by(County, OfficeName) %>%
      mutate(Total.Votes = sum(Votes)) %>%
    ungroup() %>%
    mutate(Share = Votes / Total.Votes,
           County = toupper(County)) %>%
    filter(PartyName != "",
           PartyName != "WORKING CLASS PARTY",
           !is.na(Share),
           BallotGroup == "A" ) %>%

    

  acs <- load_variables(year = 2024, dataset = "acs5")
  
  #Define the list of variables to pull from the ACS.
      temp <- paste("B01001", str_pad(7:25, width = 3, side = "left", pad = "0"), sep = "_")
      temp2 <- paste("B01001", str_pad(31:49, width = 3, side = "left", pad = "0"), sep = "_")
      vars <- c(temp, temp2)
  
  #Call the US Census API
      cen.dat <- get_acs(geography = "county",
                         variables = vars,
                         state = "17",
                         year = 2024,
                         geometry = TRUE)
      
  #Process the Census Data
      cen.dat.2 <- cen.dat %>%
        summarise(pop = sum(estimate), .by = c(NAME, geometry)) %>%
        mutate(temp = str_split_i(NAME, ",", 1),
               temp = gsub(" County", "", temp),
               County = toupper(temp),
               County = case_when(County == "DE WITT" ~ "DEWITT",
                                  County == "JO DAVIESS" ~ "JODAVIESS",
                                  TRUE ~ County))
      
  #Merge the data
      core <- cen.dat.2 %>%        #Start with the dataframe with the geometry in it
        left_join(., e.data.2, by = "County") %>%
        mutate(vote.rate = Total.Votes / pop)
      
  #Visualization Quickly
      core.2 <- core %>%
        select(County, vote.rate) %>%
        distinct()
      
      ggplot(core.2) +
        geom_sf(aes(fill = vote.rate)) +
        theme_minimal()
               