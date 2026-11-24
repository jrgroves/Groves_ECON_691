#Script for In-Class exercies in Economics 691

#Jeremy R. Groves
#November 4, 2026

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)

#Read in Data
  e.data.24 <- read.csv(file = "./Build/Input/ge2024cty.csv")
  e.data.20 <- read.csv(file = "./Build/Input/ge2020cty.csv")
  
  
#Wrangle Data
  e.data.2a <- e.data.20 %>%
    select(OfficeName, County, Votes, PartyName, BallotGroup) %>%
    group_by(County, OfficeName) %>%
      mutate(Total.Votes = sum(Votes)) %>%
    ungroup() %>%
    mutate(Share = Votes / Total.Votes,
           County = toupper(County)) %>%
    filter(PartyName == "REPUBLICAN" | PartyName == "DEMOCRATIC",
           OfficeName != "UNITED STATES SENATOR",
           !is.na(Share),
           BallotGroup == "A") %>%
    mutate(Year = 2020) %>%
    select(OfficeName, County, PartyName, Votes, Share, Year)
  
  e.data.2b <- e.data.24 %>%
    select(OfficeName, County, Votes, PartyName, BallotGroup) %>%
    group_by(County, OfficeName) %>%
    mutate(Total.Votes = sum(Votes)) %>%
    ungroup() %>%
    mutate(Share = Votes / Total.Votes,
           County = toupper(County)) %>%
    filter(PartyName == "REPUBLICAN" | PartyName == "DEMOCRATIC",
           OfficeName != "UNITED STATES SENATOR",
           !is.na(Share),
           BallotGroup == "A") %>%
    mutate(Year = 2024) %>%
    select(OfficeName, County, PartyName, Votes, Share, Year)
  
  e.data.2 <- e.data.2a %>%
    bind_rows(e.data.2b)  %>%
    arrange(County, PartyName, Year) %>%
    mutate(name = paste(PartyName, Year, sep="_"),
           per_value = (Share - lag(Share))/Share, .by = c(County,PartyName)) 
  
  
  e.data.3a <- e.data.2 %>%
    pivot_wider(id_cols = "County", names_from = "name", 
                values_from = c("Votes", "Share"), names_glue = "{name}_{.value}")
  
  
  #Define the list of variables to pull from the ACS.
      temp1 <- paste("B01001A", str_pad(7:16, width = 3, side = "left", pad = "0"), sep = "_")
      temp2 <- paste("B01001A", str_pad(22:31, width = 3, side = "left", pad = "0"), sep = "_")
      temp3 <- paste("B01001B", str_pad(7:16, width = 3, side = "left", pad = "0"), sep = "_")
      temp4 <- paste("B01001B", str_pad(22:31, width = 3, side = "left", pad = "0"), sep = "_")
      temp5 <- paste("B01001I", str_pad(7:16, width = 3, side = "left", pad = "0"), sep = "_")
      temp6 <- paste("B01001I", str_pad(22:31, width = 3, side = "left", pad = "0"), sep = "_")
      vars <- c(temp1, temp2, temp3, temp4, temp5, temp6, "B19013_001")
  
  #Call the US Census API
      cen.dat.24 <- get_acs(geography = "county",
                             variables = vars,
                             state = "17",
                             year = 2024,
                             geometry = TRUE)
      cen.dat.20 <- get_acs(geography = "county",
                            variables = vars,
                            state = "17",
                            year = 2020,
                            geometry = TRUE)
      
  #Process the Census Data
      
      temp <- cen.dat.20 %>%
        mutate(temp = str_split_i(NAME, ",", 1),
               temp = gsub(" County", "", temp),
               County = toupper(temp),
               County = case_when(County == "DE WITT" ~ "DEWITT",
                                  County == "JO DAVIESS" ~ "JODAVIESS",
                                  TRUE ~ County),
               Year = 2020) %>%
        st_drop_geometry() 
      
      cen.dat.1 <- cen.dat.24 %>%
        mutate(temp = str_split_i(NAME, ",", 1),
               temp = gsub(" County", "", temp),
               County = toupper(temp),
               County = case_when(County == "DE WITT" ~ "DEWITT",
                                  County == "JO DAVIESS" ~ "JODAVIESS",
                                  TRUE ~ County),
               Year = 2024) %>%
        st_drop_geometry() %>%
        bind_rows(temp)
      
      cen.dat.2 <- cen.dat.1 %>%
        mutate(data = case_when(variable %in% temp1 ~ "white_male",
                                variable %in% temp3 ~ "black_male",
                                variable %in% temp5 ~ "hispan_male",
                                variable == "B19013_001" ~ "med_income",
                                TRUE ~ NA),
               tot.pop = sum(estimate), .by = c(NAME, Year)) %>%
        filter(!is.na(data)) %>%
        mutate(pop = sum(estimate), .by = c(NAME, Year, data)) %>%
        distinct(County, Year, data, tot.pop, pop) %>%
        mutate(share = pop / tot.pop,
               value = case_when(data == "med_income" ~ pop,
                                 TRUE ~ share),
               name = paste(data, Year, sep = "_")) %>%
        arrange(Year, County, data) %>%
        mutate(per_value = (value - lag(value))/value, .by = c(County,data)) 
      
      cen.dat.3a <- cen.dat.2 %>%
        pivot_wider(id_cols = "County", values_from = value, names_from = name)
      
      cen.dat.3 <- cen.dat.2 %>%
        filter(!is.na(per_value)) %>%
        pivot_wider(id_cols = "County", values_from = per_value, names_from = data, names_prefix = "per_") %>%
        left_join(., cen.dat.3a, by = "County")
      
      cen.dat.map <- cen.dat.24 %>%
        mutate(temp = str_split_i(NAME, ",", 1),
               temp = gsub(" County", "", temp),
               County = toupper(temp),
               County = case_when(County == "DE WITT" ~ "DEWITT",
                                  County == "JO DAVIESS" ~ "JODAVIESS",
                                  TRUE ~ County)) %>%
        select(County, geometry) %>%
        distinct(County, .keep_all = TRUE)
     
  #Merge the data
      core <- cen.dat.3 %>%        #Start with the dataframe with the geometry in it
        left_join(., e.data.2, by = "County")
      
      
      
      
      %>%
        mutate(vote.rate = Total.Votes / pop,
               m.share = m.pop / pop,
               f.share = f.pop / pop,
               M.F.diff = m.share - f.share,
               case = case_when(REP.DEM.diff > 0 & M.F.diff > 0 ~ "More Men and Rep",
                                REP.DEM.diff > 0 & M.F.diff < 0 ~ "More Women and Rep",
                                REP.DEM.diff < 0 & M.F.diff > 0 ~ "More Men and Dem",
                                REP.DEM.diff < 0 & M.F.diff < 0 ~ "More Women and Dem"))
        
        
      
  #Visualization Quickly
      core.2 <- core %>%
        select(County, case) %>%
        distinct()
      
      ggplot(core.2) +
        geom_sf(aes(fill = case)) +
        theme_minimal()
               