#Script for In-Class exercies in Economics 691

#Jeremy R. Groves
#November 4, 2026

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)
library(gtsummary)

#Read in Data
  e.data <- read.csv(file = "./Build/Input/ge2024cty.csv")
  
  
#Wrangle Data
  e.data.2 <- e.data %>%
    select(OfficeName, County, Votes, PartyName, BallotGroup) %>%
    group_by(County, OfficeName) %>%
      mutate(Total.Votes = sum(Votes)) %>%
    ungroup() %>%
    mutate(Share = Votes / Total.Votes,
           County = toupper(County))  %>%
    filter(PartyName != "",
           PartyName != "WORKING CLASS PARTY",
           !is.na(Share),
           BallotGroup == "A" ) 

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
      
      temp3 <- paste("B01001", str_pad(20:25, width = 3, side = "left", pad = "0"), sep = "_")
      temp4 <- paste("B01001", str_pad(44:49, width = 3, side = "left", pad = "0"), sep = "_")
      old <- c(temp3, temp4)
      
      fast.male <- cen.dat %>%
        st_drop_geometry() %>%   #This removes the sf features to speed up processing
        filter(variable %in% temp1) %>%
        summarise(pop = sum(estimate), .by = c(NAME))
      
      fast.female <- cen.dat %>%
        st_drop_geometry() %>%   #This removes the sf features to speed up processing
        filter(variable %in% temp2) %>%
        summarise(pop = sum(estimate), .by = c(NAME))
      
      
      gender <- cen.dat %>%
        st_drop_geometry() %>%
        mutate(gender = case_when(variable %in% temp ~ "male",
                                  variable %in% temp2 ~ "female",
                                  TRUE ~ "missing")) %>%
        summarise(pop = sum(estimate), .by = c(NAME, gender)) %>%
        pivot_wider(id_cols = "NAME", names_from = "gender", values_from = "pop") 
      
      old <- cen.dat %>%
        st_drop_geometry() %>%
        filter(variable %in% old) %>%
        summarise(old = sum(estimate), .by = c(NAME)) 
      
      cen.dat.2 <- cen.dat %>%
        summarise(pop = sum(estimate), .by = c(NAME, geometry)) %>%
        mutate(temp = str_split_i(NAME, ",", 1),
               temp = gsub(" County", "", temp),
               County = toupper(temp),
               County = case_when(County == "DE WITT" ~ "DEWITT",
                                  County == "JO DAVIESS" ~ "JODAVIESS",
                                  TRUE ~ County)) %>%
        left_join(., gender, by ="NAME") %>%
        left_join(., old, by ="NAME") 
      
        
      
  #Merge the data
      core <- cen.dat.2 %>%        #Start with the dataframe with the geometry in it
        left_join(., e.data.2, by = "County") %>%
        mutate(vote.rate = Total.Votes / pop,
               male = male / pop,
               female = female / pop,
               old = old / pop,
               party = Votes / Total.Votes,
               party.pop = Votes / pop)
      
  #Visualization Quickly
      core.2 <- core %>%
        select(County, vote.rate) %>%
        distinct()
      
      ggplot(core.2) +
        geom_sf(aes(fill = vote.rate)) +
        theme_minimal()
               
      
#Regression Analysis
      
      reg.core <- core %>%
        st_drop_geometry() %>%
        filter(PartyName == "REPUBLICAN") 
      
      mod.1 <- lm(Share ~ male + old, data = reg.core)
      
      tbl.1 <-tbl_regression(mod.1, intercept = TRUE) %>%                                # include the intercept row
                add_glance_table(include = c(r.squared, adj.r.squared, nobs)) %>%  # fit stats below
                #add_significance_stars() %>%
                bold_p(t = 0.05) %>%                                                     # bold significant p-values
                bold_labels() %>%
                modify_caption("**Table 1: Illinois Votes in 2024 by County for Republicans**")
      
      tbl.1 %>%
        as_gt() %>%
        gt::gtsave("./Analysis/table1.tex")
      
      tbl.1 %>%
        as_gt() %>%
        gt::gtsave("./Analysis/table1.pdf")
      
      tbl_regression(mod.1) %>%
        plot()
      