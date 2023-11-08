
library(terra)
library(tidyterra)
library(ggplot2)
library(rvest)
library(sf)

#shapefile from https://gisdata.mn.gov/dataset/bdry-votingdistricts
file <- 'C:/Users/<USER>/Downloads/shp_bdry_votingdistricts/bdry_votingdistricts.shp'

#read in shapefile
voting_districts <- vect(file)

#change projection
vd2 <- project(voting_districts, "EPSG:4326")

vd3 <- vd2 %>%
  #filter to Ramsey County, Saint Paul
  filter(toupper(COUNTYNAME) %in% "RAMSEY" & MCDNAME %in% "St. Paul") %>%
  #convert to SF object for easier interaction
  st_as_sf()


#read in data from results

tax_precinct_results <- read_html("https://electionresults.sos.mn.gov/Results/Index?ersElectionId=157&scenario=ResultsByPrecinctCrosstab&QuestionId=1612")

#keep the third table as it has the relevant information
table_node <- html_nodes(tax_precinct_results,"table") %>% .[[3]]

tax_tbl <- html_table(table_node) %>% # read the table from html to make a data frame
  #set simpler names
  setNames(c("precinct", "yes", "no" )) %>%
  #drop out the commas and make numeric
  mutate(across(2:3, ~ as.numeric(gsub("\\,", "",.))),
  # calculate the total votes from yes and no
         total = rowSums(across(2:3), na.rm = TRUE),
         #calculat the percent of yes votes
          pct_yes = round(yes/total  * 100, 1)
  ) %>%
  #clean up the names to align with the vote data
  mutate(precinct = str_replace(precinct, "Ramsey: ", "") %>%
  #remove the zero before the precinct number to align with the vote data
  str_replace_all("(?<=P-)0(?=\\d)", "")
  )


tax_map <- vd3 %>%
  #set to upper
  mutate(PCTNAME = toupper(PCTNAME)) %>%
  #merge the vote data to the shapefile
  left_join(tax_tbl, by = c("PCTNAME"= "precinct"))


#push out the map
tax_map %>%
  #create categories for the approval percent
  mutate(maj_yes = case_when(pct_yes < 50 ~ "Under 50%", 
                             pct_yes < 55 ~ "50-54.9%",
                             pct_yes < 60 ~ "55-59.9%",
                             pct_yes < 65 ~ "60-64.9%",
                             pct_yes < 75 ~ "65-74.9%",
                             TRUE ~ "75+") %>%
           #set to factor to keep order
           factor(levels = c("Under 50%", "50-54.9%", "55-59.9%", "60-64.9%", "65-74.9%", "75+"))) %>%
  ggplot(aes(fill = maj_yes)) + 
  geom_sf()
  
