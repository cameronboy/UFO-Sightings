#To take a look at the states I have available to me:
library(XML)
library(tidyverse)
library(stringr)
library(lubridate)
require(ggmap)

loc_url <- "http://www.nuforc.org/webreports/ndxloc.html"

states <- readHTMLTable(loc_url, which=1)

write.csv(states,"states_ufo.csv")

head(states)
 

#List of Links
state_links <- htmlParse(loc_url) %>% 
  xpathSApply(.,"//a/@href") %>% 
  as_data_frame() %>% 
  filter(row_number() > 2)

head(state_links)

  
#dataframe to append all the results to
sightings <- data_frame()

#Base Url to compose the first bit of the URLs to loop through
base_url <- "http://www.nuforc.org/webreports/"

for (i in seq(nrow(state_links))) {
  
  link <- state_links[i,1]
  
     u <- paste(base_url, link, sep = "")
     
  temp <- readHTMLTable(u, which = 1) %>% 
      as_data_frame()
  
  sightings <- bind_rows(sightings, temp)
  
  print(paste(round(i/nrow(state_links),4)*100,"%"))
  
}

#Read in USA State Codes
stcodes <- read_csv("StateCode.csv")

#Seed for random sample to cut down the 100k Sightings for work
set.seed(123)

#Give me 20% of the original observations
set <- sample(seq(from = 1, to = nrow(sightings)/5, by = 1))


#Removal of some things
trunc_sight <- sightings[set, ] %>% 
  right_join(stcodes, by = "State") %>% 
  mutate(City = str_replace_all(City, "\\(.{1,}\\)",""),
         addr = paste(City,", ", State, " USA",sep = "")) %>% 
  filter(str_trim(Name) !=  str_trim(City))

#Get Unique Addresses to pass to ggmap
addresses <- trunc_sight %>% 
  select(addr) %>% 
  unique()


addreses$loc <- geocode(addresses$addr)


library(rgeolocate)

rgeolocate()

geo_key = "AIzaSyCYAZwta4y9p1jHNXZ_tO1NqpvzjxfWw9w"
register_google(key = geo_key, account_type = "premium", day_limit = 100000)

mapdist(c("houston, texas", "dallas"), "waco, texas")

devtools::install_github("dkahle/ggmap")

ggmap_credentials()

install.packages("ggmap")

library(ggmap)

register_google()
