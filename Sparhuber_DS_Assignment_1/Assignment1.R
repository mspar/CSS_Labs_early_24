#### Title: Digital Strategies -- Assignment 1: Psychotherapists in Austria
#### Author: Marc Sparhuber


### This script is split in three parts. First, the first 10 pages of source code
### belonging to the Austrian Association of Psychotherapists' website's search
### function are downloaded. In this step a mock-login is also performed. Then, 
### the raw data are wrangled into a tibble and mapped in a final step. This
### script is accompanied by a quarto file providing additional information and
### plots.


## Part 1: Getting the Source Codes

# loading in packages
library(selenium)
library(httr)
library(XML)
library(tidyverse)

# creating session
session <- SeleniumSession$new(browser = "firefox")

# check whether we're good to go
session$status()

# Navigate to relevant website
session$navigate("https://www.psychotherapie.at/patientinnen/psychotherapeutinnen-suche")

# Retrieve the URL to check whether everything is going smoothly
session$current_url()

# screenshot function for double checking (use of this is commented out to not
# save anything on a user's device)
screenshot <- function(what) {
  base64 <- what$screenshot()
  raw <- base64enc::base64decode(base64)
  writeBin(raw, "screenshot.png")
}

# screenshot(session)

# Agree to cookies in order to proceed (the website blocks us from doing anything
# before we either agree or disagree). Here and in the rest of the code, short
# sys.sleep's are used so that selenium throws no errors when the entire script is
# run at once.
cookies <- session$find_element(using = "css selector", value = ".eu-cookie-compliance-banner .agree-button")
Sys.sleep(1)
cookies$click()


# Here, a login is attempted. I do this to fulfill extra requirements as I'm aiming
# for an A. Since this is a website of an association of Austrian psychotherapists
# and I am not one of them I do not have an account. I nevertheless go through the
# motions of logging in to show I know how to do it.

# First we navigate to the Login page
login_site <- session$find_element(using = "xpath", value = "//a[@id='loginout']")
Sys.sleep(1)
login_site$click()
session$current_url()

# Here are some bogus ID and PW
ID <- "Dr.Zhivago" 
PW <- "Ilovetrains123"
  
# We identify and send the ID to the id-field
id_field <- session$find_element(using = "xpath", value = "//input[@id='edit-name']")
Sys.sleep(1)
# screenshot(session)
id_field$send_keys(ID)

# We identify and send the PW to the pw-field
pw_field <- session$find_element(using = "xpath", value = "//input[@id='edit-pass']")
Sys.sleep(1)
# screenshot(session)
pw_field$send_keys(PW)

# We submit our credentials
login <- session$find_element(using = "xpath", value = "//input[@id='edit-submit']")
Sys.sleep(1)
# screenshot(session)
login$click()

# oh no it didn't work
# screenshot(session)

# Disappointed we return to where we started 
session$back()
Sys.sleep(1)
session$back()
Sys.sleep(1)

# This website's standard setting is to randomize search parameters (likely in order
# to not favor certain therapists). This makes replicability of this script iffy,
# so these next two clicks ensure that the therapists are listed alphabetically, by
# 1. expanding the relevant search form and then, 2. clicking the alphabetical sorting
# checkbox.
alphabetical_expand <- session$find_element(using = "xpath", value = "//*[(@id = 'block-block-14')]//h3[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]")
Sys.sleep(1)
alphabetical_expand$click()

alphabetical_click <- session$find_element(using = "xpath", value = '//label[contains(@for, "sortierung_alphabetisch")]')
Sys.sleep(1)
alphabetical_click$click()
Sys.sleep(1)

# This executes the search for psychotherapists.
search <- session$find_element(using = "xpath", value = "//input[(((count(preceding-sibling::*) + 1) = 20) and parent::*)]")
Sys.sleep(1)
search$click()
Sys.sleep(1)

# What follows here are different ways to get at the page's source that I show
# just to prove that I know the different approaches. I make sure to combine them
# all into a unnamed list object. In total, I extract the source codes of the first
# 10 pages. Some of the Sys.sleeps are very generous because I've noticed that
# sometimes when the page is a bit slow making requests in too quick a succession,
# I will get 404 errors.

# create empty list for sources
src <- list()

src[[1]] <- session$get_page_source()
Sys.sleep(5)

# navigate to page 2, then also get its source
next_page <- session$find_element(using = "xpath", value = '//a[contains(@class, "page-link-next")]')
Sys.sleep(5)
next_page$click()
session$current_url()

src[[2]] <- session$get_page_source()

# Now do it with a loop
for (i in 1:3) {
  next_page <- session$find_element(using = "xpath", value = '//a[contains(@class, "page-link-next")]')
  Sys.sleep(5)
  next_page$click()
  Sys.sleep(3)
  src[[i+2]] <- session$get_page_source()
  Sys.sleep(5)
}

# Now do it based on the naming pattern in the URL with httr functions instead.
# The pattern for this website is always the same, only "page=X" changes.
# Currently there are 537 pages, I'll continue with 5 more from where I stopped above.
base_url <- "https://www.psychotherapie.at/patientinnen/psychotherapeutinnen-suche"
urls <- paste0(base_url, "?page=", 6:10, "&pth_search_sort=alphabetisch&pth_search_bundesland=&pth_search_bezirk=&pth_search_plz=&pth_search_ort=&pth_search_arbeitsschwerpunkt=&pth_search_methode=&pth_search_setting=&pth_search_zielgruppe=&pth_search_sprache=&pth_search_geschlecht=&pth_search_nachname=&pth_honey=mt4thbwy&search_pth=Suchen")

results <- list()
for (i in 1:5) {
  results[[i]] <- GET(urls[[i]])
  src[[i+5]] <- content(results[[i]], as = 'text')
}

# Close the session and remove all leftovers to continue cleanly with doms.
session$close()
rm(list=setdiff(ls(), "src"))
gc()

# Now save the source codes to be able to continue anytime and be nice with the
# website :)
for (i in 1:10) {
writeLines(src[[i]], paste0("psy_search", i, ".html"))
}




## Part 2: Creating a data frame


# Start by loading in all the sources codes and immediately htmlParse to get the
# dom tree structure.
doms <- list()
for (i in 1:10) {
  doms[[i]] <- htmlParse(readLines(paste0("psy_search", i, ".html")))
}

# As we want to go node-wise rather than column-wise when extracting the variables,
# we create a list containing the containers indicating a new node for each of the
# 10 pages.
node_path <- list()
for (i in 1:10) {
  node_path[[i]] <- xpathSApply(doms[[i]], '//div[contains(@class, "search-results-row")]')
}

# This huge nested for loop extracts the relevant of each node on each of the 10
# pages. Additionally, it employs regex to fix some of the most pressing issues
# in the data but leaves some for after the function.
results <- NULL
for (i in 1:10) {
  
  for (j in 1:10) {
    
  # extracts the name and academic titles
  name <- xpathSApply(node_path[[j]][[i]], './/h2', xmlValue)
  
  # extracts type of therapy offered (i.e., psychoanalytic, behavioral, etc.),
  # and the professional title (although it needs be said that I dichotomize this
  # variable later to only contain a differentiation between full psychotherapists
  # and psychotherapists in training).
  text1 <- xpathSApply(node_path[[j]][[i]], './/p[1]', xmlValue)[1]
  specialisation <- sub(".*?[a-z](?=[A-Z])", "", text1, perl = TRUE)
  title <- sub("(?<=[a-z])(?=[A-Z]).*", "", text1, perl = TRUE)

  # Extracts each node's address. This is by far the thing that caused me the most
  # anguish in this assignment and contains many ugly extra rules corresponding
  # to the fact that the way the addresses are formatted contains many special
  # cases in which additional information is given about where in a building
  # someone is located. Results in addresses in the format "Streetname Number Zip".
  (text <- xpathSApply(node_path[[j]][[i]], ".//div[contains(@class, 'pthCol')][1]", xmlValue))
  # remove whitespace (will happen frequently)
  (text <- str_squish(text))
  
  # The following are a bunch of special cases as mentioned above. Either ran
  # for every node or based on an if-statement.
  
  (text <- gsub("(?<=[0-9])([A-Z])", "\\L\\1", text, perl = TRUE))
  
  if (grepl("2. Standort", text)) {
    text <- str_remove(text, "2. Standort")
  }
  
  if (grepl("/DG", text)) {
    text <- str_remove(text, "/.*?DG")
  }
  
  if (grepl("/II", text)) {
    text <- str_remove(text, "/.*?II")
  }
  
  if (grepl(".OG", text)) {
    text <- str_remove(text, "\\d\\.OG")
  }
  
  if (grepl("Stock", text)) {
    text <- str_remove(text, "/.*?Stock")
  }
  
  if (grepl("Top111210", text)) {
    text <- str_remove(text, "/.*?Top11")
  }
  
  if (grepl("Top", text)) {
    text <- gsub("\\d(?=.*\\d{4}\\b)(?!.*Top)", "", text, perl = TRUE)
    text <- str_remove(text, "/.*?Top")
  }
  
  if (grepl("EG", text)) {
    text <- str_remove(text, "/.*?EG")
  }
  
  if (grepl("/R", text)) {
    text <- gsub("\\d(?=.*\\d{4}\\b)(?!.*R)", "", text, perl = TRUE)
    text <- str_remove(text, "/.*?R")
  }
  
  if (grepl("/Tür", text)) {
    text <- gsub("\\d(?=.*\\d{4}\\b)(?!.*Tür)", "", text, perl = TRUE)
    text <- str_remove(text, "/.*?Tür")
  }
  
  if (grepl("Nord6020", text)) {
    text <- str_remove(text, "Nord")
  }
  
  # takes streetname
  (text1 <- sub("\\d.*", "", text))
  # this is sometimes necessary
  (text2 <- sub(".*[a-zäöüß]([A-ZÄÖÜ].*)", "\\1", text1))
  # gets the other part of the string
  (remaining_text <- sub(text1, "", text))
  # gets the street number and zip code
  (extracted <- sub("[A-ZÄÖÜ].*", "", remaining_text))
  # remove whitespace
  (extracted <- str_squish(extracted))
  # combines the three parts of an address
  (address_unformated <- paste0(text2, extracted))
  # adds a space between house number and zip code
  (address <- sub("(\\d{4})$", " \\1", address_unformated))

  
# This part creates lists which contain information on the therapists areas of
# expertise (which is an optional field for them to add to their profile) and
# their spoken languages (aside from German).
text2 <- xpathSApply(node_path[[j]][[i]], ".//div[contains(@class, 'pthCol')][2]", xmlValue)
# Make sure to first check whether it's just whitespace
text2 <- if_else(str_squish(text2) == "", NA, str_squish(text2))
# if not then extract everything including and after Arbeitsschwerpunkte
text2 <- if_else(grepl("Arbeitsschwerpunkte", text2), sub(".*Arbeitsschwerpunkte", "Arbeitsschwerpunkte", text2), text2)
# if contains Zusatzqualifikationen, delete it and everything after
text2 <- if_else(grepl("Zusatzqualifikationen", text2), sub("Zusatzqualifikationen.*", "", text2), text2)
# if then contains Sprachen, delete it and everything after
text3 <- if_else(grepl("Sprachen", text2), sub("Sprachen.*", "", text2), text2)
# areas of expertise
placeholder_list <- list(NA)
# I switched to R's if-else here because I thought it might fix something but
# this was not the issue. I won't revert it now since it does work.
expertise <- if (grepl("Arbeitsschwerpunkte", text2)) strsplit(text3, ", ") else placeholder_list
# make into named list (this was necessary when I didn't yet decide to make a tibble)
names(expertise) <- "expertise"
expertise$expertise[1] <- if_else(is.null(expertise$expertise) != T, str_remove(expertise$expertise[1], "Arbeitsschwerpunkte"), NA)

# pretty much same procedure as directly above
(languages <- if_else(grepl("Sprachen", text2), sub(".*Sprachen", "", text2), NA))
placeholder_list2 <- list(NA)
languages <- if(!is.na(languages) == T) strsplit(languages, ", ") else placeholder_list2
names(languages) <- "additional_languages"
languages$additional_languages[1] <- if_else(is.null(languages$additional_languages) != T, languages$additional_languages[1], NA)


# This binds everything together in a tibble as this format is lenient with the lists within a data frame structure.
  results <- rbind(results, tibble(name, title, specialisation, address, expertise, languages
                                       ))
  }
}

# To create some order in the data, more cleaning is done here. Specifically,
# as it seems like each profile can be freely edited by the people it represents,
# there is some variation in technically categorical variables despite an actually
# greater overlap. Some efforts to deal with this are made here but not to an extent
# where actual analyses could be based on it.
results$specialisation[results$specialisation == "Psychotherapeut:in"] <- NA
results$specialisation <- str_replace(results$specialisation, "(.*)(Psychotherapeut:in.*)", "\\1")
results$specialisation <- str_replace(results$specialisation, "(.*)(Supervisor:in.*)", "\\1")
results$specialisation <- str_replace(results$specialisation, "(.*)(Mediator:in.*)", "\\1")
results$specialisation <- str_replace(results$specialisation, "(.*)(,.*)", "\\1")
results$specialisation <- str_squish(results$specialisation)

# Remove white space and superfluous information
results$address <- str_squish(results$address)
results$address <- gsub("/.*?(\\d{4})", " \\1", results$address)
results$address <- str_squish(results$address)

# Make sure all umlauts and "ß" are made into something that will get accepted by
# the OpenStreetMaps API further down.
results$address <- stringi::stri_replace_all_fixed(
  results$address, 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"), 
  vectorize_all = FALSE
)

# The above for-loops did not work in this case.
results$address[results$address == "Amraserstrasse17 6020"] <- "Amraserstrasse 17 6020"

# Replace characterized NA's
results$languages[results$languages == "NA"] <- NA
results$expertise[results$expertise == "NANA"] <- NA



## Part 3: Geocoding & Mapping

library(tmap)
library(tmaptools)
library(sf)

# Recoding problematic addresses due to different error sources.
# OpenStreetMap has an error here, it's listed wrongly as the assigned address.
results$address[results$address == "Stephan-Fadinger-Strasse 4 4070"] <- "Keplerstrasse 4 4070"
# Misspelled addresses by the individuals responsible for profile content.
results$address[results$address == "Linzergasse 74 5020"] <- "Linzer Gasse 74 5020"
results$address[results$address == "Wolfgang-Schmaelzlgasse 5 1020"] <- "Wolfgang-Schmaelzl-Gasse 5 1020"
results$address[results$address == "Burgasse 10 9020"] <- "Burggasse 10 9020"
results$address[results$address == "Johan-Offner-Strasse 1 9400"] <- "Johann-Offner-Strasse 1 9400"
# Some results get misinterpreted as non-Austrian addresses. Adding "Austria" fixes this.
results$address <- paste(results$address, "Austria")

# At this point, no more changes are made to the data, so I write them now
# to plot stuff in the other script.
write_rds(results, "scrape_results_1.Rds")
results <- read_rds("scrape_results_1.Rds")

# This requests the latitudes and longitudes for each address from the Nominatim API.
geodata <- geocode_OSM(results$address)

# Save the data so we don't need to re-request each run.
write.csv(geodata, "geodata.csv")


# Read geodata back in.
geocodes <- read.csv("geodata.csv")

# This prepares the data to be used in the mapview function.
geocodes_test <- geocodes |> 
as_tibble() |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(geocode_type = "OSM")

# Opens interactive map with blue dots representing psychotherapists.
mapview::mapview(geocodes_test)


