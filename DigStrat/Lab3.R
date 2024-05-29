################################## DSSS24 - 3 ##################################
#
# The third lab will finally get to extracting data from websites. Here you will
# learn how to extract the specific pieces of information from the mess of HTML.
# This lab introduces XPaths, which you will need to extract those pieces of
# content you are actually interested in.
#
# For more info:
# * https://en.wikipedia.org/wiki/XPath
#
# Hendrik Erz (initially Etienne Ollion & J. Boelaert)

# PRELIMINARIES
# =============
#
# As last time, let's first load the required packages and set the working
# directory.
library(httr)
library(XML)

setwd('C:/Users/marcs/Desktop/Labs/CSS_Labs_early_24/DigStrat') # Change this to wherever you want to work in.

# THE FIRST SCRAPE
# ================
#
# Download the content of the page https://planecrashinfo.com/accidents.htm and
# parse the HTML into a DOM tree.
url <- "https://aviation-safety.net/database/"
response <- GET(url)
  page_content <- content(response, as = "text")
  dom <- htmlParse(response)
  
  # The website contains a ton of links to various sub pages. Let's see how many
  # we got. How many are there?
  length(xpathSApply(dom, "//a")) #135

# Extract the titles of those links
link_titles <- xpathSApply(dom, "//a", xmlValue)
print(link_titles)

# Extract the links themselves
links <- xpathSApply(dom, "//a", xmlAttrs)
print(links)

# EXERXCISE: PLANE CRASH DATABASE
# ================
#
# Now, let's not just inspect a website, but actually put its contents into a
# data frame.

# Download the page for 1950 plane crashes
url <- 'http://planecrashinfo.com/1950/1950.htm'
response <- GET(url)
  http_status(response) # OK
page_content <- content(response, as = "text")
  
  # On some OS, you may get a "success" status, but no content.
  # Try the following line to extract it properly:
  page_content <- content(response, type = "text", encoding = "ISO-8859-1")

# Write it on your hard drive
writeLines(page_content, "planecrash_1950.html") # Go check it

# Parse the page's contents
dom <- htmlParse(page_content)
  
  # Now, let's extract the date, the location/operator, and the aircraft.
  # Write XPaths that select the described elements. What do they have in common?
  column1_row1 <- xpathSApply(dom, "//tr[2]//td[1]")
column1_row2 <- xpathSApply(dom, "//tr[3]//td[1]")

# Scrape the date
date <- xpathSApply(dom, "//td[1]", xmlValue)
#//a

# Scrape the location/operator
location <- xpathSApply(dom, "//td[2]", xmlValue)

# Scrape the Aircraft
aircraft <- xpathSApply(dom, "//td[3]", xmlValue)

# Bind them together and construct a data frame from it.
header <- c(date[1], location[1], aircraft[1])
df <- data.frame(date = date[-1], location = location[-1], aircraft = aircraft[-1])
names(df) <- header # What does this line do?
head(df)

# You can also go by rows. This is not advised with tabular data, but comes in
# helpful when you are dealing with non-tabular data.
url <- "http://www.planecrashinfo.com/accidents.htm"
response <- GET(url)
page_content <- content(response, as = 'text')

# Again, on some OS, you may have to write
page_content <- GET(url) |> content(type = "text", encoding = "ISO-8859-1")

writeLines(page_content, "accidents.htm") # Go check it

dom <- htmlParse(page_content)

# Select the information for the **first node**
node1 <- xpathSApply(dom, "//tr")[[2]]
#print(xpathSApply(node1, "***", xmlValue)) # don't know what is asked for here
## Note differences here between the 2

# Extract the information for columns 1, 2, and 7 from this node (note the ".")
cell1 <- xpathSApply(node1, ".//td[1]", xmlValue)
  cell2 <- xpathSApply(node1, ".//td[2]", xmlValue)
  cell7 <- xpathSApply(node1, ".//td[7]", xmlValue)
  
  # Same thing for node 2
  node2 <- xpathSApply(dom, "//tr")[[3]]
  cell21 <- xpathSApply(node2, ".//td[1]", xmlValue)
  cell22 <- xpathSApply(node2, ".//td[2]", xmlValue)
  cell27 <- xpathSApply(node2, ".//td[7]", xmlValue)
  
  # Same thing for node 3
    node3 <- xpathSApply(dom, "//tr")[[4]]
  cell31 <- xpathSApply(node3, ".//td[1]", xmlValue)
  cell32 <- xpathSApply(node3, ".//td[2]", xmlValue)
  cell37 <- xpathSApply(node3, ".//td[7]", xmlValue)
  
  # EXERXCISE: SCRAPING SVT.SE
  # ==========================
#
# Let's look at a second website: The economy news from svt.se. Here, the data
# of interest is not presented as a table, which means that working row-wise is
# a much better approach than working column-wise.

# First, as always, retrieve the website and parse it into a DOM tree.
url <- "https://www.svt.se/nyheter/ekonomi/"
response <- ***
  dom <- ***
  
  # The first challenge: Retrieve all DOM nodes that contain an article's info
  news <- xpathSApply(dom, '***')

# Next, print out the link to the full article, its title, and the teaser for
# the very first item.
attrs <- xpathSApply(news[[1]], '***', ***)
title <- xpathSApply(news[[1]], '***', ***)
subtitle <- xpathSApply(news[[1]], "***", ***)
print(attrs[2]) # Why [2]?
print(title)
print(subtitle)

# Finally, generalize this to extract the same info for all news on the page.
# TODO

# NOTE: You should devise a strategy in case one of these elements is missing

################################ THE END #######################################