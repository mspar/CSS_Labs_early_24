################################## DSSS24 - 4 ##################################
#
# The fourth lab's focus is on loops and storing information on disk. Here you
# will use the previous lab's efforts and store everything in data frames.
#
# Hendrik Erz (initially Etienne Ollion & J. Boelaert)

# PRELIMINARIES
# =============
#
# You know the drill by now!
library(httr)
library(XML)

setwd('/path/to/folder') # Change this to wherever you want to work in.

# PAGINATION
# ==========
#
# The two most common applications for loops in scraping the web is (1) extract
# a list of nodes from a single web page and (2) download multiple paginated
# websites one after another in a loop.
#
# First, let us have a look at a website that uses pagination:
# https://www.hendrik-erz.de/blog
# While clicking through the pages you will see how the pagination works. The
# most basic way of scraping is to simply pre-calculate all URLs you will need
# to visit.

## Create a vetor of urls /blog/1 to /blog/10
base_url <- "https://www.hendrik-erz.de/blog/"
urls <- paste0(base_url, 1:10) # NOTE: You can also use seq()
print(urls)

# Appending additional info is also easy:
paste0(base_url, 1:10, ".htm")

# Some older websites paginate not by providing a page number, but rather by
# indicating what the first element is that should appear on the page. Example:
# example.com?start=1 -> First page
# example.com?start=101 -> Second page, assuming 100 items per page
n <- 1200 # Let's assume 1,200 items
page_size <- 100
first_page <- 1 # Might be 0
last_page <- n - page_size + first_page
sequence <- seq(from = first_page, to = last_page, by = page_size)
urls <- paste0("https://www.example.com/?start=", sequence)

# To then use such a sequence, we use for-loops:
for (page in 1:10) {
  print(page)
  Sys.sleep(1) # Might come in handy for reducing the load on the web server.
}

for (start in sequence) {
  print(start)
}

# CAPTURING PAGINATED DATA WITH FOR LOOPS
# =======================================
#
# After having understood how to capture multiple paginated websites, it is time
# to retrieve them and store them in a data object so that you can work on the
# data later.

# Let us first capture all pages of the blog:
base_url <- "https://www.hendrik-erz.de/blog/"
urls <- paste0(base_url, 1:10)

# Store everything in a list
results <- list()
for (i in 1:10) {
  cat('\rFetching page ', i) # Bonus: Indicate which page we are currently grabbing
  results[[i]] <- GET(urls[i])
}

# Now we have the responses, but still no parsed HTML. That's what we will do now.
html <- list()
for (i in 1:10) {
  html[[i]] <- htmlParse(results[[i]])
}

# Alternatively, parse the page using lapply
html <- lapply(results, htmlParse)

# Store the pages on your disk
for (i in 1:10) {
  page_content <- content(results[[i]], as = 'text')
  writeLines(page_content, paste0('blog_page', i, '.html'))
}

# TIP: Use lapply whenever you don't need the index (i), but use for-loops if
#      you need it, e.g., for naming files appropriately.

# ITERATING OVER NODES
# ====================
#
# Aside from pagination, you will also often have to iterate over individual
# nodes. Here, you will learn how to extract a series of news teasers, store
# them into a data frame, and save that to disk.

url <- "https://www.svt.se/nyheter/ekonomi/"
response <- GET(url)
dom <- htmlParse(response)

# The first challenge: Retrieve all DOM nodes that contain an article's info
news <- xpathSApply(dom, '//article[contains(@class, "nyh_teaser")]')

# Finally, generalize this to extract the same info for all news on the page.
results <- NULL
for (info in news) {
  # Extract the required content
  href <- xpathSApply(info, './a[@href]', xmlAttrs)[2]
  title <- xpathSApply(info, './/span[@class="nyh_teaser__heading-title"]', xmlValue)
  subtitle <- xpathSApply(info, ".//div[@class='nyh_teaser__textcontent']", xmlValue)
  
  # Ensure nothing is undefined
  if (length(href) == 0) {
    href <- NA
  }
  if (length(title) == 0) {
    title <- NA
  }
  if (length(subtitle) == 0) {
    subtitle <- NA
  }
  
  results <- rbind(results, data.frame(href, title, subtitle))
} # Do not forget to close the loop again!

# Let's have a look at one example:
print(results[1, ])

# Save the data frame down. Since it contains exclusively text, TSV is highly
# recommended.
write.table(results, "nyheter.tsv", row.names = FALSE, sep = "\t")

# BONUS EXERCISE: COLLECTING DATA
#
# If you finished quickly, here's a bonus task:
#
# 1.) Visit https://www.planecrashinfo.com/database.htm
# 2.) Extract the various links for the years from the page
# 3.) Scrape each year-website and store the date, location, and aircraft type
#     in a data frame and save it to disk.

################################ THE END #######################################