library(selenium)
library(httr)
library(XML)
library(tidyverse)

session <- SeleniumSession$new(browser = "firefox")

session$status()

# Navigate to a webpage
session$navigate("https://www.psychotherapie.at/patientinnen/psychotherapeutinnen-suche")

# Retrieve the URL currently loaded in the browser
session$current_url()

# Make a screenshot and have a look at it to see that the Selenium browser is
# viewing the correct website. This function is only required because the
# library gives back base64 encoded image data, and I haven't found a proper way
# of displaying that data in a plot. So you'll have to open the file
# "screenshot.png" after running the function. Not ideal, but then, you know,
# web design is war.
screenshot <- function(what) {
  base64 <- what$screenshot()
  raw <- base64enc::base64decode(base64)
  writeBin(raw, "screenshot.png")
}

# Take a screenshot. NOTE: You can also take screenshots of individual elements
# IF these are visible on the website. Selenium will bark at you if the elements
# are not visible.
screenshot(session)

# actions_stream(
#   actions_scroll(x = 1, y = 1, delta_x = 0, delta_y = 1000, duration = 0.5)
# )

# screenshot(session)

cookies <- session$find_element(using = "css selector", value = ".eu-cookie-compliance-banner .agree-button")
cookies$click()

# screenshot(session)


# Click "Search"
search <- session$find_element(using = "xpath", value = "//input[(((count(preceding-sibling::*) + 1) = 20) and parent::*)]")
search$click()
session$current_url()

src1 <- session$get_page_source()
parsed1 <- htmlParse(src1)

next_page <- session$find_element(using = "xpath", value = '//a[contains(@class, "page-link-next")]')
next_page$click()
session$current_url()

src2 <- session$get_page_source()
parsed2 <- htmlParse(src2)


# next up: loop
src3_5 <- list()
for (i in 1:3) {
  next_page <- session$find_element(using = "xpath", value = '//a[contains(@class, "page-link-next")]')
  next_page$click()
  Sys.sleep(1)
  src3_5[[i]] <- htmlParse(session$get_page_source())
  Sys.sleep(1)
}

# next up - alternative way:


# Finally, you always have to close a session! Once you are done, you should also
# stop the Docker image to free up system resources (remember: As long as it is
# running there's basically a second PC running on your computer.)
session$close()
rm(session)
gc()
