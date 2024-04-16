################################## DSSS24 - 4 ### SELENIUM #####################
#
# This file introduces you to using a Selenium server to load websites and
# control them from within R. It's basically you using a regular browser, but
# instead of you yourself clicking and typing, you tell the server to do it.
#
# Setting this up is a bit of a hassle, but it works like this:
#
# STEP 1: INSTALL DOCKER DESKTOP
#
# The first step is to install Docker Desktop. Docker is a piece of software
# that simply put allows you to run additional computers on your computer, known
# as virtual machines. Learn more here: https://en.wikipedia.org/wiki/Docker_(software)
#
# To install Docker, download the corresponding setup file for your operating
# system from here: https://www.docker.com/get-started/
# NOTE: If you're using an M1-Mac or an older Intel-Mac, pay special attention
# to which file you're downloading!
#
# Then, install it just as you would.
#
# STEP 2: DOWNLOAD THE SELENIUM IMAGE
#
# Consult the manual on how to open the Docker "Dashboard". Then, in the search
# bar at the top, search for either:
#
# - "selenium/standalone-firefox" IF YOU ARE ON AN X86 MACHINE (applies to
#   basically everyone, unless you are using an M1 Mac)
# - "seleniarm/standalone-firefox" IF YOU ARE ON AN ARM MACHINE (applies
#   basically only to M1-Mac users)
#
# This will download the corresponding Selenium image (about 1.5GB in size)
#
# Then, in the list of images, click the "play" button to start that image in a
# new container (you'll have to confirm the settings). One setting that you may
# need to adapt is the port which I think defaults to 4445, and which I set to
# 4444. The latter is the default for the R library and it will help it
# automatically find the server below.
#
# Then, it will start the image and give you some output. The log output should
# end with a line indicating that Selenium has been successfully started.
#
# STEP 3: USE IT
#
# Now you're basically set. You will need to install the R package "selenium",
# either automatically or manually with `install.packages("selenium")`.

# Then you can load it:
library(selenium)
# DOCUMENTATION: https://ashbythorpe.github.io/selenium-r/index.html
# The documentation gives you a lot of info on how to use it. This file gives
# you an example.

# Initialize the session. At least for me, Selenium was smart enough to actually
# search for and find the server running in docker. You may have to provide the
# port number here, however. Note also that "browser" needs to correspond to the
# Selenium image you downloaded. There is the option to use "chrome", but then
# you'll need to download a different image.
session <- SeleniumSession$new(browser = "firefox")

# NOTE: Under the hood, the Selenium package uses -- funnily enough -- httr to
# communicate with the Selenium server, so if something goes wrong you will see
# an HTTP error (for example with a 404 or 500). Usually, the last line
# indicates what exactly the problem was, such as "No such element".

# NOTE2: Sessions can expire! And that relatively soon! So if you don't run
# anything for a minute or so, it may bark at you that there was no session. In
# that case, you'll need to run the line above again, and re-run anything in
# between.

# This allows you to check if the session works as expected.
session$status()

# Navigate to a webpage
session$navigate("http://www.google.com")

# navigate to another webpage
session$navigate("http://www.blocket.se")

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

# Find the GDPR modal (TODO: Didn't really find it yet …? Maybe one of you will.)
divs <- session$find_elements(using = "css selector", value = "div")
for (div in divs) {
  # This is what I used to search for the corresponding DIV with the correct ID.
  print(div$get_property("id"))
}

# Seems to be ID "gdprAdTransparencyModal__closeButton" (It isn't and I don't
# know why.)
close <- session$find_element(using="xpath", value="//div[@id='gdprAdTransparencyModal__closeButton']")
close$click() # If this was the right button, this will trigger a mouse click on it.
screenshot(session)

# What will work regardless of the cookie modal is to search for something. The
# page has a weird way of identifying the search bar (here I just used the
# aria-label property because that seemed the best idea to uniquely identify it.)
search_input <- session$find_element(using = 'xpath', '//input[@aria-label = "Vad vill du söka efter?"]')
# This sends the given letters to the element. The keys$enter part can also be
# used if you need other keys, such as backspace, or tab. (Typing keys$… should
# open an autocomplete that tells you which keys are available.)
search_input$send_keys("computational social science", keys$enter)
session$current_url() # Should now be the search page
screenshot(session) # Confirm by looking at it (unless you found a way to close
# the cookie banner, it will still be visible.)

## Navigate back and search for a new term
session$back()
search_input <- session$find_element(using = 'xpath', '//input[@aria-label = "Vad vill du söka efter?"]')
search_input$send_keys("corona", keys$enter)
session$current_url()
screenshot(session)

## Gets source code (then you can use classic methods; remember to load XML)
src <- session$get_page_source()
htmlParse(src)

# Finally, you always have to close a session! Once you are done, you should also
# stop the Docker image to free up system resources (remember: As long as it is
# running there's basically a second PC running on your computer.)
session$close()
rm(session)
gc()