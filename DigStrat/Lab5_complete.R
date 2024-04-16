################################## DSSS24 - 5 ##################################
#
# The final lab will introduce you to a final tool that will come in very
# helpful when scraping data: regular expressions. Remember that all you get
# from a website is text, but to run any analysis, you'll need to convert that
# to numbers somehow. Regular expressions allow you to subset the contents of
# the website even further, not just to the node-level, but below that. They
# allow you to extract very specific pieces of text that will make it easier for
# you to work with.
#
# Regular expressions in R work using the so-called PERL syntax. This is only
# one of several "flavors" of RegExp (or regex). To learn more on regex in
# general, as always have a look at the Wikipedia article:
# https://en.wikipedia.org/wiki/Regular_expression
#
# Additionally, regex can be difficult to fully understand and it has many weird
# behaviors that take time to fully understand. A good resource on everything
# regex is https://www.regular-expressions.info/. This website also contains a
# sub-page dedicated to regular expressions in R:
#
# https://www.regular-expressions.info/rlanguage.html
#
# Finally, writing regex is akin to writing in yet another programming language,
# since it is so complex. You will often make errors and not extract what you
# intended to. Thus, you should bookmark a regex tester website. One very good
# one that even explains regex to you is https://www.regex101.com/. The proper
# flavor to select here for displaying the regex correctly is PCRE2.
#
# CAUTION: ALWAYS SET THE PARAMETER `perl=TRUE` FOR ANY FUNCTION THAT ACCEPTS
# THAT, SINCE OTHERWISE R WILL FALL BACK TO PCRE1, WHICH MEANS YOU'LL BE MISSING
# OUT ON SOME FEATURES.
#
# Hendrik Erz (initially Etienne Ollion & J. Boelaert)

# PRELIMINARIES
# =============
#
# You know the drill by now!
library(httr)
library(XML)

setwd('C:/Users/marcs/Desktop/Labs/CSS_Labs_early_24/DigStrat') # Change this to wherever you want to work in.

# CLEANING DATA WITH REGEX: ICANN TLD REGISTRY
# ============================================
#
# Today, we will have a look at the ICANN registry of TLDs. This is a list of
# every registered top-level-domain that the ICANN maintains. It lists each
# top-level-domain (such as .com, .org, or .se) with its corresponding contact
# information.
#
# We'll assume that we need the contact information in a parsed state. Thus, we
# first follow the usual way of downloading the website and parsing its HTML.
# Then, the star of today's show will enter, which will allow us to extract info
# such as telephone numbers and email addresses.

# First, download the data and parse the HTML into a DOM tree.
url <- "https://www.icann.org/resources/pages/listing-2012-02-25-en"
response <- GET(url)
page_content <- content(response, as = 'text')

dom <- htmlParse(page_content)

# Second, extract the data store it into a data frame. There is a trick when
# dealing with tables on websites: You can simply parse the table immediately
# into a data frame like so:
all_tables <- xpathSApply(dom, "//table") # Returns a list of all tables
main_table <- all_tables[[1]] # Why do we do this?
table <- readHTMLTable(main_table)

# View the column headers: You will notice that the function has automatically
# extracted the first row's contents as column names.
names(table)

# Let's have a look at the fourth column:
print(table[1, 4])

# As you can see, it's extremely dirty, so let us clean it up now!

# CLEANING WHITESPACE
# ===================
#
# The first order of business usually is to remove excess whitespace. In this
# particular case, this consists of excess tab-characters, and unusual newlines.

# For ease of access, we'll just copy out the column:
contact <- table$Contacts

contact <- gsub(pattern = "\\t+", replacement = "", x = contact, perl = T)
# You don't have to name all arguments to keep it short
contact <- gsub("\\t+", "", contact, perl = T)

print(contact[1]) # Double check your work

# Next, replace CRLF with regular line feeds (more info: https://en.wikipedia.org/wiki/Newline)
contact <- gsub("[\r\n]+", "\n", contact, perl = T)

print(contact[1]) # Double check your work

# EXTRACTING PHONE NUMBERS
# ========================
#
# The final lesson of today will be to properly extract telephone and fax
# numbers. There are various formats in which these could be stored, and luckily
# (for our training purposes), the website really doesn't care to keep the
# format uniform, so you'll have to match a ton of cases.

# First, the ugly way: Simply match all characters that exist in these numbers:
# regexpr will return a list of matched indices ...
matches <- regexpr("[+\\d \\(\\)\\-\\.]+", contact, perl = T)
# ... that regmatches will then extract into a character vector.
numbers <- regmatches(contact, matches)

head(numbers) # Double check

# In this first iteration, this will match a ton of irrelevant things. So let's
# make the regex more strict and require at least three characters:
matches <- regexpr("[+\\d \\(\\)\\-\\.]{3,}", contact, perl = T)
numbers <- regmatches(contact, matches)
head(numbers) # Double check

# Still a lot of issues. Next step: Require the numbers to start with a plus.
matches <- regexpr("\\+[\\d \\(\\)\\-\\.]{3,}", contact, perl = T)
numbers <- regmatches(contact, matches)
head(numbers) # Double check


# A more elegant way that makes use of some additional regex features
matches <- regexpr("\\+\\d+(?: \\(\\d+\\))?[\\d .-]+", contact, perl = T)
telephone <- regmatches(contact, matches)

# BONUS EXERCISE: FUNCTION AND LAPPLY
# ===================================
#
# As a final task of today, combine all of this into a function that can be
# run across the data frame to automatically apply all of these various regex
# transformations so that we end up with a column of simple phone numbers.

parse_contacts_column <- function(value) {
  # TODO: Implement your function so that the call below works
  return(value)
}

table$Contacts <- lapply(table$Contacts, parse_contacts_column)

# BONUS EXERCISE 2: EXTRACT EMAIL ADDRESSES
# =========================================
#
# A regular expression that is used by many different programmers oftentimes a
# day is one to extract email addresses. If you're here, I pose to you the
# challenge to extract the email addresses instead:

extract_email_addresses <- function(value) {
  email <- NA
  # TODO: Implement your function so that the call below works
  return(email)
}

table$emails <- lapply(table$Contacts, extract_email_addresses)

################################ THE END #######################################