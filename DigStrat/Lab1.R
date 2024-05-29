################################## DSSS24 - 1 ##################################
#
# This is the introductory lab for DSSS24. Its purpose is to refresh a bit of R
# where it is important for following the rest of the course. See this as a form
# of "cheat-sheet" that you can refer to throughout the course. Work through it
# to make sure you understand what the various things you will see here do, and
# that you understand them.
#
# NOTE: If you don't understand something, or if something seems weird, ask! You
# are likely not the only one who wonders. Also note that this cheatsheet is far
# from complete; the help function will be your best friend throughout this
# course.
#
# Hendrik Erz (initially Etienne Ollion & J. Boelaert)

# DATA TYPES
# ==========
#
# R supports a set of fundamental ("primitive") data types as well as derived
# ones. The primitives it supports are Booleans ("logical"), integers (whole
# numbers), floats (floating point double-precision numbers), complex numbers
# (w/ an imaginary part), and characters (incl. strings). Since R is a
# statistical language, there are also formulas (y ~ x1 + x2) as a built-in
# data type.
#
# The derived data types include vectors, lists, matrices, and data frames.
# Furthermore, there are data types that are only returned by functions, such as
# model results (e.g., when running `lm()`), graph objects (will be important in
# the network analysis course), and others. The exact differences are not that
# important, since almost all objects behave as-if they were named lists or data
# frames (which is how you'll mostly interact with them).
#
# Each package may add more as necessary. You will, for example, come across
# "Response" objects or various XML objects that are defined in the scraping
# libraries that we use.
#
# For each data type, there are two fundamental things you can do with them:
# assign them (to variables) or perform operations on them. As long as we are
# dealing with numbers, these are mostly the usual mathematical operations, but
# as data types become more complex, you will mostly call functions to perform
# some operation. In the following, important base data types are introduced.

# Before we do anything, though, make sure you have the appropriate R-version:
version$version.string # Should be at least R version 4.1
# If in doubt, just update your R version before the course.
# NOTE: These labs have been tested with R version 4.3.3 (2024-02-29).

## PRIMITIVES
## ==========

### Boolean

#### Assignment

correct <- TRUE # Or: T
wrong <- FALSE # Or: F

#### Operations

correct & wrong # AND
correct & !wrong # AND + NOT
correct | wrong # OR
!correct & wrong # NOT + AND
xor(correct, wrong) # XOR
xor(correct, !wrong) # XOR + NOT

### Numbers

#### Assignment

integer <- 1L # Numbers are stored as doubles, unless you explicitly add L
float <- 2.35 # In R called "double" or just "numeric"
complex <- 2i # Not important, unless you start doing frequency analysis

#### Operations

integer + 3 # What data type will this become?
float - 1L
complex * 3
float / 3
float %% integer # Modulus
2.71828 ^ float # Do you recognize this operation?

# Some built-in functions (for more see the Help)
sum(integer, float)
mean(integer, float)
log(2.71828 ^ float)

### Strings

#### Assignment

hello <- "Hello"
world <- "World"
a_char <- '!'

#### Operations

# NOTE: Many use packages such as stringr for easier handling of strings
print(hello)

paste(hello, world)
paste0(hello, world) # Equivalent to paste(hello, world, sep = '')
paste(hello, world, sep = ', ')
paste(hello, paste0(world, a_char), sep = ', ')

cat(hello) # -> Does no post-processing

# \n -> LF
# \n\r -> LFCR
# \r\n -> CRLF

## DERIVED DATA TYPES
## ==================

### Vectors

#### Assignment

empty_vector <- vector('numeric', length = 10L)
numbers <- c(1, 2, 3, 4, 5) # c is short for "combine (elements into a vector)"
mixed <- c(1L, "hello", 2.3, "world", 'x')
range <- 1:11 # Shorthand for seq(from = 1, to = 11, by = 1)

range <- seq(from = 1, to = 11, by = 1) # What is the result's difference to 1:11?
range <- seq(from = 2, to = 10, by = 2)

# Vectors always have the class/type of the least common denominator; the chain
# is: logical -> integer -> numeric -> complex -> character
class(numbers)
typeof(numbers) # What's the difference between typeof and class?
class(mixed)
typeof(mixed)

#### Operations

# -> take another look at this, "only for lists do you need double brackets"

numbers[1] # Arrays in R always start with 1, not 0 as in most other languages
mixed[2]
mixed[[2]] <- "Hello!" # Re-assigns elements - vectors are mutable
mixed[2] <- "Hello?"
print(mixed)

# If we're dealing with numeric vectors, a lot works like in high school math:
# Summing, meaning, or vector/matrix multiplications
sum(numbers)
mean(numbers)
numbers * float

# You can coerce vectors into a specific data type. This operation is
# destructive if you move "up" the chain (e.g., complex to numeric), or retains
# the information if you move down (e.g., integer to numeric)
all_numbers <- as.numeric(mixed) # Produces NAs -> loses information
all_strings <- as.character(mixed) # Wraps numbers in quotes -> keeps information

### Lists

#### Assignment

# Lists can be unnamed or named
unnamed_list <- list("Peter", 62, "Stockholm")
named_list <- list(name = "Peter", age = 62, city = "Stockholm")

# Lists can contain other lists/vectors
people <- list(name = c("Peter", "Hendrik"), age = c(62, 33), city = c("Stockholm", "Norrköping"))

# At this stage, the list is effectively a data frame:
people_df <- data.frame(people)

#### Operations

# What is the difference between these two operations?
unnamed_list[1]
unnamed_list[[1]]

# Named lists have the same access operations as vectors, but additionally let
# you access properties using its name (if you provided one)
named_list[1]
named_list[[1]]
named_list$name

numeric_list <- list(2, 3, 4, 5)
numeric_list + 1 # Won't work
numbers + 1 # But this will
people$age + 1

### DataFrame

#### Assignment

df <- data.frame(name = c("Peter", "Hendrik"), age = c(62, 33), city = c("Stockholm", "Norrköping"))
df <- data.frame(people) # You can convert many things into data frames

# You can also assign lists in loops, which you will often need to do
people <- list(
  list(name = "Peter", age = 62, city = "Stockholm"),
  list(name = "Hendrik", age = 33, city = "Norrköping")
)

df <- NULL
for (i in 1:2) {
  df <- rbind(df, data.frame(people[[i]]))
}

#### Operations

# Subsetting
df[df$age < 40, ] # NOTE the comma here. What does it do?
# What does df$age < 40 do exactly?
condition <- df$age < 40

# Use multiple conditions
df[(df$age < 40) & (df$city != 'Stockholm'), ]

# Creating new variables based on others
current_year <- as.integer(format(Sys.Date(), "%Y"))
df$birth_year <- current_year - df$age
print(df)

# Equivalent, but useful for adding multiple new columns in an additional data
# frame, or populating programmatically
df <- cbind(df, list(stockholm = df$city == "Stockholm"))
print(df)

# Some standard functions
summary(df)
head(df)
tail(df)
nrow(df)
ncol(df)

### Matrices

#### Assignment

d <- matrix(data = 1:9, nrow = 3, ncol = 3)
d <- matrix(data = 1:9, nrow = 3, ncol = 3, byrow = T)
d <- matrix(data = c(1:8, 'hello'), nrow = 3, ncol = 3, byrow = T)

#### Operations

# Operations work similarly to vectors (since matrices are effectively
# multi-dimensional vectors)

d <- matrix(data = 1:9, nrow = 3, ncol = 3)
d[1, 3]
d[1:2, 3]
d[2, 1:3]

# What happens with each re-assignment to the other elements? What does it tell
# you about the `byrow` parameter above?
d[3, 3] <- float
print(d)
d[3, 1] <- float
print(d)

d * complex

d * numbers # What happens?

# What does this function do?
diag(d) <- NA
print(d)

# WORKING WITH CSV AND TSV FILES
# ==============================
#
# During this course, you will frequently create data frames and you will need
# to work with them. The most common format to store them is as CSV, which is
# short for "comma-separated values". When you are working with text data, it
# makes more sense to use TSV-files, however -- tab-separated values. The
# reason is that text often contains commas, and TSV-files reduce the chances of
# accidentally mangling your data.
#
# R also allows you to save down arbitrary text into files -- such as HTML code.

# Let's load an example dataset
data(iris)
?iris # Short-hand for searching the help for an object

# After you're done collecting or working with your data, you will want to save
# it into a file so that it persists across R sessions (do not trust R to always
# be able to load its RData-file!)

# For that, first make sure to navigate to the folder you want to save your work in

getwd() # What folder does R use right now?
setwd("/path/to/your/folder") # Change it so you know where the data is.
# -> find a smooth way to make this work on any new computer!

write.csv(iris, "iris.csv", row.names = FALSE) # Go find it on your disk
# Similar, but as a TSV
write.table(iris, file = "iris.tsv", row.names = FALSE, sep = "\t")

# Read it back from disk
iris <- read.csv("iris.csv")
iris <- read.delim('iris.tsv')
head(iris)

# CONDITIONALS, LOOPS, AND CONTROL FLOW
# =====================================
#
# The final ingredient to R concerns further syntax that you will need to scrape
# data.
#
# Conditionals are used to treat data differently depending on what value they
# hold. This is commonly referred to as "branching". Loops are used to repeat
# some code multiple times, e.g., to iterate over a list of values.
#
# Finally, functions are used to define reusable functionality, e.g., taking a
# value and squaring it. Usually, you will put reusable code into a function and
# then, in a for- or while-loop, call this function.
#
# For these things, curly brackets are not always required by R, but I fully
# recommend them. This will allow you to fold code and visually see where some
# block of statements begins, and where it ends.

# Simple if-statement
if (numbers[1] == 3) {
  print("numbers[1] is 3")
} else {
  print("numbers[1] is not 3")
}

## Else-if for multiple conditions
if (numbers[1] < 10) {
  print("numbers[1] is less than 10")
} else if (numbers[1] < 20) {
  print("numbers[1] is between 10 and 19")
} else {
  print("numbers[1] is larger than or equal to 20")
}

# Every "if" check will be coerced to "TRUE" or "FALSE", so try to always
# provide Booleans. R will coerce anything non-Boolean to either true or false,
# so using Booleans always will prevent any unwanted behavior.
if (TRUE) {
  print("This will always print")
}

if (FALSE) {
  print("This will never print")
}

# Try to guess what the output of the following will be -- you may be surprised.
as.logical(1)
as.logical(0)
as.logical('')
as.logical(NA)
as.logical(NULL)
as.logical('TRUE')
as.logical('1')

switch()

# For-loops

for (i in seq_len(length(numbers))) {
  print(numbers[i]) # Access by index
}

for (item in numbers) {
  print(item) # Iterate over the numbers directly
}

# For more info on control flow, check its help page:
?Control

# Functions are always defined like this:
my_mean <- function(nums) {
  # They can contain anything that we've seen thus far, including control flow,
  # and they can call other functions as well.
  
  # Remember to always return something from them, if applicable. Sometimes, you
  # will want to define reusable plotting-functions that take, e.g., some data
  # frame, run a regression and simply plot the results. Those functions
  # typically don't return.
  return(sum(nums) / length(nums))
}

# Verify it works
mean(c(1, 2, 3)) == my_mean(c(1, 2, 3))

################################ THE END #######################################