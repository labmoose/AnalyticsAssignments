#Load Tidyverse
library(tidyverse)



# Create a tibble of all participants (p) in the gift exchange; These will be the "Santas"
p = tibble(Santa = c("Chris", "Carla", "Jim", "Pam", 
                     "Leslie", "Ben", "Mitch", "Cameron", 
                     "Louis", "Jessica", "Dwight", "Jeremy", 
                     "Ray", "Andy"))


# Create a tibble (so) for the partner of each participant, which they cannot be matched with
so = tibble(SO = c(NA, "Chris", "Pam", "Jim", 
                   "Ben", "Leslie", "Cameron", "MItch", 
                   "Jessica", "Louis", NA, NA, NA, NA))


# Create a tibble (f) for the frenemies of each participant which they cannot be matched with
f = tibble(Frenemy = c(NA, NA, "Dwight", NA, "Jeremy", 
                       NA, NA, NA, "Ray", NA, "Jim", 
                       "Leslie", "Louis", NA))

# Filter Carla out of the random draw, as she is a "fixed" set with Chris
d = filter(p, Santa != "Carla")
dd = tibble(Santa = c("Carla"))



# Randomly draw names of participants to be matched as recipients, and add Carla back to the top to match w/ Chris
# Rename this column "Recipients" before we add all columns to a single tibble
r = sample_n(d, 13, replace = FALSE)
r = bind_rows(dd, r)
colnames(r)<-("Recipients")


# Create the exchange tibble, combining the Santas (p), SOs (so), Frenemies (f), and Recipients (r)
exchange = bind_cols(p, so) %>% 
  bind_cols(f) %>% 
  bind_cols(r)


### Use logical checks to look for disallowed matches (recipient == p, so, or f)

# Logical T/F test for recipient & santa names, return a TRUE result for disallowed matches
testself <- c(r == p)

# Check the vector testself for TRUE (disallowed matches); Save result as new variable to use later (selfcheck)
selfcheck <- TRUE %in% testself

# Logical T/F test for recipient & SO, return a TRUE result for disallowed matches
testso <- c(r == so)

# Check the vector testso for TRUE (dissallowed matches); Save result as a new variable to use later (socheck)
socheck <- TRUE %in% testso

# Logical T/F test for recipient & frenemy, return a TRUE result for disallowed matches
testfrn <- c(r == f)

# Check the vector testfn for TRUE (disallowed matches); Save result as new variable to use later (frenemycheck)
frenemycheck <- TRUE %in% testfrn


# Create a loop to check for disallowed matches
while((selfcheck == TRUE) | (socheck == TRUE) | (frenemycheck == TRUE)) {
  
  # If disallowed matches = TRUE, then rerun the sample & checks until there are no disallowed matches
  d = filter(p, Santa != "Carla")
  dd = tibble(Santa = c("Carla"))
  r = sample_n(d, 13, replace = FALSE)
  r = bind_rows(dd, r)
  colnames(r)<-("Recipients")
  exchange = bind_cols(p, so) %>% 
    bind_cols(f) %>% 
    bind_cols(r)
  testself <- c(r == p)
  selfcheck <- TRUE %in% testself
  testso <- c(r == so)
  socheck <- TRUE %in% testso
  testfrn <- c(r == f)
  frenemycheck <- TRUE %in% testfrn
}

# After self-correcting the matches, create a quick debugging tibble in case
# there are concerns about disallowed matches
DEBUG = bind_cols(p, r) %>% 
  bind_cols(so) %>% 
  bind_cols(f) %>% 
  bind_cols(testself) %>% 
  bind_cols(testso) %>% 
  bind_cols(testfrn)


# And the nice and clear pair-matching for the company who wanted it
match = bind_cols(p, r)
match


# Add a tibble for a range of gift prices up to $100.00
dlr = tibble(price = seq(from=0, to=100, by=0.01))

# Chris paid $364.97 for his gift to Carla, so we'll put that info in his paid spot
cp = tibble(price = 364.97)

# Randomly select a price for each other Santa to have paid for the gift purchased
# then add Chris's purchase price back to the top of the list
pd <- sample_n(dlr, 13, replace = TRUE)
paid = bind_rows(cp, pd)


# Add the tibble paid as an add'l column in match
matchplus = bind_cols(match, paid)


# Create a tibble for a range of dates of purchase- let's assume from Dec. 1st- Dec. 5th
dt = tibble(Purchased = as.Date(x=runif(13, min = as.Date("2019-12-01"), max = as.Date("2019-12-05")), origin = "1970-01-01"))

# Chris bought his gift on April 4th 2019, so he gets his own special early bird tibble
cb = tibble(Purchased = as.Date("2019-04-04"))

# Bind the rows of cb and dt to add Chris's date of purchase to the randomized purchase dates.
dpurch = bind_rows(cb, dt)

# Add the date of purchase column to the match tibble
matchplus = bind_cols(matchplus, dpurch)
options(pillar.sigfig = 5)
matchplus

# Exporting code as csv file
write.csv(matchplus, file = "C:\\Users\\labmo\\Documents\\R\\R projects\\Santa Project\\SantaMatchPlus.csv", row.names = FALSE)
