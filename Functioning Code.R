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


# Create a tibble (d) for the names of all partipants to be drawn that aren't recipients in pre-fixed set: 
# "Chris" has a gift for "Carla", so "Carla" will be removed from the random sampling.



### !!!Every time I've tried making this a tibble I've been able to sample, but not get Carla's ass back
### at the top of the list of names to be "fixed" with Chris, gdi.  So it's back to being a temp vector!!!
d = c("Chris", "Jim", "Pam", "Leslie", "Ben", "Cameron", "Mitch", 
      "Louis", "Jessica", "Dwight", "Jeremy", "Ray", "Andy")

# Use "drawn" to collect randomly sampled participant names without replacement from "d" (draw)

### !!!This is also a temporary vector again, as I couldn't get the fucking bind_col to work without a ton
### of NAs showing up instead of, you know, BINDING THE FUCKING COLUMNS.  Two hours later, fuck it.
drawn = sample(d, 13, replace = FALSE)


# Add the sampled participants into the recipient vector

### !!! SEE??? LOOK AT THIS NICE FUCKING TIBBLE NOW! WHY COULDN'T WE JUST bind_col to get here??? D:<
r = tibble(Recipient = c("Carla", drawn))


# Create the exchange tibble, combining the Santas (p), SOs (so), Frenemies (f), and Recipients (r)

### !!! and THESE gotdamn tibbles are bind_cols like they know wtf is up kyle.  So what the hell is up???
exchange = bind_cols(p, so) %>% 
  bind_cols(f) %>% 
  bind_cols(r)


# Print dataframe exchange to see all categories are present: Participants (p), SOs (so), Frenemies (f), and  Recipient.
exchange



### Use logical checks to look for disallowed matches (recipient == p, so, or f)

# Logical T/F test for recipient & santa names, return a TRUE result for disallowed matches
testself <- c(r == p)

# Check the vector testself for TRUE (disallowed matches); Save result as new variable to use later (selfcheck)
selfcheck <- TRUE %in% testself

# Print the result of variable selfcheck to check that this code is working
selfcheck


# Logical T/F test for recipient & SO, return a TRUE result for disallowed matches
testso <- c(r == so)

# Check the vector testso for TRUE (dissallowed matches); Save result as a new variable to use later (socheck)
socheck <- TRUE %in% testso

# Print the result of variable testso to check that this code if working
socheck


# Logical T/F test for recipient & frenemy, return a TRUE result for disallowed matches
testfrn <- c(r == f)

# Check the vector testfn for TRUE (disallowed matches); Save result as new variable to use later (frenemycheck)
frenemycheck <- TRUE %in% testfrn

# Print the result of variable TSO to check that this code if working
frenemycheck

# Create a loop to check for disallowed matches
while((selfcheck == TRUE) | (socheck == TRUE) | (frenemycheck == TRUE)) {
  
  # If disallowed matches = TRUE, then rerun the sample & checks until there are no disallowed matches
  drawn = sample(d, 13, replace = FALSE)
  
  
  # Add the sampled participants into the recipient vector
  r = tibble(Recipient = c("Carla", drawn))
  
  # Add the recipient vector to the exchange tibble
  exchange = bind_cols(p, so) %>% 
    bind_cols(f) %>% 
    bind_cols(r)
  ## Use logical checks to look for disallowed matches (recipient == p, so, or f)
  
  
  # Logical T/F test for recipient & Santa, return a TRUE result for disallowed matches
  testself <- c(r == p)
  
  # Check the vector testself for TRUE (disallowed matches); Save result as new variable to use later (selfcheck)
  selfcheck <- TRUE %in% testself
  
  
  # Logical T/F test for recipient & SO, return a TRUE result for disallowed matches
  testso <- c(r == so)
  
  # Check the vector testso for TRUE (dissallowed matches); Save result as a new variable to use later (socheck)
  socheck <- TRUE %in% testso
  
  
  # Logical T/F test for recipient & frenemy, return a TRUE result for disallowed matches
  testfrn <- c(r == f)
  
  # Check the vector testfrn for TRUE (disallowed matches); Save result as new variable to use later (frenemycheck)
  frenemycheck <- TRUE %in% testfrn
}

# After self-correcting the matches, print a quick debugging tibble to look for disallowed matches
DEBUG = bind_cols(p, r) %>% 
  bind_cols(so) %>% 
  bind_cols(f)
DEBUG

# And the nice and clear pair-matching for the company who wanted it
match = bind_cols(p, r)
match


# Add a tibble for a range of gift prices up to $100.00
dlr = tibble(price = seq(from=0, to=100, by=.01))

# Randomly select a price for each Santa to have paid for the gift purchased
pd <- sample_n(dlr, 13, replace = TRUE)

# Chris paid $364.97 for his gift to Carla, so we'll put that info in his paid spot
cp = tibble(price = 364.97)
paid = bind_rows(cp, pd)

### !!!HOLY SHIT I got the freaking bind_rows to work..... Jesus is magic.  Why does it work here and not earlier?

# Add the tibble paid as an add'l column in match
matchpls = bind_cols(match, paid)


# Create a tibble for a range of dates of purchase- let's assume from Dec. 1st- Dec. 5th
dt = tibble(Purchased = as.Date(x=runif(13, min = as.Date("2019-12-01"), max = as.Date("2019-12-05")), origin = "1970-01-01"))

# Chris bough his gift on April 4th 2019, so he gets his own special early bird tibble
cb = tibble(Purchased = as.Date("2019-04-04"))

# Bind the rows of cb and dt to add Chris's date of purchase to the randomized purchase dates.
dpurch = bind_rows(cb, dt)

# Add the date of purchase column to the match tibble
matchpls = bind_cols(matchpls, dpurch)
matchpls