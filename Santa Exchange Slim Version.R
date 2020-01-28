#Load Tidyverse
library(tidyverse)

# Create a tibble of all participants (p) in the gift exchange; These will be the "Santas"
participants = tibble(Santa = c("Chris", "Carla", "Jim", "Pam", "Leslie", "Ben", "Mitch",
                     "Cameron", "Louis", "Jessica", "Dwight", "Jeremy", "Ray", "Andy"))


# Create a tibble (so) for the partner of each participant, which they cannot be matched with
sos = tibble(SO = c(NA, "Chris", "Pam", "Jim", "Ben", "Leslie", "Cameron", 
                   "Mitch", "Jessica", "Louis", NA, NA, NA, NA))


# Create a tibble (f) for the frenemies of each participant which they cannot be matched with
frenemies = tibble(Frenemy = c(NA, NA, "Dwight", NA, "Jeremy", NA, NA, 
                       NA, "Ray", NA, "Jim", "Leslie", "Louis", NA))


# Filter Carla out of the random draw, as she is a "fixed" set with Chris
draw_from = filter(participants, Santa != "Carla")
dont_draw = tibble(Santa = c("Carla"))


# Randomly draw names of participants to be matched as recipients, and add Carla back to the top to match w/ Chris
# Rename this column "Recipients" before we add all columns to a single tibble
recipients = sample_n(draw_from, 13, replace = FALSE)
recipients = bind_rows(dont_draw, recipients)
colnames(recipients) <- ("Recipients")


# Create the exchange tibble, combining the Santas (p), SOs (so), Frenemies (f), and Recipients (r)
exchange = bind_cols(participants, sos) %>% 
  bind_cols(frenemies) %>% 
  bind_cols(recipients)


### Use logical checks to look for disallowed matches (ie recipient == participants, sos, or frenemies)
# Setup the break test
i <- 1

while (i == 1) {
  # Logical T/F test for santa = self, santa = so, santa = frenemy
  testself <- c(recipients == participants)
  self_result = (TRUE %in% testself)
  testso <- c(recipients == sos)
  so_result = (TRUE %in% testso)
  testfrn <- c(recipients == frenemies)
  frn_result = (TRUE %in% testfrn)
  # If TRUE exists in the test results, there are disallowed matches and we must redraw
  if ((TRUE %in% self_result) | (TRUE %in% so_result) | (TRUE %in% frn_result)) {
    recipients = sample_n(draw_from, 13, replace = FALSE)
    recipients = bind_rows(dont_draw, recipients)
    colnames(recipients) <- ("Recipients")
  # If TRUE is not present in the tibble, all matchs are allowed
  } else {
    i <- 0
  }
}

# Create a tibble with test info to allow us to check the matches and test results
dbug = exchange %>% 
  mutate(testself)%>% 
  mutate(testso) %>% 
  mutate(testfrn)


# And the nice and clear pair-matching for the company who wanted it
match = bind_cols(participants, recipients)
match


# Randomly select a price for each Santa to have paid for the gift purchased
# With prices from $1.00 to $100.00.  Chris paid $364.97 for his gift to Carla
# then add Chris's purchase price back to the top of the list
price = tibble(price = round(runif(13, min=0, max=100), digits=2))
chris_price = tibble(price = 364.97)
paid = bind_rows(chris_price, price)


# Create a tibble for a range of dates of purchase- let's assume from Dec. 1st- Dec. 5th
# Chris bought his gift on April 1st 2019, so he gets his own special early bird tibble
date_bought = tibble(Purchased = as.Date(x=runif(13, min = as.Date("2019-12-01"), 
            max = as.Date("2019-12-05")), origin = "1970-01-01"))

chris_date = tibble(Purchased = as.Date("2019-04-01"))


# Bind the rows of cb and dt to add Chris's date of purchase to the randomized purchase dates.
date_purchased = bind_rows(chris_date, date_bought)

# Add the purchase price and date of purchase columns to the match tibble
matchplus = bind_cols(match, paid) %>% 
  bind_cols(date_purchased)
options(pillar.sigfig = 5)
