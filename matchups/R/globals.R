library(dplyr, quietly=TRUE)
library(ggplot2, quietly=TRUE)
library(gridExtra, quietly=TRUE)
library(verification, quietly=TRUE)
library(DescTools, quitely=TRUE)

# The years to evaluate
START_YEAR = 2010
END_YEAR = 2019

# Constraint on which players to include
MIN_PA = 502

# Event codes in play by play data
EVENT_OUT = 2:3                      # Generic Out, Strikeout
EVENT_HIT = 20:23                    # 1B/2B/3B/HR
EVENT_WALK = c(14, 16)               # Walk, HBP
EVENT_K = 3                          # Strikeout
EVENT_OTHER = c(15, 17, 18, 19)      # IBB, Interference, Error, FC
EVENT_NON_PA = c(4, 6, 8, 9, 10, 12) # SB, CS, PO, WP, Passed Ball, Other

# Events that correspond to a plate apperance
EVENT_PA = unique(c(EVENT_OUT, EVENT_HIT, EVENT_WALK, EVENT_K, EVENT_OTHER))