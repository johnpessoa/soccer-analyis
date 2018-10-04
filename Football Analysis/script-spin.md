#' # Is the Premier League the most competitive league in the world? 
#' ### Let's take a look at the spi matches data to analyze the quality of the english teams

#+ packages_and_data, include = TRUE
library(tidyverse)
library(ggrepel)
library(devtools)
source("~/Desktop/token.R")
library(expappr)

spi_matches <- read.csv("~/Downloads/soccer-spi 2/spi_matches.csv")
spi_global_rankings <-  read.csv("~/Downloads/soccer-spi 2/spi_global_rankings.csv")
spi_global_rankings_int <- read.csv("~/Downloads/soccer-spi 2/spi_global_rankings_intl.csv")

#' Let's take a look at the data that I have from FiveThirtyEight and see what I'll need to do to clean it up

#+ initial table, include = TRUE
spi_matches_2 <- spi_matches %>% 
  mutate(match_id = row_number())

spi_matches_2 %>% 
  head() %>% 
  knitr::kable()