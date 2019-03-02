## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(comment = NA, warning = FALSE, message = FALSE)

## ------------------------------------------------------------------------
setwd("/Users/roberthazell/Desktop/DataScience@SMU/DoingDataScience/CaseStudy1")
beers <- read.csv("beers.csv")
brewers <- read.csv("breweries.csv")

## ------------------------------------------------------------------------
length(brewers$State)

## ------------------------------------------------------------------------
Brewtot <- merge(beers, brewers, by.x = "Brewery_id", by.y = "Brew_ID")

## ------------------------------------------------------------------------
nrow(Brewtot) # number of rows
head(Brewtot) # first six observations

## ------------------------------------------------------------------------
colnames(Brewtot)[c(2,8)] = c("Beer", "Brewer")

## ------------------------------------------------------------------------
Brewtot_Missing <- sapply(Brewtot, function(x) sum(is.na(x)))
Brewtot_Missing

## ------------------------------------------------------------------------
library(dplyr)
library(kableExtra)

beer_landscape <- Brewtot %>% group_by(State) %>% summarise(Beers = length(Beer)) %>% cbind(brewers %>% group_by(State) %>% summarise(`Total Brewers` = length(State)) %>% select(`Total Brewers`)) %>% arrange(desc(`Total Brewers`)) %>% data.frame()
beer_landscape$State <- trimws(beer_landscape$State)

str(beer_landscape)

kable(beer_landscape, align = rep('c',3)) %>% kable_styling(bootstrap_options = "striped", full_width = F)

## ------------------------------------------------------------------------
hist(Brewtot$ABV, main="Distribution of ABV", breaks=20, xlab="Alcohol by Volume", border="black", col="steel blue", xlim=c(.02,.13)) 

hist(Brewtot$IBU, main="Distribution of IBU", breaks=20, xlab="International Bitterness Units", border="black", col="dark red", xlim=c(0,100))

## ------------------------------------------------------------------------
alcohol_and_bitterness <- Brewtot %>% group_by(State) %>% 
  summarise(MedianAlcohol = median(ABV, na.rm = T), MedianBitter = median(IBU, na.rm = T))

## ------------------------------------------------------------------------
alcohol_and_bitterness <- alcohol_and_bitterness[-42,]

## ------------------------------------------------------------------------
library(ggplot2)

ggplot(alcohol_and_bitterness) + 
  geom_col(aes(x = reorder(State, MedianAlcohol), y = MedianAlcohol), fill = 'steel blue') + coord_flip() + ylab("Median ABV") + xlab("State") + ggtitle("Median Alcohol Volume by State") + theme(plot.title = element_text(hjust = 0.5))

ggplot(alcohol_and_bitterness) + 
  geom_col(aes(x = reorder(State, MedianBitter), y = MedianBitter), fill = 'orange') + coord_flip() + ylab("Median IBU") + xlab("State") + ggtitle("Median Beer Bitterness by State") +  theme(plot.title = element_text(hjust = 0.5))

## ------------------------------------------------------------------------
northeast = c('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY', 'NJ', 'PA')
midwest = c('ND', 'SD', 'NE', 'KS', 'MN', 'IA', 'MO', 'WI', 'IL', 'MI', 'IN', 'OH')
south = c('DE', 'MD', 'VA', 'WV', 'KY', 'NC', 'SC', 'TN', 'GA', 'FL', 'AL', 'MS', 'AR', 'LA', 'TX', 'OK')
west = c('WA', 'OR', 'ID', 'MT', 'WY', 'CA', 'NV', 'UT', 'AZ', 'CO', 'NM', 'AK', 'HI')

## ------------------------------------------------------------------------
str(Brewtot)

## ------------------------------------------------------------------------

Brewtot$State <- trimws(Brewtot$State, which = "left")
Brewtot = Brewtot %>% mutate(Region = ifelse(State %in% northeast, 'Northeast',
                                          ifelse(State %in% midwest, 'Midwest',
                                          ifelse(State %in% south, 'South', 'West')))
                    )

## ------------------------------------------------------------------------
set.seed(101)
Brewtot %>% select(c(Beer, Brewer, State, Region)) %>% 
  sample_n(10) %>% kable(align = rep('c',4)) %>% kable_styling(bootstrap_options = "striped", full_width = F)

## ------------------------------------------------------------------------
alcohol_and_bitterness_regionalized <- Brewtot %>% select(c(Beer, Brewer, State, Region, ABV, IBU)) %>% 
  group_by(Region) %>% summarise(MedianAlcohol = median(ABV, na.rm = T), MedianBitter = median(IBU, na.rm = T))

alcohol_and_bitterness_regionalized$Region <- as.factor(alcohol_and_bitterness_regionalized$Region)

ggplot(alcohol_and_bitterness_regionalized) + geom_col(aes(Region, MedianBitter, fill = Region)) + ggtitle("Median Bitterness of Beer by Region") + ylab("Median Bitterness") + theme(plot.title = element_text(hjust = 0.5))

ggplot(alcohol_and_bitterness_regionalized) + geom_col(aes(Region, MedianAlcohol, fill = Region)) + ggtitle("Median ABV of Beer by Region") + ylab("Median Alcohol Content") + theme(plot.title = element_text(hjust = 0.5))

## ------------------------------------------------------------------------
ggplot(data=Brewtot, aes(x=IBU, y=ABV)) +geom_point(shape = 16, size = 2, color="blue") + stat_smooth(method = 'lm', color='red') + labs(title = 'Craft Beer IBU by ABV') + theme(plot.title = element_text(hjust = 0.5))

## ------------------------------------------------------------------------
cor.test( ~ ABV + IBU,data=Brewtot,method = "pearson")

## ----echo = TRUE---------------------------------------------------------
library(rvest)
library(stringr)

# scrape consumption info
scraping_beer <- read_html("https://www.usatoday.com/story/money/personalfinance/2018/05/02/which-states-residents-drink-most-beer/569430002/")
national_consum <- scraping_beer %>% html_nodes("ul") %>% html_text() %>% data.frame()
colnames(national_consum)[1] <- "Total Consumption"
# remove first four rows (irrelevant info)
# use drop = FALSE since there's only 1 column
national_consum <- national_consum[-c(1:4), , drop = FALSE]
# change column from factor to string (character)
national_consum$`Total Consumption` <- as.character(national_consum$`Total Consumption`)

beer_values = c() # holds the actual consumption values
for (row in national_consum){
  total_consum <- substring(row, first = str_locate(row, "Total")[1], last = str_locate(row, "5 yr.")[1] - 1)
  total_consum_numeric <- regmatches(total_consum, regexpr('\\d+\\.+\\d', total_consum)) %>% as.numeric()
  beer_values <- c(beer_values, total_consum_numeric)
}

# make the beer values the column values of nationa_consum
national_consum$`Total Consumption` <- beer_values
# remove row names bc of improper numbering
rownames(national_consum) <- NULL


# scrape the name of the states
statenames <- scraping_beer %>% html_nodes("p") %>% html_text() %>% data.frame()
# remove row names
colnames(statenames)[1] <- 'State'
statenames$State <- as.character(statenames$State)
rownames(statenames) <- NULL
# immediately delete irrelevant first 22 and last 4 rows
statenames <- statenames[-c(1:22, 88:91), 1, drop = FALSE]
# first isolate rows similar in format to "46. New York"
statenames <- statenames[grepl("\\d+\\.", statenames$State), , drop = FALSE]
# remove list numbers and "(tied)"
statenames$State <- str_replace_all(statenames$State, "\\d+\\.|\\(+tied+\\)", "")
# remove spaces before or after name
statenames$State <- str_replace_all(statenames$State, "^\\s|\\s$", "")
rownames(statenames) <- NULL
# convert to abbreviations using state.name and state.abb built-ins
for (i in 1:nrow(statenames)) {
  statenames$State[i] <- state.abb[grep(statenames$State[i], state.name)]
}
# merge statenames and national_consum to a unified dataset
national_consum <- cbind(national_consum, statenames)
#merge national_consum and beer_landscape for the total dataset
complete_beer_landscape <- merge(national_consum, beer_landscape, by = 'State') %>%
  arrange(desc(`Total Consumption`))
colnames(complete_beer_landscape)[4] <- "Total Brewers"
# format table
kable(complete_beer_landscape, align = rep('c',4)) %>% kable_styling(bootstrap_options = "striped", full_width = F)

## ------------------------------------------------------------------------
ggplot(complete_beer_landscape) + geom_point(aes(`Total Brewers`, `Total Consumption`), col = 'purple' )+ggtitle("Comparing Beer Consumption Levels to Number of Breweries") + xlab("Total Brewers") + ylab("Total Consumption (Gallons)") + theme(plot.title = element_text(hjust = 0.5))

## ------------------------------------------------------------------------
complete_beer_landscape[which(complete_beer_landscape$`Total Brewers` > 40), ]

