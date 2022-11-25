
# setup -------------------------------------------------------------------

# https://www.r-bloggers.com/2021/06/extract-text-from-pdf-in-r-and-word-detection/
# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html
# https://www.ohiolottery.com/Games/ScratchOffs/Prizes-Remaining
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html

## loading required pac
pacman::p_load(tidyverse, rvest, httr, here, pdftools, tesseract, magick, xml2)


# local calculation test --------------------------------------------------

# game meta data
game = '719'
cost = 2.00
odds = 4.57
# remaining prizes 
prizes = c(10000,5000,1000,500,100,50,20,10,5,4,2)
amounts = c(4,6,43,156,3031,6024,25360,51297,52170,200025,243051)

weighted_potential = function(prizes,amounts,odds,cost){
  odds = 1/odds
  prizes_remaining = sum(amounts)
  # calculating remaining losers
  losers_remaining = round(prizes_remaining / odds)
  # appending losers remaining
  total_amounts = c(amounts,losers_remaining)
  total_prizes = c(prizes,0)
  total_remaining = losers_remaining + prizes_remaining
  total_probs = (total_amounts / total_remaining)
  # calculating weighted potential score (monte-carlo 1e06 samples)
  weighted_potential = mean(sample(x = total_prizes,size = 1e06,replace = T,prob = total_probs)) - cost

  return(weighted_potential)
}
weighted_potential(prizes,amounts,odds,cost)

## defining prize formatter
prize_formatter = function(x){
  if(x == 'ENTRY TICKET'){
    return(0)
  }
  
  ## extracting prize number
  prize = str_extract(x,'(?<=\\$ )[\\d,]+') %>%
    str_remove_all(',') %>%
    as.numeric()
  ## handling missing space
  if(is.na(prize)){
    prize = str_extract(x,'(?<=\\$)[\\d,]+') %>%
      str_remove_all(',') %>%
      as.numeric()
  }
  ## handling XK per year for X years
  if(str_detect(x,str_c(prize,'K'))){
    years = str_extract(str_to_upper(x),'\\d{1,2}(?= YRS)') %>%
      as.numeric()
    prize = prize * 1000 * years
  }
  return(prize)
}



# PDF automated reading ---------------------------------------------------
## listing all locally downloaded files
prize_files = list.files(
  path = here('projects','scratch-off-probability','data'),
  pattern = '*.html',
  full.names = T
)
print(prize_files)

## running OCR on prizes remaining
source_file = prize_files[1]

## reading tables
rawHTML = read_html(source_file) 
tables = html_table(rawHTML,header = T)

## formatting table results
game_storage = list()
for(i in 1:length(tables)){
  ## looking at first table
  tmp = tables[[i]]
  
  ## pulling meta information
  game_meta = colnames(tmp)[1]
  game_number = str_extract(game_meta,'^\\(\\d{3}\\)') %>%
    str_remove_all('\\(|\\)')
  game_name = str_extract(game_meta,
                          pattern = '(?<=\\(\\d{3}\\) ).+(?= \\$\\d+)')
  cost = str_extract(game_meta,
                     pattern = '(?<=\\$)\\d+$') %>%
    as.numeric()
  
  ## removing last column, 1st row, setting names, formatting numbers
  tmp = tmp[2:nrow(tmp),1:2] %>%
    set_names(c('PRIZES','AMOUNTS')) %>%
    rowwise() %>%
    mutate(PRIZE = prize_formatter(PRIZES),
           AMOUNT = as.numeric(str_remove_all(AMOUNTS,',')))
  
  ## pulling additional information
  prizes = tmp$PRIZE
  amounts = tmp$AMOUNT
  
  ## starting game level tracking
  game_list = list(
    game_number = game_number,
    game_name = game_name,
    cost = cost,
    prizes = prizes,
    amounts = amounts
  )
  
  game_storage[[game_name]] = game_list
}


## creating summary table
name = c()
cost = c()
number = c()
for(game_name in (names(game_storage))){
  tmp = game_storage[[game_name]]
  name = c(name,tmp$game_name)
  cost = c(cost,tmp$cost)
  number = c(number,tmp$game_number)
}
results = data.frame(
  name = name,
  cost = cost,
  number = number
)
write.csv(results,file = here('projects','scratch-off-probability','data','games.csv'))

## pulling lottery odds
lottery_odds = read.csv(here('projects','scratch-off-probability','data','scratch-off-odds.csv')) %>%
  mutate(number = as.character(number))


## running calculations
weighted_results = results %>%
  left_join(lottery_odds) %>%
  mutate(weighted_potential = -999)
for(game_name in unique(weighted_results$name)){
  # grabbing odds and cost
  odds = weighted_results$odds[weighted_results$name == game_name]
  cost = weighted_results$cost[weighted_results$name == game_name]
  
  # pulling game info
  tmp = game_storage[[game_name]]
  
  # grabbing prizes and amounts
  prizes = tmp$prizes
  amounts = tmp$amounts
  prize_nas = sum(is.na(prizes))
  amount_nas = sum(is.na(amounts))
  if(prize_nas + amount_nas > 0){
    print(game_name)
    print(cost)
    stop('correct that shit')
  }
  
  
  weighted_results$weighted_potential[weighted_results$name == game_name] = weighted_potential(prizes,amounts,odds,cost)
}



