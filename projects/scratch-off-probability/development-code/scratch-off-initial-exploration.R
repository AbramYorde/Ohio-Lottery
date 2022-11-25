
# setup -------------------------------------------------------------------
require(pacman)

# https://www.r-bloggers.com/2021/06/extract-text-from-pdf-in-r-and-word-detection/
# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html
# https://www.ohiolottery.com/Games/ScratchOffs/Prizes-Remaining

## loading required pac
p_load(tidyverse, rvest, httr, here, pdftools, tesseract)


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
  # calculating weighted potential score
  weighted_potential = sum((total_prizes - cost) * total_amounts)/total_remaining
  
  return(weighted_potential)
}
weighted_potential(prizes,amounts,odds,cost)



# PDF automated reading ---------------------------------------------------
## listing all locally downloaded files
prize_files = list.files(
  path = here('projects','scratch-off-probability','data'),
  pattern = '*.pdf',
  full.names = T
)

## running OCR on prizes remaining
pdf_text = pdf_ocr_text(
  pdf = prize_files[2],
  dpi = 300
)

## running initial formatting
line_storage = c()
for(i in 1:length(pdf_text)){
  ## pulling page of interest
  page = pdf_text[i]
  
  ## pulling game numbers on page
  game_number_locations = str_locate_all(
    string = page,
    pattern = '\\(\\d{3}\\)'
  ) %>% 
    as_vector()
  
  ## skipping page if no game numbers are listed
  if(length(game_number_locations) == 0){
    next
  }
  
  ## checking where to remove data if first page
  if(i == 1){
    ## stripping initial information
    page_clean = str_sub(
      string = page,
      start = game_number_locations[1]
    )
  }else{
    page_clean = page
  }
  
  ## splitting string at each line break
  page_matrix = str_split(
    string = page_clean,
    pattern = '\n'
  )[[1]]
  
  ## saving to storage
  line_storage = c(line_storage,page_matrix)
}

## starting to pull information
valid_costs = c(2,5,10)
game_storage = list()
kill_list = c()
cost_kill_list = c()
game_counter = 0
for(i in 1:length(line_storage)){
  ## initial line cleaning
  line = line_storage[i]
  line = str_remove_all(line,'\\]')

  ## skipping empty lines
  if(line == ''){
    next
  }
  ## skipping Prize line
  if(str_detect(line,'PRIZES')){
    next
  }
  
  ## checking for a game identifier
  if(str_detect(line,'^\\(\\d{3}\\)')){
    ## storing previous game information
    if(game_counter >= 1){
      game_list$line_end = i
      game_storage[[game_name]] = game_list
    }
    
    ## parsing string for relevant information
    game_number = str_extract(line,'^\\(\\d{3}\\)') %>%
      str_remove_all('\\(|\\)')
    game_name = str_extract(line,
                            pattern = '(?<=\\(\\d{3}\\) ).+(?= \\$\\d+)')
    cost = str_extract(line,
                       pattern = '(?<=\\$)\\d+$') %>%
      as.numeric()
    
    ## starting game level tracking
    game_list = list(
      game_number = game_number,
      game_name = game_name,
      cost = cost,
      prizes = c(),
      amounts = c(),
      lines = c(),
      line_start = i
    )
    
    ## incrementing game counter
    game_counter = game_counter + 1
    
    ## checking if valid cost
    if(!cost %in% valid_costs){
      cost_kill_list = c(cost_kill_list,game_counter)
    }
  }
  
  ## recording prizes and amounts
  if(str_detect(line,'^\\$')){
    ## extracting prize number
    prize = str_extract(line,'(?<=\\$ )[\\d,]+') %>%
      str_remove_all(',') %>%
      as.numeric()
    ## handling missing space
    if(is.na(prize)){
      prize = str_extract(line,'(?<=\\$)[\\d,]+') %>%
        str_remove_all(',') %>%
        as.numeric()
    }
    ## handling XK per year for X years
    if(str_detect(line,str_c(prize,'K'))){
      years = str_extract(str_to_upper(line),'\\d{1,2}(?=YRS)') %>%
        as.numeric()
      prize = prize * 1000 * years
    }
    
    ## extracting remaining number
    amount = str_extract(line,'[\\d,]+$') %>%
      str_remove_all(',') %>%
      as.numeric()
    ## handling misread prize numbers
    if(is.na(amount)){
      message(str_c('issue on game ',game_counter,'\nadding to kill list'))
      kill_list = c(kill_list,game_counter)
    }
    ## storing in game table
    game_list$prizes = c(game_list$prizes,prize)
    game_list$amounts = c(game_list$amounts,amount)
    game_list$lines = c(game_list$lines,i)
  }
}
## storing final game
game_storage[[game_name]] = game_list
keep_list = seq(1,length(game_storage))
game_storage_clean = game_storage[keep_list[!keep_list %in% cost_kill_list]]

# ## removing games with errors
# final_kill_list = unique(kill_list)
# game_storage_clean = game_storage[keep_list[!keep_list %in% final_kill_list]]

name = c()
cost = c()
number = c()
for(game_name in (names(game_storage_clean))){
  tmp = game_storage_clean[[game_name]]
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
  tmp = game_storage_clean[[game_name]]
  
  # grabbing prizes and amounts
  prizes = tmp$prizes
  amounts = tmp$amounts
  prize_nas = sum(is.na(prizes))
  amount_nas = sum(is.na(amounts))
  # if(prize_nas + amount_nas > 0){
  #   print(game_name)
  #   print(cost)
  #   stop('correct that shit')
  #   amounts[11] = 50417
  #   amounts = c(6,amounts)
  #   amounts = c(2315,76721,394317)
  #   amounts
  # 
  #   prizes[2] = 20000
  #   prizes = c(150000,prizes)
  #   prizes = c(500,100,50)
  #   prizes
  # 
  #   game_storage_clean[[game_name]]$prizes = prizes
  #   game_storage_clean[[game_name]]$amounts = amounts
  # }
  
  
  weighted_results$weighted_potential[weighted_results$name == game_name] = weighted_potential(prizes,amounts,odds,cost)
}



