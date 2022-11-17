
# setup -------------------------------------------------------------------
require(pacman)

# https://www.r-bloggers.com/2021/06/extract-text-from-pdf-in-r-and-word-detection/
# 

## loading required pac
p_load(tidyverse, rvest, httr, here, pdftools, tesseract)



# local calculation test --------------------------------------------------

# game meta data
game = '719'
cost = 2.00
odds = 1/4.57
# remaining prizes 
prizes = c(10000,5000,1000,500,100,50,20,10,5,4,2)
amounts = c(4,6,43,156,3031,6024,25360,51297,52170,200025,243051)
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



# PDF automated reading ---------------------------------------------------
## listing all locally downloaded files
prize_files = list.files(
  path = here('projects','scratch-off-probability','data'),
  pattern = '*.pdf',
  full.names = T
)

## running OCR on prizes remaining
pdf_read = pdf_ocr_text(
  pdf = prize_files[1]
)


## running initial formatting
line_storage = c()
for(i in 1:length(pdf_read)){
  ## pulling page of interest
  page = pdf_read[i]
  
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
game_storage = list()
game_counter = 0
for(i in 1:length(line_storage)){
  line = line_storage[i]

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
      game_storage[[game_counter]] = game_list
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
      line_start = i
    )
    
    ## incrementing game counter
    game_counter = game_counter + 1
  }
  
  ## recording prizes and amounts
  if(str_detect(line,'^\\$')){
    ## extracting prize number
    prize = str_extract(line,'(?<=\\$ )[\\d,]+') %>%
      str_remove_all(',') %>%
      as.numeric()
    if(is.na(prize)){
      prize = str_extract(line,'(?<=\\$)[\\d,]+') %>%
        str_remove_all(',') %>%
        as.numeric()
    }
    
    ## extracting remaining number
    amount = str_extract(line,'[\\d,]+$') %>%
      str_remove_all(',') %>%
      as.numeric()
    ## storing in game table
    game_list$prizes = c(game_list$prizes,prize)
    game_list$amounts = c(game_list$amounts,amount)
  }
  
  
}

