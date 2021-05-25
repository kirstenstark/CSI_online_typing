# load packages
library(tidyr)
library(dplyr)
library(stringr)
library(stringdist)


## 1) Function to delete of last character typed words if those are space or enter
# This function checks whether the last character(s) of the word entries is a space or 
# is the typed word "Enter", and if so, deletes the last/the last five characters. 
# If the last character(s) are neither of both, the word remains unchanged. The 
# function can be used within dplyr's mutate function. 
# Additionally, the function has the option to delete an alternative ending, 
# while keeping " " and "Enter" at the end of the word

delete_ending <- function(word, ending = NA) {
  if(is.na(ending)) {
      # if no custom ending is given, delete " " and "Enter" at the end
      case_when(str_ends(word, " ") ~ str_sub(word, end=str_length(word)-str_length(" ")),
            str_ends(word, "Enter") ~ str_sub(word, end =str_length(word)-str_length("Enter")),
            TRUE ~ word)
  } else{
      # else delete custom ending
      case_when(str_ends(word, ending) ~ str_sub(word, end=str_length(word)-str_length(ending)),
            TRUE ~ word)
  }
}


## 2) Function to replace special keys (e.g. backspace, shift, etc.) 
##    by other characters (e.g. numbers)
# Function takes as entries the word entries, the keys to be changed, and the 
# characters they should be replaced with.

replace_special_chars <- function(input, oldnames, newnames) {
  if(length(oldnames) != length(newnames)){
    print("Your oldname/newname vectors don't have the same length. Please correct!")
    stop()
  }
  for (i in 1:length(input)) {
    for (j in 1:length(oldnames)) {
      # loop through all input values and all special characters and replaces all special characters by new name
      input[i] <- str_replace_all(input[i], pattern = oldnames[j], 
                        replacement = as.character(newnames[j]))
      if( i == 1) {
          print(paste0("The pattern ", oldnames[j], 
                   " has been replaced by the pattern ", 
                   newnames[j], ".", sep = ""))}
    }}
  return(input)
} 


## 3) Function that computes the final words by applying all backspaces
# The function takes as input the word entries and, optionally, the backspace identifier.

replace_backspace <- function(input, backspace = "Backspace") {
  for(i in 1:length(input)) {
    # loop through all word entries and count number of backspaces in the current word
    backspaces <- str_locate_all(input[i], backspace)[[1]]
    for(j in 1:nrow(backspaces)){
      # loop as many times as there are backspaces
      # for the current first backspace, delete backspace and the preceding character
      input[i] <- sub(str_c(".{1}",backspace), "", input[i])
    }
  }
  return(input)
}


## 4) Function that computes the fuzzy string matching
# Calculate stringdistance between (backspace corrected) input word and item/alternative 
# namings, and select the "best match", i.e. the item/alternative with the lowest distance 
# and the first letter being correct.  
# The default method is the Jaro distance (Jaro-Winkler distance ("jw") with p = 0), 
# but other methods, of the stringdist function (van der Loo, 2014) are theoretically 
# possible as well. However, further options of the stringdist function might be necessary 
# to adapt as well.
# alternatives should be a dataframe with at least one column called item (same entry 
# for as many alternatives as there are for a specific item and another column called 
# alternatives). 

calculate_stringdist <- function(word, stims, alternatives =
                                alternatives, method = "jw", p = 0,
                                weight = c(1,1,1,1), q = 1,
                                firstlettercorrect = TRUE) {
 
  # input check
  if(length(word) != length(stims)){
    print("Your typed words and item vectors don't have the 
          same length. Please correct!")
    stop()
  }
  
  # Compute string distance between word entry and item 
  # using given method
  stringdistance <- stringdist(toupper(word), 
                               toupper(stims), method = method,
                               p = p, weight = weight, q = q)
  
  # placeholders that will be filled in the for-loop
  bestmatch <- stims
  dist <- 100 
  
  # convert all entries to upper
  word <- toupper(word)
  stims <- toupper(stims)
  alternatives$item <- toupper(alternatives$item)
  alternatives$alternatives <- toupper(alternatives$alternatives)
  
  # loop through all word entries
  for(i in 1:length(word)){
    # loop only if string distance to item is not already perfect
    # and is distance is not NA (meaning the word is NA)
    if(stringdistance[i] != 0 & !is.na(stringdistance[i])) {
      # curritem <- alternatives %>% filter(item == stims[i]) %>% 
      #   mutate(dist = stringdist(word[i], alternatives,
      #               method = method, p = p, q = q, 
      #               weight = weight)) %>%
      #   filter(substring(word[i],1,1) == 
      #        substring(alternatives,1,1)) %>%
      #   slice(which.min(dist)) 
      # if(nrow(curritem) != 0 &
      #    curritem$dist[1] < stringdistance[i]){
      #   stringdistance[i] <- curritem$dist[1]
      #   bestmatch[i] <- curritem$alternatives[1]
      # }}}

      # filter "alternatives" df for alternatives of current item
      curritem <- alternatives %>% filter(item == stims[i])
      # check whether current alternative column is not empty
      if(nrow(curritem) != 0) {
        for(j in 1:nrow(curritem)) {
          currentalternative <- curritem$alternatives[j]
          dist <- stringdist(word[i], currentalternative,
                    method = method, p = p, q = q, weight = weight)
          # compare the current string distance to the best
          # distance so far
                # for firstlettercorrect = TRUE
          if(dist < stringdistance[i] &
             firstlettercorrect == TRUE  &
             substring(word[i],1,1) ==
             substring(currentalternative,1,1)) {
                  stringdistance[i] <- dist
                  bestmatch[i] <- currentalternative
                  dist <- 100
                  } else if (dist < stringdistance[i]) {
                # for firstlettercorrect = FALSE
                  stringdistance[i] <- dist
                  bestmatch[i] <- currentalternative
                  dist <- 100
                  }
        }}}}
  distancebest <- cbind(stringdistance, bestmatch)
  return(distancebest)
}



## 5) Function that classifies the word entries
# Function that classifies the word entries for correctness and different typing errors.

case_character_type <- function(word, item, wordcorrected,
                                distance, bestmatch, d) {
  case_when(
        
    # correct answers: participants typed exactly the correct word, 
    # with space or enter at the end
     toupper(word) == toupper(item) | 
       toupper(word) == toupper(str_c(item, " ")) |
       toupper(word) == toupper(str_c(item, "Enter")) ~ "correct",
     
    # correctedtocorrect: participants corrected their entry to the correct
    # word using "Backspace"
    (toupper(wordcorrected) == toupper(item) | 
       toupper(wordcorrected) == toupper(str_c(item, " ")) |
       toupper(wordcorrected) == toupper(str_c(item, "Enter"))) &
        substring(wordcorrected,1,1) == substring(word,1,1) &
        substring(word,2, 10) != "Backspace" ~ "correctedtocorrect",
    
    # approx_correct: the approximately correct and best fitting word is the actual item
    # distance limits needs to be set
    (distance < d) & toupper(item) == toupper(bestmatch) & 
      toupper(substring(wordcorrected,1,1)) ==
                  toupper(substring(bestmatch,1,1)) & 
      toupper(substring(word, 1,1)) == 
              toupper(substring(bestmatch,1,1)) & 
      substring(word,2, 10) != "Backspace" ~ "approx_correct",
    
    # alternative: alternative was typed correctly 
      (distance == 0) & (toupper(word) == toupper(wordcorrected) |
        toupper(str_c(word, " ")) == toupper(wordcorrected) |
        toupper(str_c(word, "Enter")) == toupper(wordcorrected))  &
      toupper(substring(word,1,1)) ==
      toupper(substring(bestmatch,1,1)) ~ "alternative",
    
    # alternative_corrected: alternative typed correctly after backspace correction
     (distance == 0) & toupper(word) != toupper(wordcorrected) &
      toupper(str_c(word, " ")) != toupper(wordcorrected) &
      toupper(str_c(word, "Enter")) != toupper(wordcorrected) &
      toupper(substring(word, 1,1)) ==
      toupper(substring(bestmatch,1,1)) &
      substring(word,2, 10) != "Backspace" ~ 
      "alternative_corrected",
    
    # approx_alternative: distance limit needs to be set
    (distance < d) & distance != 0 &
      toupper(substring(wordcorrected,1,1)) ==
      toupper(substring(bestmatch,1,1)) &
      toupper(substring(word, 1,1)) ==
      toupper(substring(bestmatch,1,1)) &
      substring(word,2, 10) != "Backspace" ~ "approx_alternative",
       
    # backspace_space_enter: participants started by typing backspace, space,
    # enter, or capslock
     str_starts(word,"Backspace") |
       str_starts(word," ") |
       str_starts(word,"CapsLock") |
       str_starts(word,"Enter") ~ "backspace_space_enter",
    
    # shift_start: participants started by pressing the shift key
     str_starts(word,"Shift") ~ "shift_start",
    
    # isna: participants didn't enter anything
is.na(word)       ~ "isna",

# distance-based error
(distance >= d) & 
  toupper(substring(wordcorrected,1,1)) ==
  toupper(substring(bestmatch,1,1)) &
  toupper(substring(word, 1,1)) ==
  toupper(substring(bestmatch,1,1)) &
  substring(word,2, 10) != "Backspace" ~ "distance_based_error",

# first letter-based error
(distance < d) & 
  (toupper(substring(wordcorrected,1,1)) !=
     toupper(substring(bestmatch,1,1)) |
     toupper(substring(word, 1,1)) !=
     toupper(substring(bestmatch,1,1)) |
     substring(word,2, 10) == "Backspace") ~ "first_letter_error",

# are all answers classified
TRUE                      ~ "not_correct" )
}
