sing_line <- function(data, num, phrase)
{
  num_word <- data[num,2]
  
  intro <- paste("On the", num_word, "day of Christmas, my true love gave to me,", sep = " ")
  
  phrases <- data %>%
    pull({{phrase}})
  
  phrases[1] <- paste0("and ", phrases[1], ".")
  
  gift_lines <- paste0(phrases[num:1], collapse = ",\n")
  gift_lines <- gsub(" ,", ",", gift_lines)
  
  answer <- paste(intro, gift_lines, sep = "\n")
  return(cat(answer))
}