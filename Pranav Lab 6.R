pluralize_gift <- function(gift)
{
  if (str_detect(gift, "y")) 
  {
    gift <- str_replace(gift, "y", "ies")
  }
  else if (str_detect(gift, "oo")) 
  {
    gift <- str_replace(gift, "oo", "ee")
  }
  else 
  {
    gift <- paste0(gift, "s")
  }
  return(gift)
}


make_phrase <- function(num, num_word, item, verb, adjective, location) 
{
  item <- replace_na(item, "")
  verb <- replace_na(verb, "")
  adjective <- replace_na(adjective, "")
  location <- replace_na(location, "")
  
  vowels = c("a", "e", "i", "o", "u")
  
  if (num > 1) 
  {
    item <- pluralize_gift(item)
    begword <- num_word
  } else 
  {
    if(sum(substring(item, 1, 1) == vowels) == 1) 
    {
      begword <- "an"
    } 
    else 
    {
      begword <- "a"
    }
  }
  
  ans <- paste(begword, adjective, item, verb, location, sep = " ")
  ans <- gsub("\\s+", " ", ans)
  return(ans)
}

xmas <- xmas %>%
  mutate(
    Num.Word = as.character(english::english(Day)),
    Full.Phrase = pmap_chr(list(Day, Num.Word, Gift.Item, Verb, Adjective, Location), make_phrase)
  )
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