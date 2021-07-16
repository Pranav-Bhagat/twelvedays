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