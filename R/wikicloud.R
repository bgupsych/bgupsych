#' A wordcloud generator from Wikipedia values
#' @export
#' @import wordcloud2
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rvest
#' @description By typing any Wikipedia value you will get a beautiful wordcloud, with word's frequency as its size. The function is highly modifiable.
#' @details This is a combination of Data mining (using \code{\link{rvest}}), Data cleaning (using the \code{\link{tidyverse}}) and word cloud visualization, using \code{\link{wordcloud2}}.
#' Just type your desired Wikipedia value and run, or if you wish you can modify many things, such as color, frequancy, langauge and more..
#' @param page The name of the page as it appears in Wikipedia. Pay attention to Capita letters and undeline (incase of multiple words in a single value).
#' @param language The language of Wikipedia page you wish. default is set to English ("en"). type the short language symbol \code{"en","fr","es"} and etc..
#' @param bg_color The background color. default is white. a single string with quotes: \code{"white"}.
#' @param word_color The color of the words pressented. This is a un-limited vector in size. Use basic colors in R such as \code{c("red","steelblue","deeppink","brown")} and/or HTML codes such as \code{"#76acdc"}.
#' @param shape Choose your desired cloud shape. \code{circle} (default), \code{cardioid, diamond, triangle, triangle-forward, pentagon, star}.
#' @param min_freq Minimum frequency of the word to display. Default is 1 (Once).
#' @param max_freq Maximum frequency of the word to display. Default is the maximum frequency.
#' @param min_length Minimum word's length to display. Default is 1.
#' @param max_length Maximum word's length to display. Default is 25.
#' @param remove_pattern A pattern to ignore, such as \code{[:punct:]}. It's best to leave untouched.
#' @param remove_words Words to ignore. Function is set to ignore repetitive words such as \code{a, and, the} and etc.. If you do change this one, use free text, such as \code{"just type any words in one sentence and R will seperate them and ignore each"}.
#' @examples #Simpelest way, just plot a word cloud, using default settings:
#' wikicloud("RStudio")
#' @examples
#' wikicloud("RStudio",shape="star",min_freq=2)
#' # Changing the shape of the cloud and filtering words that appear only once.
#'
#' @examples wikicloud("Tag cloud",shape="triangle",bg_color="blue",word_color=c("red","deeppink","#418122"))
#' # This is how the value" Tag cloud" is on Wikipedia's URL.
#'
#' @examples
#' wikicloud("Donald Trump",language="es",word_color="orange")
#' # Changing the language of the Wiki page.
#'
#' @examples wikicloud("donald trump",min_freq=5,max_freq=60)
#' # By selecting the min and max frequency, you can modify the range of frequency. you can set either. both, or none (for full word table, leave untouched).
#' @returns \code{\link{data.frame}} with the unique words and their frequency.
#' @seealso \url{https://yannapps.shinyapps.io/Wikipedia_Word_Cloud/}
#'
wikicloud <- function(page,language="en",bg_color="white",
word_color=c("#76acdc","#4c4c4c"),shape="circle",
min_freq=0,max_freq=NULL,min_length=1,max_length=25,
remove_pattern="[:punct:]",remove_words=NULL,more_content){

  # Following packages are required:
  require("wordcloud2") # the cloud
  require("rvest") # data mining
  require("magrittr") # using the pipe
  require("stringr") # clean text
  require("dplyr") # filter results
  require("tidyr") # show as tibble

  # Transform page to valid syntax for rvest::
page <- page %>% str_replace_all(" ","_") %>% str_to_title()

  # IF none mentioned, remove all these words:
  fr.es_remove=c("su fue una como o mas al Más ha para no sus elle los À ne À si e ser uso sin ce qui'il Apr Ã s comme cette a han ya e esto estan D'un este esta  ellas ellos lo son avec aux se por Ã y el il par sur sa ses qui que en es un con las del de la et le les en des a du dans sont ou est pour un au une pas ont")
  en_remove=c("he will who more have because into can then so do about what been him but may being this its had has which there these such than due were their also is not or are if it be the an at from as after who they had by for that with were the of and in to a his on was")
  he_remove=c(" הוא של על את עם אם כי וגם גם אולי או ידי בין היא לאחר אחרי בגלל כדי")
  remove_words_null = str_to_title(unlist(str_split(c(fr.es_remove,en_remove,he_remove),pattern=" ")))
  if(is.null(remove_words)){
    remove_words=remove_words_null
  } else {remove_words=c(str_to_title(unlist(str_split(remove_words,pattern=" "))),
                         remove_words_null )}
  # ELSE, remove the words mentioned.
  # Data mining from wikipedia
  # Take all content from wiki page
  node="#bodyContent"
  if(!more_content){
    node="p"
  }
  web_link <- paste(c("https://"),language,c(".wikipedia.org/wiki/"),page,sep = "")

  raw_text=rvest::read_html(web_link) %>% rvest::html_nodes(node) %>%
    rvest::html_text()
  # Data cleanining
  clean.text <- str_to_title(str_remove_all(
    unlist(str_split(raw_text,pattern = " ")),
    pattern = remove_pattern))
  #
  words.freq <- data.frame(words=clean.text,Frequency=1,
                           Length=str_length(clean.text)) %>%
    group_by(words) %>% summarise(Frequency=sum(Frequency),
                                  Length=mean(Length)) %>%
    arrange(desc(Frequency)) %>% filter(!words %in%remove_words,
                                        Length>=min_length & Length<=max_length)
  if(is.null(max_freq)){max_freq=max(words.freq$Frequency)}
  else {max_freq=max_freq}
  # Word cloud
  word_col <- sample(word_color,nrow(words.freq[words.freq$Frequency>=min_freq& words.freq$Frequency<=max_freq,]),T)
  cloud=wordcloud2::wordcloud2(words.freq[words.freq$Frequency>=min_freq& words.freq$Frequency<=max_freq,],
                               color = word_col,backgroundColor = bg_color,shape=shape)
  # Print cloud
  print(cloud)
  return(words.freq[words.freq$Frequency>=min_freq & words.freq$Frequency<=max_freq,])
}
