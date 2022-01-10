#' A wordcloud generator from Wikipedia values
#' @export
#' @import wordcloud2
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rvest
#' @description By typing any Wikipedia value you will get a beautiful wordcloud, with word's frequency as its size. The function is highly modifiable.
#' @details This is a combination of Data mining (using \code{\link{rvest}}), data cleaning (using multiple packages, but mostly \code{\link{stringr}}) and a beautiful string visualization, using \code{\link{wordcloud2}}.
#' You dont need to change anything to make it work, and if you wish you can modify many things, from the color of the words and background, to the shape of the cloud, minimal frequency, language of Wiki value and more!
#' @param page The name of the page as it appears in Wikipedia. Pay attention to Capita letters and undeline (incase of multiple words in a single value).
#' @param language The language of Wikipedia page you wish. default is set to English ("en"). type the short language symbol \code{"en","fr","es"} and etc..
#' @param bg_color The background color. default is white. a single string with quotes: \code{"white"}.
#' @param word_color The color of the words pressented. This is a un-limited vector in size. Use basic colors in R such as \code{c("red","steelblue","deeppink","brown")} and/or HTML codes such as \code{"#76acdc"}.
#' @param shape Choose your desired cloud shape. \code{ ‘circle’ (default), ‘cardioid’, ‘diamond’ (alias of square), ‘triangle-forward’, ‘triangle’, ‘pentagon’, and ‘star’}.
#' @param min_freq Minimum frequency of the word in order to appear in the cloud and table. Default is 1 (that is, every word).
#' @param remove_pattern A pattern to ignore, such as \code{.?:;-}. Its best to leace untouched.
#' @param remove_words Words to ignore. If left as NULL, R will automatically ignore non-meaningful words in English, Spanish and French. It is advised to leave untouchd. If you do change this one, use free text, such as \code{"just type any words in one sentence and R will seperate them and ignore each"}.
#' @examples wikicloud("RStudio")
#' # Most simple way, with default settings.
#' @examples wikicloud("RStudio",shape="star",min_freq=2)
#' # Changing the shape of the cloud and filtering words that appear only once.
#' @examples wikicloud("Tag_cloud",shape="triangle",bg_color="blue",word_color=c("red","deeppink","#418122"))
#' # This is how the value" Tag cloud" is on Wikipedia's URL.
#' @examples
#' wikicloud("Trump",language="es",word_color="orange")
#' # Changing the language of the Wiki page.
#' ## You might want to remove words yourself in this one, by modifying the 'remove_words' argument.
#' @note It is advised to simply install \code{\link{tidyverse}}. It includes many useful packages.
#' @returns \code{\link{data.frame}} with the unique words and their frequency.

wikicloud <- function(page,language="en",bg_color="white",
word_color=c("#76acdc","#4c4c4c"),shape="circle",min_freq=0,
remove_pattern="[.?:;,-]",remove_words=NULL){
  # Following packages are required:
  require("wordcloud2") # the cloud
  require("rvest") # data mining
  require("magrittr") # using the pipe
  require("stringr") # clean text
  require("dplyr") # filter results
  require("tidyr") # show as tibble

  # IF none mentioned, remove all these words:
  if(is.null(remove_words)){
    fr.es_remove=c("los a lo son aux se por À y el que en es un con las del de la et le les en des a du dans sont ou est pour un au une pas ont")
    en_remove=c("he will more this its has which were also is not or are if it be the an at from as by for that with were the of and in to a his on was")
    remove_words = str_to_title(unlist(str_split(c(fr.es_remove,en_remove),pattern=" ")))
  } else {remove_words=str_to_title(unlist(str_split(remove_words,pattern=" ")))}
  # ELSE, remove the words mentioned.
  web_link <- paste(c("https://"),language,c(".wikipedia.org/wiki/"),page,sep = "")
  ##
  raw_text=rvest::read_html(web_link) %>% rvest::html_nodes("p") %>%
    rvest::html_text()
  clean.text <- str_to_title(str_remove_all(
    unlist(str_split(raw_text,pattern = " ")),
    pattern = remove_pattern))
  words.freq <- data.frame(words=clean.text,freq=1) %>%
    group_by(words) %>% summarise(f=sum(freq)) %>%
    arrange(desc(f)) %>% filter(!words %in%remove_words)
  ##
  word_col <- sample(word_color,nrow(words.freq[words.freq$f>=min_freq,]),T)
  cloud=wordcloud2::wordcloud2(words.freq[words.freq$f>=min_freq,],
                               color = word_col,backgroundColor = bg_color,shape=shape)
  ##
  print(cloud)
  return(words.freq[words.freq$f>=min_freq,])
}
