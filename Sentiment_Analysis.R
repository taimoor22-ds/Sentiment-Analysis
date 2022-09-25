# Calling Libraries

library(dplyr) # used for data manipulation such as select, mutate etc
library(tidyr) # Making data tidy for analysis and visualization
library(stringr) # similar as tidyr and plays a big role in data cleaning
library(tidytext) # helps in text mining for tidy data 
suppressWarnings(library(gutenbergr)) # R built in library for accessing and processing the public work
library(ggplot2) # used for visualization
suppressWarnings(library(ggthemes)) 
library(ggraph)
library(reshape2)


# Setting & Getting working directory

setwd('C:/Users/Admin/Desktop/Labs/MA331_R/Finalwork')
getwd()

# Reading CSV of books and loading it into the object

Childbook <- read.csv(file = "498_Rebecca of Sunnybrook Farm.csv", header = TRUE)
head(Childbook)

Adultbook <- read.csv(file = "1400_Great Expectations.csv", header = TRUE)
head(Adultbook)

# Getting stop words

data("stop_words") #Getting stop words in tidy format
stop_words

# Un-nest is the function for converting words in a row to single word in a row in lower case by default for tidying the data and it makes the analysis easier to perform

Childbook_Tidy <- Childbook %>% unnest_tokens(word, text)# Breaking data into individual token sets
anti_join(stop_words) # removal of stop words here
head(Childbook_Tidy)
sum(is.na(Childbook_Tidy)) # Counting NA values in the tidy DF

Adultbook_Tidy <- Adultbook %>% unnest_tokens(word, text) 
anti_join(stop_words)# removal of stop words here
head(Adultbook_Tidy) # Head function Prints top rows
sum(is.na(Adultbook_Tidy)) # Counting NA values in the tidy DF

# counting the number of occurrences of words in the book
Childbook_Tidy %>% count(word, sort = TRUE) 
Adultbook_Tidy %>% count(word, sort = TRUE)

# Creating Visualization of common words in the book

Childbook_Tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 1500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


Adultbook_Tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 2800) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# Adding custom words to be removed because of huge repetition numbers and are common

Myown_stopwords <- bind_rows(tibble(word = c("to", "and", "the", "of", "that"),  # add your own extra stop words
                                      lexicon = c("custom")), 
                               stop_words)
Myown_stopwords

# Removing my own stop words from both the books

Childbook_Tidy <- Childbook_Tidy %>% 
  anti_join(Myown_stopwords) # removal of custom stop words 
Childbook_Tidy %>%count(word, sort = TRUE)  # count most common words


Adultbook_Tidy <- Adultbook_Tidy %>% 
  anti_join(Myown_stopwords) # removal of custom stop words 
Adultbook_Tidy %>%count(word, sort = TRUE)  # count most common words

#Creating visualization of most common words after removing custom stop words

Childbook_Tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


Adultbook_Tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


#removing numbers/digits from both the books

Childbook_Tidy <- Childbook_Tidy %>% 
  filter(!grepl('[0-9]', word))    # grep is regular expression for matching

Adultbook_Tidy <- Adultbook_Tidy %>%
  filter(!grepl('[0-9]', word))    # grep is regular expression for matching


# Calculating frequency for each word for childbook and Adultbook

GTBook <- gutenberg_download(c(11, 55, 113, 120, 236, 271, 289, 1448, 1874, 15, 84, 105, 158, 205, 432, 766, 1260, 6688))
GTBook_tidy <- GTBook %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  anti_join(Myown_stopwords)
GTBook_tidy


GTBook$gutenberg_id

library(tidytext)
frequency_calculation<- bind_rows(mutate(Childbook_Tidy, author = "Wiggin, Kate Douglas Smith"), 
                                  mutate(Adultbook_Tidy, author = "Charles Dickens"),
                                  mutate(GTBook_tidy, author = "Gutenbergr book")) %>% 
                             mutate(word = str_extract(word, "[a-z']+")) %>% # [a-z] expression for matching the letters between a to z
                             count(author, word) %>%
                             group_by(author) %>% 
                             mutate(proportion = n / sum(n)) %>%
                             select(-n) %>% 
                             pivot_wider(names_from = author, values_from = proportion) %>%
                             pivot_longer(`Wiggin, Kate Douglas Smith`:`Charles Dickens`,
                                          names_to = "author", values_to = "proportion")
  
frequency_calculation

# Finding the chapter numbers in adult and child book

Adultchapters <- Adultbook %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()
Adultchapters

Childchapters <- Childbook %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^[I.  WE ARE SEVEN	     II.  REBECCAS RELATIONS	    III.  A DIFFERENCE IN HEARTS	     IV.  REBECCA'S POINT OF VIEW	      V.  WISDOM'S WAYS	     VI.  SUNSHINE IN A SHADY PLACE	    VII.  RIVERBORO SECRETS	   VIII.  COLOR OF ROSE	     IX.  ASHES OF ROSES	      X.  RAINBOW BRIDGES	     XI.  THE STIRRING OF THE POWERS	    XII.  SEE THE PALE MARTYR	   XIII.  SNOW-WHITE; ROSE-RED	    XIV.  MR. ALADDIN	     XV.  THE BANQUET LAMP	    XVI.  SEASONS OF GROWTH	   XVII.  GRAY DAYS AND GOLD	  XVIII.  REBECCA REPRESENTS THE FAMILY	    XIX.  DEACON ISRAEL'S SUCCESSOR	     XX.  A CHANGE OF HEART	    XXI.  THE SKY LINE WIDENS	   XXII.  CLOVER BLOSSOMS AND SUNFLOWERS	  XXIII.  THE HILL DIFFICULTY	   XXIV.  ALADDIN RUBS HIS LAMP	    XXV.  ROSES OF JOY	   XXVI.  OVER THE TEACUPS	  XXVII.  THE VISION SPLENDID	 XXVIII.  TH INEVITABLE YOKE	   XXIX.  MOTHER AND DAUGHTER	    XXX.  GOOD-BY, SUNNYBROOK!	   XXXI.  AUNT MIRANDA'S APOLOGY] [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()
Childchapters

#Chapter numbers in line for both books

cumsum(str_detect(Adultchapters$text, 
                  regex("^chapter [\\divxlc]",
                        ignore_case = TRUE))) 

cumsum(str_detect(Childchapters$text, 
                  regex("^[     I.  WE ARE SEVEN	     II.  REBECCAS RELATIONS	    III.  A DIFFERENCE IN HEARTS	     IV.  REBECCAS POINT OF VIEW	      V.  WISDOMS WAYS	     VI.  SUNSHINE IN A SHADY PLACE	    VII.  RIVERBORO SECRETS	   VIII.  COLOR OF ROSE	     IX.  ASHES OF ROSES	      X.  RAINBOW BRIDGES	     XI.  THE STIRRING OF THE POWERS	    XII.  SEE THE PALE MARTYR	   XIII.  SNOW-WHITE; ROSE-RED	    XIV.  MR. ALADDIN	     XV.  THE BANQUET LAMP	    XVI.  SEASONS OF GROWTH	   XVII.  GRAY DAYS AND GOLD	  XVIII.  REBECCA REPRESENTS THE FAMILY	    XIX.  DEACON ISRAELS SUCCESSOR	     XX.  A CHANGE OF HEART	    XXI.  THE SKY LINE WIDENS	   XXII.  CLOVER BLOSSOMS AND SUNFLOWERS	  XXIII.  THE HILL DIFFICULTY	   XXIV.  ALADDIN RUBS HIS LAMP	    XXV.  ROSES OF JOY	   XXVI.  OVER THE TEACUPS	  XXVII.  THE VISION SPLENDID	 XXVIII.  TH INEVITABLE YOKE	   XXIX.  MOTHER AND DAUGHTER	    XXX.  GOOD-BY, SUNNYBROOK!	   XXXI.  AUNT MIRANDAS APOLOGY] [\\divxlc]",
                        ignore_case = TRUE))) 

# Chapter numbers in table
table(Adultchapters$chapter)
table(Childchapters$chapter)

 # Tidying and untidy the data of chapters data
Adultbook_Tidy2 <- Adultchapters %>%  # un-nest in order to convert to tidy format 
  unnest_tokens(word, text) %>%
  anti_join(Myown_stopwords) # removal of custom stop words here
Adultbook_Tidy2 %>%count(word, sort = TRUE) # words in desc order


Untidyadultbook <- Adultbook_Tidy2 %>% 
  group_by(chapter, linenumber) %>% 
  summarize(text = str_c(word, collapse = " ")) %>%
  ungroup()

Untidyadultbook


# Finding the chapter headings using chapter detection and through pipes

#Adult book chapters (59 chapters)
str_detect(Adultbook$text,regex("^chapter [\\divxlc]", ignore_case = TRUE)) # Finding word 'Chapter'
Adultbook$text%>%str_subset(regex("^chapter [\\divxlc]",ignore_case = TRUE)) #pipes 
sum(str_detect(Adultbook$text,regex("^chapter [\\divxlc]", ignore_case = TRUE)))

# Child book chapters (31 chapters)
Childbook$text%>%str_subset(regex("^[     I.  WE ARE SEVEN	     II.  REBECCAS RELATIONS	    III.  A DIFFERENCE IN HEARTS	     IV.  REBECCAS POINT OF VIEW	      V.  WISDOMS WAYS	     VI.  SUNSHINE IN A SHADY PLACE	    VII.  RIVERBORO SECRETS	   VIII.  COLOR OF ROSE	     IX.  ASHES OF ROSES	      X.  RAINBOW BRIDGES	     XI.  THE STIRRING OF THE POWERS	    XII.  SEE THE PALE MARTYR	   XIII.  SNOW-WHITE; ROSE-RED	    XIV.  MR. ALADDIN	     XV.  THE BANQUET LAMP	    XVI.  SEASONS OF GROWTH	   XVII.  GRAY DAYS AND GOLD	  XVIII.  REBECCA REPRESENTS THE FAMILY	    XIX.  DEACON ISRAELS SUCCESSOR	     XX.  A CHANGE OF HEART	    XXI.  THE SKY LINE WIDENS	   XXII.  CLOVER BLOSSOMS AND SUNFLOWERS	  XXIII.  THE HILL DIFFICULTY	   XXIV.  ALADDIN RUBS HIS LAMP	    XXV.  ROSES OF JOY	   XXVI.  OVER THE TEACUPS	  XXVII.  THE VISION SPLENDID	 XXVIII.  TH INEVITABLE YOKE	   XXIX.  MOTHER AND DAUGHTER	    XXX.  GOOD-BY, SUNNYBROOK!	   XXXI.  AUNT MIRANDAS APOLOGY] [\\divxlc]",ignore_case = TRUE)) #pipes 

# No of lines per chapter

table(Adultchapters$linenumber, Adultchapters$chapter)

# No of words per chapter
table(Adultbook$text, Adultchapters$chapter)

# Words in each chapter
Adultwordcounts <- Adultbook_Tidy %>%  # how many words are in each chapter
  summarize(words = n())

Adultwordcounts # There are 55,575 words in the book

Childwordcounts <- Childbook_Tidy %>%  # how many words are in each chapter
  summarize(words = n())

Childwordcounts # There are 26,592 words

# working with sentences and sections

tibble(text = Adultbook$text) %>% 
  unnest_tokens(sentence, text, token = "sentences") # There are 15,949 sentences

tibble(text = Adultbook$text) %>% 
  unnest_tokens(line, text, token = "lines") # There are 15,939 lines

tibble(text = Adultbook$text) %>% 
  unnest_tokens(character, text, token = "characters") # There are 761,768 characters

tibble(text = Childbook$text) %>% 
  unnest_tokens(character, text, token = "characters") # There are 318,093 characters

tibble(text = Childbook$text) %>% 
  unnest_tokens(sentence, text, token = "sentences") # There are 8,969 sentences

tibble(text = Childbook$text) %>% 
  unnest_tokens(line, text, token = "lines") # There are 6,612 lines


# Finding common words between 2 books

commonwords <- inner_join(Adultbook_Tidy, Childbook_Tidy, by ="word")
commonwords %>% count(word, sort = TRUE)

# Visualising the most common words

commonwords %>%
  count(word, sort = TRUE) %>%
  filter(n > 20000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) 

# Finding uncommon word that are exclusive to each book

uncommon_words_1 <- anti_join(Adultbook_Tidy,Childbook_Tidy,by="word")
Uncommon_adultbook <-  uncommon_words_1%>%count(word, sort = TRUE)
Uncommon_adultbook # words that are not in child book but in Adult book

uncommon_words_1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 220) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


uncommon_words_2 <- anti_join(Childbook_Tidy,Adultbook_Tidy,by="word")
uncommon_childbook <- uncommon_words_2%>%count(word, sort = TRUE)
uncommon_childbook  # words that are not in the Adult book but in child book

uncommon_words_2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


# afinn sentiment analysis for child book

sentimentalChildwords <- uncommon_childbook %>%inner_join(get_sentiments("afinn"),"word")
sentimentalChildwords
sentimentalChildwords <- sentimentalChildwords %>% mutate(weighted=n*value)
sentimentalChildwords  # Result of sentiment reference
str(sentimentalChildwords) # 157 observations for 4 variables
sentimentofChildwords <- as.numeric(sentimentalChildwords$weighted)
hist(sentimentofChildwords, box(lty = '1373', col = 'red'))  # Plotting of the sentimental child words 
#most of the words are equally distributed between positive and negative sentiment lexicon


# afinn sentiment analysis for adult book

sentimentaladultwords <- Uncommon_adultbook %>% inner_join(get_sentiments("afinn"),"word")
sentimentaladultwords <- sentimentaladultwords %>% mutate(weighted=n*value)
sentimentaladultwords
str(sentimentaladultwords)
sentimentofadultwords <- as.numeric(sentimentaladultwords$weighted)
hist(sentimentofadultwords) # Plotting of the sentimental Adult words
# Most of the words in adult book has negative sentiment as per afinn dictionary

# t test difference of means test for the two distributions 
t.test(sentimentofadultwords, sentimentofChildwords)
# Adult book has more negative sentiments whereas child book has positive sentimental lexicons

library("dgof")  # looking at the culmulative dist
cummulativesentimentaladult <- ecdf(sentimentaladultwords) 
plot(cummulativesentimentaladult,main="Observation",pch = c(10),ylab="",xlab="sentiments") #plotting the value
cummulativesentimentalchild <- ecdf(sentimentalChildwords) #Treasure values
#Combining the plots
lines(cummulativesentimentaladult,col="green",pch = c(10)) 
abline(v=mean(cummulativesentimentalchild), col="red") # vertical line  at Treasure mean 
legend("bottomleft", 
       legend = c("Adult","Child"), 
       col = c("blue","green"), 
       pch = c(10,17), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1)) 

# kolmogorov-smirnov test for a difference in distributions
ks.test(sentimentofChildwords,cummulativesentimentalchild) 
ks.test(sentimentofadultwords,cummulativesentimentaladult) 
# P value for both the ks.test = 1 which shows that the data fit is perfect

# Getting sentiments lexicons using 

nrc_trust <- get_sentiments("nrc") %>%  
  filter(sentiment == "trust")  # Using NRC sentiment
nrc_trust

Adulttrustwords <- Adultbook_Tidy%>%
  inner_join(nrc_trust)   # selecting trust as a sentiment in adult book

Adulttrustwords

Adulttrustwords %>%
  inner_join(nrc_trust) %>%
  count(word, sort = TRUE)  # selecting trust as a sentiment in adult book

#Plotting the matched lexicons received 

Adulttrustwords %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

library(wordcloud) # Creating word cloud of matched lexicons with trust 
Adulttrustwords %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20))

# Applying the NRC to child book
nrc_trust_child <- get_sentiments("nrc") %>% 
  filter(sentiment == "trust")  # selecting trust sentiment lexicon
nrc_trust

childtrustwords <- Childbook_Tidy%>%
  inner_join(nrc_trust_child)   # trust sentiment in child book

childtrustwords

childtrustwords %>%
  inner_join(nrc_trust_child) %>%
  count(word, sort = TRUE)  # most commonly used trust words

# plotting

childtrustwords %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


childtrustwords %>% # creating word cloud for the matched lexicons with trust
  count(word) %>%
  with(wordcloud(word, n, max.words = 20))


#  Using bing sentiment

get_sentiments("bing")

bingwords <- Adultbook_Tidy %>% 
  inner_join(get_sentiments("bing")) %>%
  count(gutenberg_id, index = row_number() %/% 100, sentiment)%>%     # count pos/neg over 100 words
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)
bingwords

ggplot(bingwords, aes(index, sentiment, fill = gutenberg_id )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gutenberg_id , ncol = 2, scales = "free_x")

# comparing 3 sentiments dictionaries

afinn <- Adultbook_Tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = row_number() %/% 100) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  Adultbook_Tidy %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing"),
  Adultbook_Tidy %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = row_number() %/% 100, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn, #binding positive & negative sentiments together for each sentiment lexicon
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Calculating positive and negative words for understanding the difference between NRC & bing


get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment) # Positive 2308 & Negative 3318


get_sentiments("bing") %>% # Positive 2005 & Negative 4781
  count(sentiment) # negative lexicons have higher values in bing as compared to NRC 

#counting bing words

bing_word_counts <- Adultbook_Tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

#plotting the contribution to sentiment lexicons using bing
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#Positive and negative words of both books using bing dictionary

library(reshape2)
Adultbook_Tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 20)

Childbook_Tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 20)


#Working with Bigrams for both the books

childbookbigram <- tibble(text = Childbook$text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)  # creating the pair of two words that are called bigram where 2 words are combined in a row 

childbookbigram


childbookbigram %>%count(bigram, sort = TRUE) # Counting the bigrams to identify stop word pairs

childbookbigram_separated <- childbookbigram %>%
  separate(bigram, c("word1", "word2"), sep = " ") # separating the bigram in 2 cols
childbookbigram_separated

childbookbigram_separated_filter <- childbookbigram_separated %>%
  filter(!word1 %in% stop_words$word) %>%  # removing the stop word 
  filter(!word2 %in% stop_words$word)
childbookbigram_separated_filter

childbookbigramcount <- childbookbigram_separated_filter %>% count(word1, word2, sort = TRUE)
childbookbigramcount # Count of bigrams after removing the common stop words

childbookbigramunited <- childbookbigram_separated_filter %>%
  unite(bigram, word1, word2, sep = " ")
childbookbigramunited  # reuniting the bigram that does not contain stop words

# Plotting the connection of bigrams 

library(ggraph)
library(igraph)

bigram_graph <- childbookbigramcount %>%
  filter(n > 10) %>%
  graph_from_data_frame()

bigram_graph


# Visualization to show the words connection with each other
set.seed(2020)

A1 <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#Finding negative lexicons using stopwords
not_words <- childbookbigram_separated %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%  # these sentiments are faulty
  arrange(desc(abs(contribution))) %>%
  head(40)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(40) %>%
  mutate(word2 = reorder(word2, contribution)) %>%  # can pipe above to ggplot
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

negativewords <- c("not", "no", "never", "without") # adding more negation words

Negativewordsfinal <- childbookbigram_separated %>%
  filter(word1 %in% negativewords) %>%  # filter for the set of negative words
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)
Negativewordsfinal


boxplot(Negativewordsfinal$value) # Most sentiment lexicons are negative  
# Minimum = -3
# Maximum = 3
# Median = 1
# Q3 = 2
# Q1 = -2


# Adult book bigram 

adultbookbigram <- tibble(text = Adultbook$text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)  # creating the pair of two words that are called bigram where 2 words are combined in a row 

adultbookbigram


adultbookbigram %>%count(bigram, sort = TRUE) # Counting the bigrams to identify stop word pairs

adultbookbigram_separated <- adultbookbigram %>%
  separate(bigram, c("word1", "word2"), sep = " ") # separating the bigram in 2 cols
adultbookbigram_separated

adultbookbigram_separated_filter <- adultbookbigram_separated %>%
  filter(!word1 %in% stop_words$word) %>%  # removing the stop word 
  filter(!word2 %in% stop_words$word)
adultbookbigram_separated_filter

adultbookbigramcount <- adultbookbigram_separated_filter %>% count(word1, word2, sort = TRUE)
adultbookbigramcount # Count of bigrams after removing the common stop words

adultbookbigramunited <- adultbookbigram_separated_filter %>%
  unite(bigram, word1, word2, sep = " ")
adultbookbigramunited  # reuniting the bigram that does not contain stop words

# Plotting the connection of bigrams 

library(ggraph)
library(igraph)

bigram_graph_adult <- adultbookbigramcount %>%
  filter(n > 10) %>%
  graph_from_data_frame()

bigram_graph_adult


# Visualization to show the words connection with each other
set.seed(2020)

A1 <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_adult, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#Finding negative lexicons using stopwords
not_words_adult <- adultbookbigram_separated %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words_adult

library(ggplot2)

not_words_adult %>%
  mutate(contribution = n * value) %>%  # these sentiments are faulty
  arrange(desc(abs(contribution))) %>%
  head(40)

not_words_adult %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%  # can pipe above to ggplot
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

negativewords <- c("not", "no", "never", "without") # adding more negation words

Negativewordsfinaladult <- adultbookbigram_separated %>%
  filter(word1 %in% negativewords) %>%  # filter for the set of negative words
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)
Negativewordsfinaladult


boxplot(Negativewordsfinaladult$value) # Most of the lexicons are positive 
# Minimum = -3
# Maximum = 3
# Median = -1
# Q3 = -2
# Q1 = 2












