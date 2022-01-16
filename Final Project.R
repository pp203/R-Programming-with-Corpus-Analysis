# 4DO3 Final Project

library(tidyverse)
library(quanteda)
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)
load("harry_potter_corpus.rda")
harrypotter_corpus <- corpus(harrypotter_corpus)


# ------------------------------------------------------------------------------
# # Analysis of the character development of Voldemort
#1 Tokenize
harry_tok <- tokens(harrypotter_corpus, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE)
harry_tok <- tokens_tolower(harry_tok) 


#2 relative frequency
dfm_harry <- dfm(harry_tok)
freq_harry <- dfm_weight(dfm_harry, scheme = "prop")
freq_harry_v <- dfm_select(freq_harry, pattern = "Voldemort",
                           valuetype = "fixed", selection = "keep")
df <- as.data.frame(freq_harry_v)
df


#3 plot relative frequency
df <- df %>% add_column(text = c(1:7))
plot(voldemort~text, data = df)

cor.test(df$text, df$voldemort, method = "spearman")
# # p-value = 0.02381
# # since p-value is close to 0, they are independent and no relationship
# # rho 0.8571429 


#4 dispersion of Voldemort
textplot_xray(kwic(harry_tok, pattern = "Voldemort"),
              scale = "relative", 
              sort = T)
# # or
tokens_subset(harry_tok) %>% kwic("Voldemort") %>% textplot_xray()


#5 frequencies of text 1 and text 7 
freq_t1_v <- tokens_subset(harry_tok, BookNumber == "1") %>% kwic("voldemort")
freq_t1_y <- tokens_subset(harry_tok, BookNumber == "1") %>% kwic("You-Know-Who")

freq_t7_v <- tokens_subset(harry_tok, BookNumber == "7") %>% kwic("voldemort")
freq_t7_y <- tokens_subset(harry_tok, BookNumber == "7") %>% kwic("You-Know-Who")

freq_t1_t7 <- matrix(c(nrow(freq_t1_v), nrow(freq_t7_v), nrow(freq_t1_y), nrow(freq_t7_y)), 
                     2, 2, 
                     dimnames = list(c("text1", "text7"), c("Voldemort", "You-Know-Who")))

num <- chisq.test(freq_t1_t7, correct = F)$statistic
denom <- sum(freq_t1_t7)*(min(dim(freq_t1_t7))-1)
phi <- sqrt(num/denom)
phi


#6 concordance lines
harry_tok_t1 <- tokens_subset(harry_tok, BookNumber == "1")
harry_tok_t7 <- tokens_subset(harry_tok, BookNumber == "7")

k_t1 <- kwic(harry_tok_t1, pattern = "Voldemort", valuetype = "fixed", window = 2)
head(k_t1, 10)

k_t7 <- kwic(harry_tok_t7, pattern = "Voldemort", valuetype = "fixed", window = 2)
head(k_t7, 10)


# ------------------------------------------------------------------------------
# # Analysis of books in the series

# # Part 1
#1
textstat_readability(harrypotter_corpus, measure = c("Flesch.Kincaid"))


#2
harry_tok_stop <- tokens_remove(harry_tok, pattern = stopwords("English"))

type<- ntype(harry_tok_stop)
token <- ntoken(harry_tok_stop) #length for each book
type/token

# # or
harry_ttr <- textstat_lexdiv(harry_tok_stop, measure = "TTR")


# ------------------------------------------------------------------------------
# # Part 2 
# # I chose text 2 and text 3

#3 keyword analysis
dfm_harry <- dfm(harry_tok_stop)
dfm_t2_t3 <- dfm_subset(dfm_harry, BookNumber %in% c("2", "3")) 

textplot_keyness(textstat_keyness(dfm_t2_t3, measure = "lr"),
                 show_reference = TRUE, 
                 show_legend = TRUE,
                 n = 10,
                 min_count = 2)


#4 10 collocations 
# # text 2
tokens_t2 <- tokens_subset(harry_tok_stop, BookNumber == "2")
colloc_t2_l <- textstat_collocations(tokens_t2, 
                                     method = "lambda",
                                     size = 2, 
                                     min_count = 2) %>% arrange(desc(lambda))
head(colloc_t2_l, 10) 
# # greatest collocation strength is "whomping willow" 

colloc_t2 <- textstat_collocations(tokens_t2, 
                                     method = "lambda",
                                     size = 2, 
                                     min_count = 2) %>% arrange(desc(count))
head(colloc_t2, 10) 
# # greatest frequency is "said harry" in text 2

# # text 3
tokens_t3 <- tokens_subset(harry_tok_stop, BookNumber == "3")
colloc_t3_l <- textstat_collocations(tokens_t3, 
                                method = "lambda",
                                size = 2, 
                                min_count = 2) %>% arrange(desc(lambda))
head(colloc_t3_l, 10) 
# # greatest collocation strength is "diagon alley" text 1

colloc_t3 <- textstat_collocations(tokens_t3, 
                                     method = "lambda",
                                     size = 2, 
                                     min_count = 2) %>% arrange(desc(count))
head(colloc_t3, 10) 
# # greatest frequency is "said harry" in text 1



#5 create wordcloud
# # text 2
tokens_t2_comp <- tokens_compound(tokens_t2, 
                                  phrase("said harry"),
                                  valuetype = "fixed")
tokens_remove_t2 <- tokens_select(tokens_t2_comp, pattern = "said_harry",
                                  valuetype = "fixed", selection = "remove")
dfm_t2_fix <- dfm(tokens_remove_t2)
textplot_wordcloud(dfm_t2_fix, 
                   min_size = 1, max_size = 4, 
                   min_count = 3, max_words = 500)

# # text 3
tokens_t3_comp <- tokens_compound(tokens_t3, 
                                  phrase("said harry"),
                                  valuetype = "fixed")
tokens_remove_t3 <- tokens_select(tokens_t3_comp, pattern = "said_harry",
                                  valuetype = "fixed", selection = "remove")
dfm_t3_fix <- dfm(tokens_remove_t3)
textplot_wordcloud(dfm_t3_fix, 
                   min_size = 1, max_size = 4, 
                   min_count = 3, max_words = 500)


# # SIDE: you can also compare between text 1 and text 2 in one plot
tokens_t2_t3 <- tokens_subset(harry_tok_stop, BookNumber %in% c("2", "3"))
tokens_t2_t3_comp <- tokens_compound(tokens_t2_t3, 
                                     phrase("said harry"),
                                     valuetype = "fixed")
tokens_remove_t2_t3 <- tokens_select(tokens_t2_t3_comp, pattern = "said_harry",
                            valuetype = "fixed", selection = "remove")

dfm_t2_t3_fix <- dfm(tokens_remove_t2_t3)
textplot_wordcloud(dfm_t2_t3_fix, 
                   min_size = 0.5, max_size = 4, 
                   min_count = 3, max_words = 500, 
                   comparison = TRUE, color = c("blue", "red"))


#6 network plot
# # text 2
fcm_t2 <- fcm(tokens_remove_t2, # we can use this data from question 5
              context = "window", 
              window = 5, 
              ordered = T)
fcm_top_t2 <- fcm_select(fcm_t2,
                         pattern = names(topfeatures(fcm_t2, 20)),
                         selection = "keep",
                         valuetype = "fixed")
textplot_network(fcm_top_t2, min_freq = 2)

# # text 3
fcm_t3 <- fcm(tokens_remove_t3, # we can use this data from question 5
              context = "window", 
              window = 5, 
              ordered= T)
fcm_top_t3 <- fcm_select(fcm_t3,
                         pattern = names(topfeatures(fcm_t3, 20)),
                         selection = "keep",
                         valuetype = "fixed")
textplot_network(fcm_top_t3, min_freq = 2)
