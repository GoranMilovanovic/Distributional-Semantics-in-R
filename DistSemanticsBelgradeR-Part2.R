
### ----------------------------------------------------------------------
### BelgradeR Meetup :: 30 Nov 2016 :: Startit Center, Savska 5, Belgrade
### Goran S. Milovanovic, PhD, Data Science Serbia
### Methods of Distributional Semantics in R
### ----------------------------------------------------------------------

### Part 2: Entity Recognition with {openNLP}

### ----------------------------------------------------------------------
### --- 2.1 clear all + libraries
rm(list=ls())

### --- libraries

# - helpers
library(dplyr)
library(tidyr)

# - text-mining: pre-processing + stemmer, tokenizer, sentiment
library(tm)
library(tm.plugin.sentiment)
library(XML)
library(tau)
library(stringr)

# - SnowballC for Porter's stemmer
library(SnowballC)

# - Support + JVM Options
library(rJava)
numCores <- 2; # for multicore platforms
options(mc.cores=numCores)
.jinit(classpath="myClasses.jar", parameters="-Xmx512m")
library(RWeka) # load RWeka for tokenizers only after setting Options

# - topic models via Latent Dirichlet Allocation
library(topicmodels)

# - entitity recognition via Apache OpenNLP
library(openNLP)

# - graphics
library(ggplot2)
library(ggrepel)
library(igraph)
library(wordcloud)

# - analytics
library(Hmisc)
library(smacof)
library(ape)

# - parallelism
library(snowfall)

### ----------------------------------------------------------------------
### --- 2.2 Load Shakespear's Plays

# - Load Shakespeare's Plays as a {tm} Corpus
outDir <- paste0(getwd(),"/FullCorpus")
setwd(outDir)
corpus <- readRDS('ShakespeareAnnotated.Rds')

# - Document-level Metadata in corpus:
corpus[[1]]$meta

WScharacters <- meta(corpus, tag = "characters")
head(WScharacters,2)

# - Macbeth:
corpus[[19]]$meta$id
macbeth <- corpus[[19]]$content
class(macbeth)

### ----------------------------------------------------------------------
### --- 2.3 Some pre-processing
  
# - a small correction: CHARACTERS to Names:
# - CHARACTERS to Titles w. {string}:
candidateCharacters1 <- unique(str_extract_all(macbeth, "[[:upper:]]{2,}\\s[[:upper:]]{2,}")[[1]])
candidateCharacters2 <- unique(str_extract_all(macbeth, "[[:upper:]]{2,}")[[1]])
candidateCharacters1

candidateCharacters2

candidateCharacters <- c(candidateCharacters1, candidateCharacters2)
# - clean up w. regex:
candidateCharacters <- candidateCharacters[-which(grepl("^ACT|\\bACT\\b", candidateCharacters))]
candidateCharacters <- candidateCharacters[-which(grepl("^SCENE|\\bSCENE\\b", candidateCharacters))]
# - Remove Roman numerals I - X w. regex:
candidateCharacters <- candidateCharacters[-which(grepl("^I{1,3}$|^VI{1,3}$|^IV$|^V$|^IX$|^X$", candidateCharacters))]
candidateCharacters

for (i in 1:length(candidateCharacters)) {
  macbeth <- str_replace_all(macbeth, candidateCharacters[i], str_to_title(candidateCharacters[i]))
}

### ----------------------------------------------------------------------
### --- 2.4 {NLP} and {openNLP} word and sentence annotations**
  
# - String class for {NLP} and {openNLP}
macbeth <- as.String(macbeth)
class(macbeth)

macbeth[10,20] # subsetting string objects in {NLP}

macbeth[102,178]

wordAnnotator <- Maxent_Word_Token_Annotator(language = "en") # {openNLP}
wordAnnotator

sentenceAnnotator <- Maxent_Sent_Token_Annotator(language = "en") # {openNLP}
sentenceAnnotator

annotatedMacbeth <- NLP::annotate(macbeth,
                                  list(sentenceAnnotator, wordAnnotator))
class(annotatedMacbeth)

length(annotatedMacbeth)

annotatedMacbeth[[1]]

annotatedMacbeth[[2]]

annotatedMacbeth[[2]]$start

macbeth[annotatedMacbeth[[2]]$start, annotatedMacbeth[[2]]$end]

annotatedMacbeth[[1]]$features

class(annotatedMacbeth[[1]]$features)

annotatedMacbeth[[1]]$features[[1]]$constituents

# $constituents:
annMacbeth <- as.data.frame(annotatedMacbeth)
head(annMacbeth)

tail(annMacbeth)

annMacbeth <- annMacbeth %>% 
  filter(type == "word")
annotatedMacbeth[[1]]$features[[1]]$constituents

length(annotatedMacbeth[[1]]$features[[1]]$constituents)
# the constituents of annotatedMacbeth[[1]]: words in a sentence
annMacbeth$id[1:8]

macbethAPT <- AnnotatedPlainTextDocument(macbeth, annotatedMacbeth)
class(macbethAPT)

is.list(macbethAPT)

length(macbethAPT$annotations[[1]])

macbethAPT$annotations[[1]][[23274]]

length(sents(macbethAPT)) # 1195

is.list(sents(macbethAPT))

sents(macbethAPT)[[100]]

class(sents(macbethAPT)[[1]])

sents(macbethAPT) %>% head(5)

words(macbethAPT) %>% head(100)

# - clear
rm(list = c('annMacbeth', 'annotatedMacbeth', 'macbethAPT'))

### ----------------------------------------------------------------------
### --- 2.5 Entity recognition with {openNLP}**
  
### --- Entity Recognition from {openNLP}
# - create an annotator:
# - list of models available in openNLP 1.5 series:
# - http://openNLP.sourceforge.net/models-1.5/
# - visit: http://datacube.wu.ac.at/
# - To install for Spanish:
install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.es_1.5-1.tar.gz",
                 repos = NULL,
                 type = "source")
# - To install for Dutch:
install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.nl_1.5-2.tar.gz",
                 repos = NULL,
                 type = "source")

characterAnnotatorEN <- Maxent_Entity_Annotator(language = "en", kind = "person")
characterAnnotatorES <- Maxent_Entity_Annotator(language = "es", kind = "person")
characterAnnotatorNL <- Maxent_Entity_Annotator(language = "nl", kind = "person")

annotatedMacbeth <- NLP::annotate(macbeth,
                                  list(sentenceAnnotator,
                                       wordAnnotator,
                                       characterAnnotatorEN,
                                       characterAnnotatorES,
                                       characterAnnotatorNL))

as.data.frame %>%
  filter(type == "entity")
# - extract Shakespeare's characters from Macbeth:
charactersMachbet <- str_sub(as.character(macbeth),
                             start = annotatedMacbeth$start,
                             end = annotatedMacbeth$end) %>%
  unique()
charactersMachbet

charactersMachbetCorpus <- unlist(strsplit(WScharacters[[19]],
                                           split = ", ", fixed = T)[[1]])
foundCharacters <- charactersMachbetCorpus[which(charactersMachbetCorpus %in% charactersMachbet)]
foundCharacters

acc <- round((length(foundCharacters)/length(charactersMachbetCorpus))*100,2)
acc

# - let's provide a small assistance to {openNLP}...
charactersMachbet <- unique(c(charactersMachbet,
                              unique(
                                unlist(strsplit(charactersMachbet, 
                                                split = " ", 
                                                fixed = T)))
)
)
foundCharacters <- 
  charactersMachbetCorpus[which(charactersMachbetCorpus %in% charactersMachbet)]
foundCharacters

acc <- round((length(foundCharacters)/length(charactersMachbetCorpus))*100,2)
acc

### ----------------------------------------------------------------------
### --- 2.6 The whole Shakespeare undergoes {openNLP} entity recognition...

# - Annotate all plays
### ----------------------------------------------------------------------
recognized <- character()
accuracy <- numeric()
# - a directory where annotated plays will be saved as .Rds files
setwd(paste0(getwd(),"/FullCorpus/openNLPAnnotations"))
for (i in 1:length(corpus)) {
  print(paste0("Processing play ", i, ". out of 37..."))
  play <- corpus[[i]]$content
  # - CHARACTERS to Titles w. {string}:
  candidateCharacters1 <- 
    unique(str_extract_all(play, "[[:upper:]]{2,}\\s[[:upper:]]{2,}")[[1]])
  candidateCharacters2 <- 
    unique(str_extract_all(play, "[[:upper:]]{2,}")[[1]])
  candidateCharacters <- 
    c(candidateCharacters1, candidateCharacters2)
  # - clean up w. regex:
  candidateCharacters <- 
    candidateCharacters[-which(grepl("^ACT|\\bACT\\b", candidateCharacters))]
  candidateCharacters <- 
    candidateCharacters[-which(grepl("^SCENE|\\bSCENE\\b", candidateCharacters))]
  # - Remove Roman numerals I - X w. regex:
  candidateCharacters <- 
    candidateCharacters[-which(grepl("^I{1,3}$|^VI{1,3}$|^IV$|^V$|^IX$|^X$", candidateCharacters))]
  # - implement:
  for (j in 1:length(candidateCharacters)) {
    play <- 
      str_replace_all(play, candidateCharacters[j], str_to_title(candidateCharacters[j]))
  }
  
  # - annotate w. {openNLP}
  play <- as.String(play)
  annotatedPlay <- NLP::annotate(play,
                                 list(sentenceAnnotator,
                                      wordAnnotator,
                                      characterAnnotatorEN,
                                      characterAnnotatorES,
                                      characterAnnotatorNL))
  saveRDS(annotatedPlay, file = paste0(corpus[[i]]$meta$id,
                                       "_Annotated.Rds"))
  
  annotatedPlay <- annotatedPlay %>%
    as.data.frame() %>%
    filter(type == "entity")
  
  foundCharacters <- str_sub(as.character(play),
                             start = annotatedPlay$start,
                             end = annotatedPlay$end) %>%
    unique()
  
  # - a small hint to help {openNLP}:
  foundCharacters <- unique(c(foundCharacters,
                              unique(
                                unlist(strsplit(foundCharacters,
                                                split = " ",
                                                fixed = T)))
  )
  )
  
  corpusCharacters <- unlist(strsplit(corpus[[i]]$meta$characters,
                                      split = ", ",
                                      fixed = T))
  
  recognized[i] <- 
    paste(corpusCharacters[which(corpusCharacters %in% foundCharacters)],
          collapse = ", ")
  
  accuracy[i] <- 
    length(corpusCharacters[which(corpusCharacters %in% foundCharacters)])/length(corpusCharacters)
  
  rm(list = c('play','annotatedPlay','foundCharacters','corpusCharacters'))
  
}

charRecognition <- data.frame(play = as.character(meta(corpus, tag = "id")),
                              recognized = recognized,
                              accuracy = accuracy,
                              stringsAsFactors = F)
charRecognition$type = as.character(meta(corpus, tag = "description"))
charRecognition$numCharsFound <- sapply(charRecognition$recognized, function(x) {
  length(strsplit(x, split = ", ", fixed = T)[[1]])
})
charRecognition$numCharsCorpus <- sapply(meta(corpus, tag = "characters"), function(x) {
  length(strsplit(x, split = ", ", fixed = T)[[1]])
})
write.csv(charRecognition, file = "charRecognition.csv")

### ----------------------------------------------------------------------
### --- 2.7 Evaluation: how well does {openNLP} perform against Shakespeare's plays?

# - Results

rm(list=ls())
library(dplyr)
library(ggplot2)
charRecognition <- read.csv("charRecognition.csv",
                            header = T,
                            row.names = 1,
                            stringsAsFactors = F)
head(charRecognition)

summary(charRecognition$accuracy)

charRecognition$recognized[26] # Romeo and Juliet

charRecognition$recognized[19] # Macbeth

charRecognition$recognized[15] # Julius Caesar

# - accuracy density plot 
ggplot(data = charRecognition) +
  geom_line(aes(accuracy), stat="density", color = "black", linetype = 2) + 
  geom_line(aes(accuracy, color = type), stat="density") + 
  scale_colour_manual(values = c("cadetblue4","firebrick4", "darkorchid4")) +
  theme_classic() +
  ggtitle("{openNLP} Named Entity Recognition\nShakespeare's Plays") +
  theme(axis.line.y = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(plot.title = element_text(size = 9, hjust = .5)) +
  xlim(0,1)

# - plot accuracy by type
ggplot(data = charRecognition,
       aes(x = type, y = accuracy, color = type)) +
  scale_colour_manual(values = c("cadetblue4","firebrick4", "darkorchid4")) +
  geom_jitter(aes(alpha = accuracy), size = 3.5, width = .1) + 
  ylim(0, max(charRecognition$accuracy)+.2) + xlab(NULL) + ylab("Accuracy") + 
  ggtitle("{openNLP} Named Entity Recognition\nShakespeare's Plays") + 
  theme_classic() + 
  theme(axis.line.y = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(plot.title = element_text(size = 9, hjust = .5))

# - accuracy by type
charRecognition %>% 
  group_by(type) %>% 
  summarise(MeanAcc = mean(accuracy), 
            StDevAcc = sd(accuracy),
            meanNumChar = mean(numCharsCorpus)) %>% 
  arrange(desc(MeanAcc))

# - plot accuracy by type 2
ggplot(data = charRecognition,
       aes(x = numCharsCorpus, y = accuracy, color = type)) +
  scale_colour_manual(values = c("cadetblue4","firebrick4", "darkorchid4")) + 
  theme_classic() +
  geom_smooth(method = lm, alpha = .05) +
  geom_point(aes(alpha = accuracy)) +
  ylim(0, max(charRecognition$accuracy)+.2) + 
  xlim(0, 70) +  
  ylab("Accuracy") + xlab("Characters in Corpus") + 
  ggtitle("{openNLP} Named Entity Recognition\nShakespeare's Plays") +
  theme(axis.line.y = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(plot.title = element_text(size = 9, hjust = .5))

### --- Logistic Regression over Acc (proportion correct)

playsFit1 <- 
  glm(cbind(numCharsFound, numCharsCorpus-numCharsFound) ~ type + type:numCharsCorpus,
      family = binomial(logit),
      data = charRecognition)
summary(playsFit1)

exp(playsFit1$coefficients)

playsFit2 <- 
  glm(cbind(numCharsFound, numCharsCorpus-numCharsFound) ~ type,
      family = binomial(logit),
      data = charRecognition)
summary(playsFit2)

exp(playsFit2$coefficients)

# - model selection: compare model Akaike's Information Criteria:
playsFit1$aic < playsFit2$aic

### --- Why did the name recognition failed for histories 
### --- in comparison to tragedies and comedies?

wDir <- paste0(getwd(),'/FullCorpus')
setwd(wDir)
playList2 <- read.csv('playList2.csv',
                      header = T,
                      check.names = F,
                      stringsAsFactors = F)
str(playList2)

# - clear up playList2 a bit:
library(stringr)
playList2$Genre <- str_trim(playList2$Genre, side = "right")
playList2$City <- str_trim(playList2$City, side = "right")
playList2$Country_Modern <- str_trim(playList2$Country_Modern, side = "right")
playList2$CountryNote <- str_trim(playList2$CountryNote, side = "right")
head(playList2)

# - Load Shakespeare's Plays as a {tm} Corpus
library(tm)
# - Load Shakespeare's Plays as a {tm} Corpus
outDir <- paste0(getwd(),"/FullCorpus")
setwd(outDir)
corpus <- readRDS('ShakespeareAnnotated.Rds')

# - new metadata
meta(corpus, tag = "description2", type = "local") <- 
  unname(sapply(meta(corpus, tag = "id"),
                function(x) {
                  wPlay <- which(playList2$Title == as.character(x))
                  playList2$Genre
                }))
meta(corpus, tag = "city", type = "local") <- 
  unname(sapply(meta(corpus, tag = "id"),
                function(x) {
                  wPlay <- which(playList2$Title == as.character(x))
                  playList2$City
                }))
meta(corpus, tag = "country", type = "local") <- 
  unname(sapply(meta(corpus, tag = "id"),
                function(x) {
                  wPlay <- which(playList2$Title == as.character(x))
                  playList2$Country_Modern
                }))
meta(corpus, tag = "countryNote", type = "local") <- 
  unname(sapply(meta(corpus, tag = "id"),
                function(x) {
                  wPlay <- which(playList2$Title == as.character(x))
                  playList2$CountryNote
                }))

### --- writeCorpus()
# - Load Shakespeare's Plays as a {tm} Corpus
outDir <- paste0(getwd(),"/FullCorpus")
setwd(outDir)
saveRDS(corpus, file = "ShakespeareAnnotated.Rds")

type <- unlist(meta(corpus, tag = "description"))
wComedy <- which(type == "Comedy")
wTragedy <- which(type == "Tragedy")
wHistory <- which(type == "History")

comedyCountries <- unname(unlist(meta(corpus, tag = "country")))[wComedy]
table(comedyCountries)

tragedyCountries <- unname(unlist(meta(corpus, tag = "country")))[wTragedy]
table(tragedyCountries)

historyCountries <- unname(unlist(meta(corpus, tag = "country")))[wHistory]
table(historyCountries)


### - Visit my blog :: [The Exactness of Mind](http://www.exactness.net), 12/24/2016.  
