
### ----------------------------------------------------------------------
### BelgradeR Meetup :: 30 Nov 2016 :: Startit Center, Savska 5, Belgrade
### Goran S. Milovanovic, PhD, Data Science Serbia
### Methods of Distributional Semantics in R
### ----------------------------------------------------------------------

### Part 1: The {tm} structures for text-mining in R

### --- clear all

### ----------------------------------------------------------------------
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

# - Load Shakespear's Plays: learn about {tm} sources, readers, and corpora
### ----------------------------------------------------------------------
### --- Load Shakespear's Plays

### --- working directory
wDir <- 
  paste0(getwd(),'/FullCorpus/Shakespeare')
setwd(wDir)
list.files(wDir)


# - Load Shakespeare's Plays in a {tm}** `Corpus`

### --- read Shakespeare w. {tm}

# - {tm} data structures
getSources()
getReaders()

# - construct readerControl for Shakespeare:
corpus <- VCorpus(
  DirSource(wDir, 
            encoding = "UTF-8",
            mode = "text"), # uses {base} readLines()
  readerControl = list(reader = readPlain,
                       language = "en")
)

# - how many documents there are?
length(corpus)
class(corpus)


### --- different readers, e.g. XML:

# - Macbeth as a an XML sourced {tm} corpus
# XML reader for Macbeth :: five XML files, each Act = one XML file
# - xmlDir
xmlDir <- paste0(getwd(),'/FullCorpus/XMLExample')
setwd(xmlDir)
list.files(xmlDir)

# reader function:
myXMLReader <- readXML(spec = list(Title = list("node", "/Document/Title"),
                                   Year = list("node", "/Document/Year"),
                                   Language = list("node", "/Document/Language"),
                                   Description = list("node", "/Document/Description"),
                                   content = list("node", "/Document/Content")),
                       doc = PlainTextDocument())
# define source
myXMLSource <- DirSource(directory = xmlDir,
                         encoding = "UTF-8",
                         recursive = FALSE,
                         mode = "text")

# load Example Corpus
xmlCorpus <- VCorpus(myXMLSource, 
readerControl = list(reader = myXMLReader))

# - inspect xmlCorpus
xmlCorpus[[1]]$meta
str_sub(xmlCorpus[[1]]$content,1,1000)

### ----------------------------------------------------------------------
### --- Accessing documents and metadata

# - accessing documents: content and metadata
corpus[[1]]$meta
class(corpus[[1]]$content)
length(corpus[[1]]$content)

corpus[[1]]$content[1:20] # we need to fix this, right

# - PlainTextDocument
class(corpus[[1]])

# - content is character()
class(corpus[[1]]$content)

# - even document-level meta-data has a class of their own:
class(corpus[[1]]$meta)
head(corpus[[1]]$content,10)

### ----------------------------------------------------------------------
### --- Content transformations

# - VCorpus is a list, right? Yes, but...
# - N.B. Do not do this:
# - fix content as obtained from readLines():
# corpus <- lapply(corpus, function(x) {
#   x$content <- paste(x$content, collapse = " ")
# })

# In {tm}, one needs to use a content transformer function
# First, define the function that you want to apply over the document content
contentCollapse <- function(content) {
  content <- paste(content, collapse = " ")
  return(content)
}

# then call tm_map():
corpus <- tm_map(corpus, 
                 content_transformer(contentCollapse),
                 lazy=TRUE)
# test
str_sub(corpus[[1]]$content,1,1000)

### --- Read more on tm_map from {tm} Documentation
# - URL: https://cran.r-project.org/web/packages/tm/tm.pdf
# parameter lazy:
# a logical. Lazy mappings are mappings which are delayed until the content is 
# accessed. It is useful for large corpora if only few documents will be
# accessed. In such a case it avoids the computationally expensive application
# of the mapping to all elements in the corpus.

# - tm_map() call from {tm} on multicore platforms:
# then call tm_map():
# corpus <- tm_map(corpus, 
#                  content_transformer(contentCollapse),
#                  mc.cores = 2)

### ----------------------------------------------------------------------
### --- Metadata access and editing

# - access metadata
head(meta(corpus, tag="id")) # {tm} meta() to access document-level metadata
class(meta(corpus, tag="id"))

playTitle <- unname(sapply(as.character(meta(corpus, tag="id")),
                           function(x) {
                             strsplit(x, split = ".", fixed = T)[[1]][1]
                             }))
playTitle

# - enter new metadata: id
meta(corpus, tag="id", type="local") <- playTitle
head(meta(corpus, tag="id"))

# - enter new metadata: author
head(meta(corpus, tag="author"))

meta(corpus, tag="author", type="local") <- 
  rep("William Shakespeare",length(corpus))
head(meta(corpus, tag="author"))

# - load more metadata
wDir <- paste0(getwd(),'/FullCorpus')
setwd(wDir)
playList <- read.csv('playList.csv',
                     header = T,
                     check.names = F,
                     row.names = 1,
                     stringsAsFactors = F)
str(playList)

# - have we collected all plays?
wMissing <- which(!(playList$Play %in% as.character(meta(corpus,tag="id"))))
# - check
playList$Play[wMissing]

# - is the order of plays correct? - No, but who cares
as.character(meta(corpus, tag="id")) == playList$Play

# - new metadata
meta(corpus, tag = "yearWritten", type = "local") <- 
  unname(sapply(meta(corpus, tag = "id"), function(x) {
    wPlay <- which(playList$Play == as.character(x))
    playList$Written[wPlay]
    }))
# - check
head(meta(corpus, tag = "yearWritten"))

# - check yearWritten tag
corpus[[1]]$meta$id
corpus[[1]]$meta$yearWritten
corpus[[19]]$meta$id
corpus[[19]]$meta$yearWritten

# - origin tag: http://shakespeare.mit.edu/
meta(corpus, tag = "origin", type = "local") <-
  rep("http://shakespeare.mit.edu/", length(corpus))
# - check
head(meta(corpus, tag = "origin"))

# - description tag: I will use this one for play type:
meta(corpus, tag = "description", type = "local") <- 
unname(sapply(meta(corpus, tag = "id"), function(x) {
  wPlay <- which(playList$Play == as.character(x))
  playList$Type[wPlay]
  }))
# - check
head(meta(corpus, tag = "description"))

# - code tag: a key from playList that matches 'DramatisPersonae.csv'
meta(corpus, tag = "code", type = "local") <-  
  unname(sapply(meta(corpus, tag = "id"), function(x) {
    wPlay <- which(playList$Play == as.character(x))
    playList$Code[wPlay]
}))
# - check
head(meta(corpus, tag = "code"))

# - load more metadata
wDir <- paste0(getwd(),'/FullCorpus')
setwd(wDir)
dramatisPersonae <- read.csv('DramatisPersonae.csv',
                             check.names = F,
                             header = T,
                             stringsAsFactors = F)
head(dramatisPersonae)
  
# - produce dramatis personae tags for corpus
dPersonae <- character(length(corpus))
dPersonae <- sapply(playList$Code, function(x) {
  wPlay <- which(grepl(x ,dramatisPersonae$Play, fixed=T))
  paste(dramatisPersonae$Character[wPlay], collapse=", ")
})
# enter dramatis personae to playList
playList$Characters <- dPersonae
head(playList$Characters)

# - check
wMacbeth <- which(grepl('Macbeth',playList$Play,fixed=T))
playList$Characters[wMacbeth]

# - enter dramatis personae metadata to corpus
meta(corpus, tag = "characters", type = "local") <- 
unname(sapply(meta(corpus, tag = "id"),
function(x) {
wPlay <- which(playList$Play == as.character(x))
playList$Characters[wPlay]
}))
# - check
wPlay <- which(meta(corpus, tag="id") %in% "The Tempest")
corpus[[wPlay]]$meta$characters
wPlay <- which(meta(corpus, tag="id") %in% "Romeo and Juliet")
corpus[[wPlay]]$meta$characters

### --- writeCorpus()
saveRDS(corpus, file = "ShakespeareAnnotated.Rds")

### --- writeCorpus()
outDir <- paste0(getwd(),"/FullCorpus/outCorpus")
setwd(outDir)
writeCorpus(corpus)

### - Visit my blog :: [The Exactness of Mind](http://www.exactness.net), 12/24/2016.  
