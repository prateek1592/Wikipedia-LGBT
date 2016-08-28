library(rvest)
library(dplyr)
library(stringr)
library(babynames)
library(reshape2)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(NLP)
library(ggplot2)
library(RCurl)
library(combinat)
library(RWeka)
library(ngram)
library(XML)

# Defining a list to include functionality of 
# returning multiple arguments from a function
# Ref: https://stat.ethz.ch/pipermail/r-help/2004-June/053343.html

list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

url <- "https://en.wikipedia.org/wiki/List_of_gay,_lesbian_or_bisexual_people:_"
l1 <- "A.Ba-Bh.Bi-Bz.C.D-E.F.G.H.I-J.K.L.M.N-O.P-Q.R.Sa-Sc.Sd-Si.Sj-Sz.T-V.W-Z"
l1 <- strsplit(l1, split='\\.')[[1]]
urls <- paste0(url, l1)


# Applying a function to extract data tables from each URL
data <- lapply(urls, function(x) {
  temp <- x %>% read_html %>% html_nodes("table")
  indx <- grep("wikitable sortable",temp)
  
  for (j in seq_along(indx)){
    if (j==1){
      mat <- html_table(temp[indx[j]], fill=T)[[1]][,1:5]
    }
    else {
      mat2 <- html_table(temp[indx[j]], fill=T)[[1]][,1:5]
      mat <- rbind(mat,mat2)
    }
  }
  names(mat) <- gsub("\\[[^\\]]*\\]","",names(mat))
  return(mat)
})


# List to df
data <- do.call(rbind,data)
data[c(6,9),]

list1 <- list(data$Lifetime,data$Notes)
list1 <- lapply(list1, function(x) gsub("\\[[^\\]]*\\]", "", x))

# Utilising the previously defined "list" class
list[data$Lifetime, data$Notes] <- list1

# Cleaning up the values through case-by-case 
# removal/substitution of non-relevant characters
data$Lifetime <- gsub(" !b.*", "", data$Lifetime)
data$Lifetime <- gsub(".*!","",data$Lifetime)
data$Lifetime <- gsub("/.*-","-",data$Lifetime)
data$Lifetime <- gsub("-","-",data$Lifetime)
data$Lifetime <- gsub("/.*","",data$Lifetime)
data$Lifetime <- gsub("c.","",data$Lifetime)
data$Lifetime <- gsub("d. ","-",data$Lifetime)
data$Lifetime <- gsub("b.","",data$Lifetime)
data$Lifetime <- gsub("p.","",data$Lifetime)
data$Lifetime <- gsub("BCE","",data$Lifetime)
data$Lifetime <- gsub("BC","",data$Lifetime)
data$Lifetime <- gsub("AD","",data$Lifetime)
data$Lifetime <- gsub("CE","",data$Lifetime)
data$Lifetime <- gsub("E","",data$Lifetime)
data$Lifetime <- gsub("B. ?","",data$Lifetime)
data$Lifetime <- gsub("\\?","",data$Lifetime)
data$Lifetime <- gsub("\\..*","",data$Lifetime)
data$Lifetime <- gsub(" ","",data$Lifetime)
data$Lifetime <- gsub("\n.*","",data$Lifetime)
data$Lifetime <- gsub("([A-Za-z])","",data$Lifetime)
data$Lifetime <- gsub("^0$","",data$Lifetime)
data$Lifetime <- gsub(" ","",data$Lifetime)

head(data$Lifetime)

unwanted_array = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                      'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y', 'û'='u',
                      'ü'='u', 'ð'='d')

# A more concise way for gsub when
# sequential substitution is not required
data$Name <- chartr(paste(names(unwanted_array), collapse=''),
                    paste(unwanted_array, collapse=''),data$Name)
data$Name <- gsub(".*!","",data$Name)
data$Name <- gsub("([.])|[[:punct:]]","\\1",data$Name)
data$Name <- gsub("(aka).*","",data$Name)
data$Name <- gsub('([[:upper:]])', ' \\1', data$Name)
data$Name <- gsub("<[^>]*>","",data$Name)
data$Name <- unlist(lapply(data$Name,function(x)paste(unique(strsplit(
  x," ")[[1]]),collapse=" ")))
data$Name <- gsub("^\\s+|\\s+$", "", data$Name)
data$Name[6:9]

gi <- c("-","/", "born","USA","Romani ","United States","English")
go <- c(" "," ","","American","Romanian ","American","British")
names(go) <- gi
data$Nationality <- str_replace_all(data$Nationality, go)
data$Notes <- gsub('\\[.*?\\]',"",data$Notes)
data$Notes <- gsub('\\.',"",data$Notes)
data$Notes <- gsub("^\\s+|\\s+$", "", data$Notes)

head(data[,c("Nationality","Notes")], 3)

data <- data[!duplicated(data$Name),]
data$Nationality <- gsub("born","",data$Nationality)
data$Nationality <- gsub("([[:punct:]])"," ",data$Nationality)
data$Nationality <- strsplit(data$Nationality, " ")
wordcloud(unlist(data$Nationality),max.words=100,colors=brewer.pal(8,"Dark2"))

vocations <- unlist(strsplit(data$`Notable as`," "))
vocations <- gsub("[^A-Za-z]","",vocations)
vocations <- vocations[vocations!=""]
vocations <- tolower(vocations)
wordcloud(vocations,max.words=100,colors=brewer.pal(8,"Dark2"))

length(data$Name[grep("politician|Politician", data$`Notable as`)])/nrow(data) * 100

politician.data <- data$Nationality[grep("politician|Politician", data$`Notable as`)]
wordcloud(unlist(politician.data),max.words=100,colors=brewer.pal(8,"Dark2"),random.order=F,random.color=F)

top <- plyr::count(vocations) %>% arrange(desc(freq)) %>% head(.,10)
words_to_remove <- unique(vocations)
words_to_remove <- words_to_remove[!grepl(paste("\\b",paste(top$x,collapse="\\b|\\b"),"\\b",sep=""),words_to_remove)]

clean.up <- function(x) {
  x <- removePunctuation(x)
  x <- tolower(x)
  x <- removeWords(x,stopwords("en"))
  x <- removeWords(x,words_to_remove)
  x <- gsub("[^A-Za-z]"," ",x)
  x <- gsub("^\\s+|\\s+$", "", x)
  return(x)
}

# Function to generate 2-grams
ngramTokenizer <- function(x,n) {
  unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names =
           FALSE)
}

corp <- Corpus(VectorSource(clean.up(data$`Notable as`)))
tdm <- TermDocumentMatrix(corp, control = list(tokenize = function(x)
  ngramTokenizer(x,2),stopwords = TRUE))
top.pairs <- data.frame(sort(rowSums(as.matrix(tdm)), decreasing=TRUE))
names(top.pairs) <- "top.pairs"
head(top.pairs)

data.frame(babynames)[1:3,1:4]

namedf <- data.frame(acast(babynames,name ~ sex,sum,value.var = "n"))
namedf$Name <- row.names(namedf)
namedf <- namedf %>% mutate(score=M-F) %>% select(Name,score)
namedf <- matrix(namedf$score,ncol=1,dimnames=list(namedf$Name,"score"))

# The output of the function depends on the gender
# allocated to each individual word in a name
# along with the word's prevalence in the babynames set

gendercheck <- function(name){
  splts <- unlist(strsplit(name," "))
  splts.score <- sapply(splts, function(x){
    score <- try(namedf[x,],silent = T)
    if(inherits(score,"try-error")) {score = 0}
    return(score)
  })
  splts.score <- sum(splts.score)
  splts.score <- ifelse(splts.score>0,"M",ifelse(splts.score<0,"F",0))
  return(splts.score)
}

# Gender guessing only for cases where 
# sexuality is listed as bi-sexual

data$Sex <- NA
for (i in 1:nrow(data)){
  data$Sex[i]=ifelse(grepl("L",data$Notes[i]),"F",ifelse(
    grepl("G",data$Notes[i]),"M",gendercheck(data$Name[i])))
}

sum(data$Sex==0)/nrow(data)*100
data <- data[-which(data$Sex==0),]

# Trying to extract birthyear
birthyear <- substr(data$Lifetime,1,4)
birthyear[grep("[^0-9]",birthyear)] <- 0
birthyear <- as.numeric(birthyear)
birthyear[is.na(birthyear)] <- 0
data$Birth <- birthyear

sex.data <- data %>% subset(.,select=c(Notes,Sex))
sex.data <- plyr::count(sex.data)
sex.data$Notes <- gsub("^G$|^L$","G/L",sex.data$Notes)
names(sex.data)[1] <- "Orientation"

ggplot(sex.data, aes(x=Sex, y=freq, fill=Orientation)) +
  geom_bar(stat="identity", position="fill",width=0.5) + 
  labs(y = "Relative frequency")

n <- nrow(data)
links <- numeric(n)

#### CAUTION 
#### THIS CODE CHUNK IS VERY SLOW

# Trial-and-error for getting wikipedia page link
# Inefficient solution
for (j in 1:n){
  print(j)
  nom <- data$Name[j]
  mash <- unlist(strsplit(nom, split=" "))
  if (length(mash)>3) {next}
  con <- do.call(rbind,(permn(mash)))
  con <- do.call(paste, c(data.frame(con),sep="_"))
  for (i in seq_along(con)){
    link = paste("https://en.wikipedia.org/wiki/",con[i],sep="")
    if(url.exists(link)) {links[j]=link; break}
  }
}

data$Link <- links
partners <- numeric(n)

# Opening link if exists & extracting
# info from the infobox about the partners/spouses
for (i in 1:n){
  print(i)
  if (data$Link[i]!=0){
    temp <- data$Link[i] %>% 
      read_html %>%
      html_nodes("table")
    indx <- grep("infobox", temp)[1]
    if (!is.na(indx)){
      infobox <- html_table(temp[indx],fill=T)
      partner.indx <- grep("Partner|partner|spouse|Spouse",infobox[[1]][,1])[1]
      if (!is.na(partner.indx)){partners[i] <- infobox[[1]][partner.indx,2]}
    }
  }
}

data$Partners <- partners

####

partners <- data$Partners
partners <- gsub("\\([^\\)]*\\)", "\n",partners)
partners <- gsub("\\[[^\\]]*\\]", "",partners)
partners <- gsub("\\b[a-z]+\\b","",partners)
partners <- gsub("partner|Partner|children|child|none|Jr|
                 divorced|deceased|separated", "",partners)
partners <- gsub("([[:punct:]])", "",partners)
partners <- gsub(" \n", "\n",partners)
partners <- gsub("\\n+","\n",partners)
partners <- gsub("\\d", "",partners)
partners <- gsub("^\\s+","",partners)
partners <- gsub("\\s+$","",partners)
partners <- strsplit(partners, "\n|,")

# Guessing gender of partners
data$pgender <- NA
for (i in 1:nrow(data)){
  data$pgender[i] <- paste0(sapply(partners[[i]], function(x) {
    x <- gsub("^\\s+|\\s+$","",x)
    gendercheck(x)
  }),collapse=",")
}

data[grepl(0,data$pgender),"pgender"] <- ""

years <- lapply(data$Partners, function(x) {
  temp <- regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]]
  temp <- gsub("\\D*(\\d+).*","\\1",temp)
  temp <- gsub("\\D","",temp)
  temp <- paste0(temp,collapse=",")
  return(temp)
})
data$wbells <- years

married.folk <- data %>% 
  filter(pgender!="") %>% 
  select(Name,Sex,pgender,wbells,Birth,Nationality,`Notable as`)
married.folk$wbells <- gsub(",+",",",married.folk$wbells)

# Comparing the individuals' genders with their partners.
sex.list = c("M","F")
for (i in 1:nrow(married.folk)){
  other.sex <- setdiff(sex.list,married.folk$Sex[i])
  married.folk$pgender[i] <- gsub(married.folk$Sex[i],"1",married.folk$pgender[i])
  married.folk$pgender[i] <- gsub(other.sex,"0",married.folk$pgender[i])
}

commas <- function(x) {
  temp <- gregexpr(",",x)
  temp <- do.call(rbind,lapply(temp, function(y) {
    f1 <- attr(y,"match.length")
    f1 <- pmax(sum(f1),0)
  }))
  return(temp)
}

# If number of marriage years extracted is consistent
# with number of partners (in case of errors in regex code)

test <- married.folk %>% filter(wbells!="")
test$wbells <- gsub("^,|,$","",test$wbells)
num.partners <- do.call(cbind,lapply(test[,c("pgender","wbells")], commas))
test <- test[num.partners[,1]==num.partners[,2],]
test <- data.frame(splitstackshape::cSplit(test,c("wbells","pgender"),direction="long"))

test$Nationality <- unlist(lapply(test$Nationality,paste0,collapse=","))

filter1 <- plyr::count(test$Nationality)
identities <- as.character(filter1[filter1$freq>10,"x"])

test <- test[test$Nationality %in% c("American","Canadian","British"),]

freq.table <- test %>% 
  filter(pgender!=0,wbells>1950) %>%
  select(Nationality,wbells) %>%
  group_by(Nationality,wbells) %>%
  count()

ggplot(freq.table,aes(x=wbells,y=n)) + 
  geom_line(aes(color=Nationality),size=1.2,linetype=1) +
  scale_x_continuous(breaks=seq(1950,2016,4)) + 
  labs(list(title = "Same-Sex Marriages", x = "Wedding Bells", y = "Instances"))

test[test$wbells==2008 & test$Nationality=="American" & test$pgender==1,c("Name","Sex","Notable.as")]

hetero.sexual <- married.folk[commas(married.folk$pgender)>0,]
hetero.sexual <- hetero.sexual %>% 
  filter(substr(pgender,nchar(pgender),nchar(pgender))=="1") %>%
  filter(grepl("0",pgender))
paste(nrow(hetero.sexual)," / ",nrow(married.folk), " = ", round(nrow(hetero.sexual)/nrow(married.folk),2),sep="")

### ALTERNATIVE TO SLOW CODE CHUNK 

temp <- urls[1] %>% read_html %>% html_nodes("table")
indx <- grep("wikitable sortable",temp)

getLinks <- function() {
  links <- character()
  list(a = function(node, ...) {
    links <<- c(links, xmlGetAttr(node, "href"))
    node
  },
  links = function() links)
}
h1 <- getLinks()

temp <- htmlParse(temp[[indx]], handlers = h1)
url.lnks <- h1$links()
url.lnks <- unique(url.lnks)
url.lnks <- url.lnks[grep("#",url.lnks) + 1]
url.lnks <- url.lnks[!grepl("#",url.lnks)]
url.lnks <- url.lnks[!is.na(url.lnks)]
head(url.lnks)