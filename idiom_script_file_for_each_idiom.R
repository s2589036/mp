library(plyr)
library(stringr)

#=====================================================================================================================================
#=============================================================ONLY ONE QUERY==========================================================
#=====================================================================================================================================
datalist = list()

for (i in 1:143) {
  filename = paste("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\idioms_sonar\\without_verb\\",i,".csv",sep="")
  dat <- read.csv(filename)
  dat$idiom_id <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat # add it to your list
}
idioms = do.call(rbind, datalist)



#=====================================================================================================================================
#============================COMBINE QUERY FILE OF STATIC PART WITH A SEARCH FOR VERB FORM IN CONTEXTS================================
#=====================================================================================================================================



findidioms <<- function(i,verbformspattern){
  filename <- paste("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\idioms_sonar\\with_verb\\",i,".csv",sep="")
  idiomstatic <- read.csv(filename)
  idiomstatic <- idiomstatic[c(1,2,3,4,5,6,7,8,32)]
  colnames(idiomstatic) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id")
  idiomstatic$sentenceid <- paste(idiomstatic$doc_id, word(idiomstatic$xml_id,1,sep = ".w."),sep="-")
  correctidioms <<- rbind(idiomstatic[grep(verbformspattern, idiomstatic$right_context),],idiomstatic[grep(verbformspattern, idiomstatic$left_context),])
  }

onderdepethouden <- findidioms(144,"\\bhouden\\s|\\bgehouden\\s|\\bhoudt\\s|\\bhielden\\s")
bovenwaterhalen <- findidioms(145,"\\bhalen\\s|\\bhalen\\s|\\bhalen\\s|\\bhalen\\s|\\bhalen\\s")



lala <- rbind(idiomstatic[grep(verbformspattern, idiomstatic$right_context),])

              grep(verbformspattern, idiomstatic$right_context,value=TRUE)



#======================================================================================================================================


#remove empty columns
idioms <- idioms[c(1,2,3,4,5,6,7,8,32)]

#add id
idioms$id <- seq.int(nrow(idioms))


#add column names
colnames(idioms) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id","id")

#add doc_type
idioms$doc_type <- substr(idioms$doc_id,1,8)

#add doc_type_name: doc_type with a meaningful name: e-newsletters (WR-P-P-E) and and 
idioms$doc_type_name <- revalue(idioms$doc_type, c("WR-P-E-A"="discussion lists","WR-P-E-C"="e-magazines","WR-P-E-E"="E-newsletters","WR-P-E-F"="press releases","WR-P-E-G"="subtitles","WR-P-E-H"="teletext pages","WR-P-E-I"="web sites","WR-P-E-J"="wikipedia","WR-P-E-K"="blogs","WR-P-E-L"="tweets","WR-P-P-B"="books","WR-P-P-C"="brochures","WR-P-P-D"="newsletters","WR-P-P-E"="guides manuals","WR-P-P-F"="legal texts","WR-P-P-G"="newspapers","WR-P-P-H"="periodicals magazines","WR-P-P-I"="policy documents","WR-P-P-J"="proceedings","WR-P-P-K"="reports","WR-U-E-A"="chats","WR-U-E-D"="sms","WR-U-E-E"="written assignments","WS-U-E-A"="auto cues","WS-U-T-B"="texts for the visually impaired"))

#put id up front
idioms <- idioms[,c(10,11,12,1,2,3,4,5,6,7,8,9)]

#remove CGN-annotations: IN NEW VERSION NOT IN THE CSV (EXCLUDED VIA OPENSONAR), SO THIS IS NOT NEEDED ANYMORE
#idioms <- idioms[!grepl("CGN document", idioms$doc_name),]

idioms$idiom_lemma <- tolower(idioms$idiom_lemma)
