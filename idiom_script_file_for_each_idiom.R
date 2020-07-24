library(plyr)
library(stringr)

#=====================================================================================================================================
datalist = list()

for (i in 1:2) {
  filename = paste("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\idioms_sonar\\",i,".csv",sep="")
  
  dat <- read.csv(filename)
  dat$idiom_id <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat # add it to your list
}
idioms = do.call(rbind, datalist)

#=====================combine two query files (one with verb, other with idiom part) MAKE FUNCTION FROM THIS==========================
library(stringr)

idiom1a = read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\idioms_sonar\\1a.csv")
idiom1a <- idiom1a[c(1,2,3,4,5,6,7,8,32)]
colnames(idiom1a) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id")
idiom1a$sentenceid = paste(idiom1a$doc_id, word(idiom1a$xml_id,1,sep = ".w."),sep="-")

idiom1b = read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\idioms_sonar\\1b.csv")
idiom1b <- idiom1b[c(1,2,3,4,5,6,7,8,32)]
colnames(idiom1b) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id")
idiom1b$sentenceid = paste(idiom1b$doc_id, word(idiom1b$xml_id,1,sep = ".w."),sep="-")

data <- intersect(idiom1a$sentenceid,idiom1b$sentenceid)

idiom1a1b <- idiom1a[FALSE,]
for(i in 1:length(data)){
  idiom1a1b <- rbind(idiom1a1b,subset(idiom1a,idiom1a$sentenceid == data[i]))
}


idiom_static = read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\idioms_sonar\\with_verb\\onder_de_pet.csv")
idiom_static <- idiom_static[c(1,2,3,4,5,6,7,8,32)]
colnames(idiom_static) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id")
idiom_static$sentenceid = paste(idiom_static$doc_id, word(idiom_static$xml_id,1,sep = ".w."),sep="-")

idiom_true <- idiom_static[grepl("houd|houdt|gehouden|hield", idiom_static$right_context) || grepl("houd|houdt|gehouden|hield", idiom_static$left_context),]

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
