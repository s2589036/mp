library(plyr)

#read data from file
idioms=read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\all_idioms.csv")


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

write.csv(idioms,"G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\all_idioms_for_python.csv", row.names = FALSE)

library(reticulate)

#The python-script makes an id-list with all idiom ids by checking the nouns, verbs and preps of the idiom lemma 

py <- py_run_file("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\add_ids.py")
idioms$idiom_id <- c(py$idlist)

write.csv(idioms,"G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\all_idioms_plus_idiom_id.csv", row.names = FALSE)

counts_per_collection <- ddply(idioms, .(doc_type_name), nrow)

counts_per_idiom <- ddply(idioms, .(idiom_lemma, idiom_id), nrow)

counts_per_idiom_collection <- ddply(idioms, .(idiom_id, doc_type_name), nrow)

#TODO: show per idiom_id but give the most frequent idiom_lemma as idiom_lemma


counts_per_collection_with_lemmas <- merge(counts_per_idiom_collection, counts_per_idiom, by = "idiom_id")

library(dplyr)

#dat2 <- counts_per_collection_with_lemmas %>% group_by(idiom_id) %>% summarise(val=paste(unique(idiom_lemma), collapse=","))

dat2 <- counts_per_collection_with_lemmas %>% group_by(idiom_id) %>% summarise(val=paste(unique(idiom_lemma), collapse=","))

good <- merge(counts_per_idiom_collection, dat2, by="idiom_id")


#dat2 <-  aggregate(.~idiom_id, counts_per_collection_with_lemmas, paste, collapse=",")

names(good) <- c("idiom_id","doc_type_name","idiom_id_in_doctype","idiom")




#WR-P-E-A - discussion lists
#WR-P-E-C - e-magazines
#WR-P-E-E - e-newsletters
#WR-P-E-F - press releases
#WR-P-E-G - subtitles
#WR-P-E-H - teletext pages
#WR-P-E-I - web sites
#WR-P-E-J - wikipedia
#WR-P-E-K - blogs
#WR-P-E-L - tweets

#WR-P-P-B - books
#WR-P-P-C - brochures
#WR-P-P-D - newsletters
#WR-P-P-E - guides manuals
#WR-P-P-F - legal texts
#WR-P-P-G - newspapers
#WR-P-P-H - periodicals magazines
#WR-P-P-I - policy documents
#WR-P-P-J - proceedings
#WR-P-P-K - reports

#WR-U-E-A - chats
##WR-U-E-D - sms
##WR-U-E-E - written assignments

#WS-U-E-A - auto cues

#WS-U-T-B - texts for the visually impaired



#-------------------------------------------- LOOK FOR POS-TAGS WITH FEATURES FOUND IN IDIOMS -----------------------------------------------------#

  


