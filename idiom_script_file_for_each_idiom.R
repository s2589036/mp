library(plyr)
library(stringr)

#=====================================================================================================================================
#=============================================================ONLY ONE QUERY==========================================================
#=====================================================================================================================================
datalist = list()

for (i in 1:129) {
  filename = paste("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\idioms_sonar\\without_verb\\",i,".csv",sep="")
  dat <- read.csv(filename)
  dat$with_verb <- 0
  dat$idiom_id <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat # add it to your list
}

allidiomstype1 = do.call(rbind, datalist)

#=====================================================================================================================================
#============================COMBINE QUERY FILE OF STATIC PART WITH A SEARCH FOR VERB FORM IN CONTEXTS================================
#=====================================================================================================================================

findidioms <- function(i,verbforms){
  verbformspattern <- paste("\\b",paste(verbforms, collapse = "\\b|\\b"),"\\b",sep="")
  filename <- paste("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\idioms_sonar\\with_verb\\",i,".csv",sep="")
  idiomstatic <- read.csv(filename)
  idiomstatic <- idiomstatic[ with(idiomstatic,  grepl(verbformspattern, left_context)  | grepl(verbformspattern, right_context)  ) , ]
  idiomstatic$with_verb <- 1
  idiomstatic$idiom_id <- i
  idiomstatic
  }

allidiomstype2 <- data.frame()
allidiomstype2 <- rbind(allidiomstype2, findidioms(130,c("maken","maak","maakt","maakte","maakten","gemaakt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(131,c("houden","houd","hou","houdt","hield","hielden","gehouden")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(132,c("halen","haal","haalt","haalde","haalden","gehaald")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(133,c("blijven","blijf","blijft","bleef","bleven","gebleven")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(134,c("doen","doe","doet","deed","deden","gedaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(135,c("draaien","draai","draait","draaide","draaiden","gedraaid")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(136,c("grijpen","grijp","grijptt","greep","grepen","gegrepen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(137,c("houden","houd","hou","houdt","hield","hielden","gehouden")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(138,c("helpen","help","helpt","hielp","hielpen","geholpen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(139,c("kloppen","klop","klopt","klopte","klopten","geklopt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(140,c("knopen","knoop","knoopt","knoopte","knoopten","geknoopt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(141,c("komen","kom","komt","kwam","kwamen","gekomen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(142,c("leven","leef","leeft","leefde","leefden","geleefd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(143,c("leren","leer","leert","leerde","leerden","geleerd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(144,c("lichten","licht","lichtte","lichtten","gelicht")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(145,c("lopen","loop","loopt","loopte","loopten","gelopen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(146,c("lopen","loop","loopt","loopte","loopten","gelopen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(147,c("nemen","neem","neemt","nam","namen","genomen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(148,c("roeien","roei","roeit","roeide","roeiden","geroeid","oproeien","oproei","oproeit","oproeit")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(149,c("schreeuwen","schreeuw","schreeuwt","schreeuwde","schreeuwden","geschreeuwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(150,c("spelen","speel","speelt","speelde","speelden","gespeeld")))
#(...) HIER VERDER GAAN

idioms <- rbind(allidiomstype1,allidiomstype2)


#======================================================================================================================================


#remove empty columns
idioms <- idioms[c(1,2,3,4,5,6,7,8,32,35,36)]

#add id
idioms$id <- seq.int(nrow(idioms))

#add column names
colnames(idioms) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id","with_verb","idiom_id","id")

#add doc_type
idioms$doc_type <- substr(idioms$doc_id,1,8)

#add doc_type_name: doc_type with a meaningful name: e-newsletters (WR-P-P-E) and and 
idioms$doc_type_name <- revalue(idioms$doc_type, c("WR-P-E-A"="discussion lists","WR-P-E-C"="e-magazines","WR-P-E-E"="E-newsletters","WR-P-E-F"="press releases","WR-P-E-G"="subtitles","WR-P-E-H"="teletext pages","WR-P-E-I"="web sites","WR-P-E-J"="wikipedia","WR-P-E-K"="blogs","WR-P-E-L"="tweets","WR-P-P-B"="books","WR-P-P-C"="brochures","WR-P-P-D"="newsletters","WR-P-P-E"="guides manuals","WR-P-P-F"="legal texts","WR-P-P-G"="newspapers","WR-P-P-H"="periodicals magazines","WR-P-P-I"="policy documents","WR-P-P-J"="proceedings","WR-P-P-K"="reports","WR-U-E-A"="chats","WR-U-E-D"="sms","WR-U-E-E"="written assignments","WS-U-E-A"="auto cues","WS-U-T-B"="texts for the visually impaired"))

#change order of columns
idioms <- idioms[,c(12,10,13,14,1,2,3,4,5,6,7,8,9)]

#remove CGN-annotations: IN NEW VERSION NOT IN THE CSV (EXCLUDED VIA OPENSONAR), SO THIS IS NOT NEEDED ANYMORE
#idioms <- idioms[!grepl("CGN document", idioms$doc_name),]

idioms$idiom_lemma <- tolower(idioms$idiom_lemma)

idioms$sentenceid <- paste(idioms$doc_id, word(idioms$xml_id,1,sep = ".w."),sep="-")