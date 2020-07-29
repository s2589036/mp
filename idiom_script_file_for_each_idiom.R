library(plyr)
library(stringr)

#=====================================================================================================================================
#=============================================================ONLY ONE QUERY==========================================================
#=====================================================================================================================================
datalist = list()

for (i in 1:130) {
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
  #idiomstatic$verb <- verbforms[1] <- add this in order to put the verb in crosstable as well (concatenated to static part)
  idiomstatic
  }

allidiomstype2 <- data.frame()
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
allidiomstype2 <- rbind(allidiomstype2, findidioms(151,c("spugen","spuug","spuugt","spuugde","spuugden","spoog","spogen","gespuugd",
                                                         "spuwen","spuw","spuwt","spuwde","spuwden","gespuwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(152,c("staan","sta","staat","stond","stondden","gestaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(153,c("strijken","strijk","strijkt","streek","streken","gestreken")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(154,c("toveren","tover","tovert","toverde","toverden","getoverd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(155,c("trekken","trek","trekt","trok","trokken","getrokken")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(156,c("trekken","trek","trekt","trok","trokken","getrokken")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(157,c("vallen","val","valt","viel","vielen","gevallen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(158,c("vallen","val","valt","viel","vielen","gevallen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(159,c("vechten","vecht","vocht","vochten","gevochten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(160,c("wippen","wip","wipt","wipte","wipten","gewipt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(161,c("zitten","zit","zat","zaten","gezeten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(162,c("zetten","zet","zette","gezet")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(163,c("zweten","zweet","zweette","zweetten","gezweet")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(164,c("kijken","kijk","kijkt","keek","keken","gekeken")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(165,c("spelen","speel","speelt","speelde","speelden","gespeeld")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(166,c("vatten","vat","vatte","gevat")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(167,c("trappen","trap","trapt","trapte","trapten","getrapt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(168,c("trouwen","trouw","trouwt","trouwde","trouwden","getrouwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(169,c("zitten","zit","zat","zaten","gezeten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(170,c("krijgen","krijg","krijgt","kreeg","kregen","gekregen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(171,c("brengen","breng","brengt","bracht","brachten","gebracht")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(172,c("doen","doe","doet","deed","deden","gedaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(173,c("hebben","heb","hebt","heeft","had","hadden","gehad")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(174,c("houden","houd","hou","houdt","hield","hielden","gehouden")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(175,c("houden","houd","hou","houdt","hield","hielden","gehouden",
                                                         "hebben","heb","hebt","heeft","had","hadden","gehad")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(176,c("houden","houd","hou","houdt","hield","hielden","gehouden",
                                                         "hebben","heb","hebt","heeft","had","hadden","gehad")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(177,c("kloppen","klop","klopt","klopte","klopten","geklopt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(178,c("eten","eet","at","aten","gegeten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(179,c("slaan","sla","slaat","sloeg","sloegen","geslaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(180,c("staan","sta","staat","stond","stondden","gestaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(181,c("staan","sta","staat","stond","stondden","gestaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(182,c("stoten","stoot","stootte","stootten","gestoten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(183,c("gaan","ga","gaat","ging","gingen","gegaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(184,c("vertrouwen","vertrouw","vertrouwt","vertrouwde","vertrouwden","vertrouwd",
                                                         "toevertrouwen","toevertrouw","toevertrouwt","toevertrouwde","toevertrouwden","toevertrouwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(185,c("maken","maak","maakt","maakte","maakten","gemaakt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(186,c("lopen","loop","loopt","liep","liepen","gelopen")))


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
idioms <- idioms[,c(12,11,10,13,14,1,2,3,4,5,6,7,8,9)]

#remove CGN-annotations: IN NEW VERSION NOT IN THE CSV (EXCLUDED VIA OPENSONAR), SO THIS IS NOT NEEDED ANYMORE
#idioms <- idioms[!grepl("CGN document", idioms$doc_name),]

idioms$idiom_lemma <- tolower(idioms$idiom_lemma)

idioms$sentenceid <- paste(idioms$doc_id, word(idioms$xml_id,1,sep = ".w."),sep="-")


#================================================================================================================================================================
#==================================================================================old===========================================================================
#================================================================================================================================================================



counts_per_collection <- ddply(idioms, .(doc_type_name), nrow)

counts_per_idiom <- ddply(idioms, .(idiom_id), nrow)

counts_per_idiom_collection <- ddply(idioms, .(idiom_id, doc_type_name), nrow)

#FUNCTION OF JACOLIEN, MERG THIS ======================================

mostfreq <- function(x){
  tab <- sort(table(x))
  return(names(tail(tab,1)))
}

mostfreq <- function(x){
  tab <- sort(table(x))
  return(paste(names(tab[tab==max(tab)]), collapse=";"))
}

findlemmas <- ddply(idioms, "idiom_id", summarise,
                    most_common_lemma=mostfreq(idiom_lemma))

#========================================================================




idioms <- merge(idioms, findlemmas, by="idiom_id", all.x=TRUE)
# maybe now order idioms:
idioms <- idioms[order(idioms$id),]


cross_table <- as.data.frame.matrix(addmargins(table(idioms$most_common_lemma,idioms$doc_type_name),))
write.csv(cross_table,"G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\cross_table.csv")
