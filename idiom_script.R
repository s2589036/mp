#=====================================================================================================================================
#=============================================================LOAD PACKAGES===========================================================
#=====================================================================================================================================


dev.off()
#install.packages("spacyr", INSTALL_opts = '--no-lock')
#install.packages("mgcv")
#install.packages("itsadug")
#install.packages("randomcoloR")
#install.packages("factoextra")
library(plyr)
library(stringr)
library(qdap)
library(spacyr)
library(mgcv)
library(itsadug)
library(ggplot2)
library(randomcoloR)
library(ggpubr)
library(tidyr)
library(factoextra)
library(dplyr)


setwd("G:/Mijn Drive/Studie informatiekunde/master/master project/project")

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16))
#=========================================================================================================================================
#=========================================================================================================================================
#=========================================================================================================================================
#===============================================================IDIOM ANALYSIS============================================================
#=========================================================================================================================================
#=========================================================================================================================================
#=========================================================================================================================================


#=====================================================================================================================================
#=============================================================IMPORT IDIOMS===========================================================
#=====================================================================================================================================
datalist = list()

#12,15,74 verb added (N=3)
#1, 31,127,129 removed (1: regen in de drup: problem with corpus; rest too literal )
for (i in(1:130)[c(-1,-12,-15,-31,-74,-127,-129)]) {
  dat <- read.csv(paste("idioms_sonar\\after_changes\\without_verb\\",i,".csv",sep=""),encoding="UTF-8")
  dat$with_verb <- 0
  dat$idiom_id <- i  # maybe you want to keep track of which iteration produced it?
  dat$verb <- ""
  datalist[[i]] <- dat # add it to your list
}

allidiomstype1 = do.call(rbind, datalist)


#==========================================================================================================
#===========COMBINE QUERY FILE OF STATIC PART WITH A SEARCH FOR VERB FORM IN CONTEXTS======================
#==========================================================================================================

findidioms <- function(i,verbforms){
  verbformspattern <- paste("\\b",paste(verbforms, collapse = "\\b|\\b"),"\\b",sep="")
  filename <- paste("idioms_sonar\\after_changes\\with_verb\\",i,".csv",sep="")
  idiomstatic <- read.csv(filename,encoding="UTF-8")
  idiomstatic <- idiomstatic[ with(idiomstatic,  grepl(verbformspattern, left_context)  | grepl(verbformspattern, right_context)  ) , ]
  idiomstatic$with_verb <- 1
  idiomstatic$idiom_id <- i
  idiomstatic$verb <- verbforms[1] #<- add this in order to put the verb in crosstable as well (concatenated to static part)
  idiomstatic
}

#183,186 removed
allidiomstype2 <- data.frame()
allidiomstype2 <- rbind(allidiomstype2, findidioms(12,c("dragen","draag","draagt","droeg","droegen", "gedragen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(15,c("gaan","ga","gaat","ging","gingen","gegaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(74,c("lopen","loop","loopt","loopte","loopten","gelopen")))
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
#allidiomstype2 <- rbind(allidiomstype2, findidioms(183,c("gaan","ga","gaat","ging","gingen","gegaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(184,c("vertrouwen","vertrouw","vertrouwt","vertrouwde","vertrouwden","vertrouwd",
                                                         "toevertrouwen","toevertrouw","toevertrouwt","toevertrouwde","toevertrouwden","toevertrouwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(185,c("maken","maak","maakt","maakte","maakten","gemaakt")))
#allidiomstype2 <- rbind(allidiomstype2, findidioms(186,c("lopen","loop","loopt","liep","liepen","gelopen")))

idioms <- rbind(allidiomstype1,allidiomstype2)
#this function works but the annotation of pos_head is often wrong
idioms$amountofnouns <- str_count(idioms$pos_head,"N")-str_count(idioms$pos_head,"VNW")

#remove empty columns
idioms <- idioms[c(1,2,3,4,5,6,7,8,32,35,36,37,38)]

#add id
idioms$id <- seq.int(nrow(idioms))

#add column names
colnames(idioms) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id","with_verb","idiom_id","verb","amountofnouns","id")

#add doc_type
idioms$doc_type <- substr(idioms$doc_id,1,8)

#add doc_type_name: doc_type with a meaningful name: e-newsletters (WR-P-P-E) and and 
idioms$doc_type_name <- revalue(idioms$doc_type, c("WR-P-E-A"="discussion lists","WR-P-E-C"="e-magazines","WR-P-E-E"="E-newsletters","WR-P-E-F"="press releases","WR-P-E-G"="subtitles","WR-P-E-H"="teletext pages","WR-P-E-I"="websites","WR-P-E-J"="wikipedia","WR-P-E-K"="blogs","WR-P-E-L"="tweets","WR-P-P-B"="books","WR-P-P-C"="brochures","WR-P-P-D"="newsletters","WR-P-P-E"="guides manuals","WR-P-P-F"="legal texts","WR-P-P-G"="newspapers","WR-P-P-H"="periodicals magazines","WR-P-P-I"="policy documents","WR-P-P-J"="proceedings","WR-P-P-K"="reports","WR-U-E-A"="chats","WR-U-E-D"="sms","WR-U-E-E"="written assignments","WS-U-E-A"="auto cues","WS-U-T-B"="texts for the visually impaired"))

#change order of columns
#idioms <- idioms[,c(12,11,10,13,14,1,2,3,4,5,6,7,8,9)]

#remove CGN-annotations: IN NEW VERSION NOT IN THE CSV (EXCLUDED VIA OPENSONAR), SO THIS IS NOT NEEDED ANYMORE
#idioms <- idioms[!grepl("CGN document", idioms$doc_name),]

idioms$idiom_lemma <- tolower(idioms$idiom_lemma)

idioms$sentenceid <- paste(idioms$doc_id, word(idioms$xml_id,1,sep = ".w."),sep="-")

idioms <- idioms[idioms$doc_type_ != "auto cues",]

#FUNCTION OF JACOLIEN, MERG THIS ======================================

mostfreq <- function(x){
  tab <- sort(table(x))
  return(names(tail(tab,1)))
}

#mostfreq <- function(x){
#  tab <- sort(table(x))
#  return(paste(names(tab[tab==max(tab)]), collapse=";"))
#}

most_common <- ddply(idioms, "idiom_id", summarise,
                     most_common_lemma=paste(mostfreq(idiom_found),mostfreq(verb)))

#remove trailing space from most_common_lemma (added when adding optional verb)
most_common$most_common_lemma <- trimws(most_common$most_common_lemma)


idioms <- merge(idioms, most_common, by="idiom_id", all.x=TRUE)

idioms$idiom_length_orig <- word_count(idioms$most_common_lemma)
#with_verb = 1 if verb is included and 0 if no verb is included, this is added to the amount of words in the idiom_lemma
idioms$idiom_length_this <- word_count(idioms$idiom_lemma) + idioms$with_verb

#order idioms:
idioms <- idioms[order(idioms$id),]

id_most_common <- unique(data.frame(idioms$idiom_id,idioms$most_common_lemma))


#=====================================================================================================================================
#==========================================================ANALYZE IDIOM FREQS========================================================
#=====================================================================================================================================

counts_per_collection <- ddply(idioms, .(doc_type_name), nrow)
counts_per_idiom <- ddply(idioms, .(idiom_id), nrow)

#=====================================================================================================================================
#=============================================================ADD COLLECTION SIZES====================================================
#=====================================================================================================================================

texttype <- c("written assignments","policy documents","legal texts","books","subtitles","guides manuals","websites","reports","sms","chats","brochures","texts for the visually impaired","proceedings","press releases","discussion lists","teletext pages","e-magazines","newspapers","tweets","periodicals magazines","wikipedia","blogs","newsletters")
tokenfreq <- c(357947,8711551,10689681,26184781,28209846,236099,3111589,2218223,723876,11873434,1213382,675082,314025,332795,57070554,448865,8626248,211669748,23197211,93058924,23001184,139765,35446)
freqdf <- data.frame(texttype,tokenfreq)
tfreqdf <- t(freqdf)
colnames(tfreqdf) <- texttype

#=====================================================================================================================================
#=============================================================ADD AVG DOC LENGTHS ====================================================
#=====================================================================================================================================
avg_doc_length <- read.csv("results/avg_doc_length_new_media_correct.csv",header=TRUE,sep=";")

#=====================================================================================================================================
#==============================================================MAKE CROSS TABLES======================================================
#====================================SOMETHING WENT WRONG WITH PROP IDIOM FREQS HERE, USE THE NEW ONES!===============================
#=====================================================================================================================================

#make cross table
#cross_table <- as.data.frame.matrix(table(idioms$most_common_lemma,idioms$doc_type_name),)

# #add sums and write to file
# cross_table_print <- as.data.frame.matrix(addmargins(table(idioms$most_common_lemma,idioms$doc_type_name),))
# 
# write.csv(cross_table_print,"results\\cross_table.csv")
# 
# #add sizes of collections in order to calculate relative idiom frequencies 
# tcross_table <- as.data.frame(t(cross_table))
# tcross_table$texttype <- row.names(tcross_table)
# 
# sort(tcross_table$texttype)
# sort(freqdf$texttype)
# 
# prop_cross <- merge(tcross_table, freqdf,by.x="texttype", by.y="texttype")
# #tcross_table <- tcross_table[-179]
# 
# sort(prop_cross$texttype)
# 
# #tpropcross: calculate relative idiom frequencies (per million words)
# #prop_cross_million <- (prop_cross[2:179]/prop_cross$tokenfreq)*100000000
# 
# #tpropcross: calculate relative idiom frequencies (per 100 million words)
# prop_cross[2:179] <- round((prop_cross[2:179]/prop_cross$tokenfreq)*100000000) #rounded freq per 100,000,000 words
# str(prop_cross)
# 
# tprop_cross <- t(prop_cross)
# 
# colnames(tprop_cross) <- tprop_cross[1,]
# tprop_cross <- tprop_cross[-1,]
# 
# tprop_cross[, c(1:23)] <- sapply(tprop_cross[, c(1:23)], as.numeric)
# 
# write.csv(tprop_cross,"results\\cross_table_per_100_million_tokens_rounded.csv")


#=====================================================================================================================================
#==============================================================ADD IDIOM FEATURES=====================================================
#=====================================================================================================================================

fixedness_nv <- read.table("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\fixedness\\nounverbidioms-id-out-lowered-dict-set-of-idlists-after.txt", sep="\t")
fixedness_nn <- read.table("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\fixedness\\twonounidioms-id-out-lowered-dict-set-of-idlists-after.txt", sep="\t")
fixedness <- rbind(fixedness_nn,fixedness_nv)


idiom_features <- merge(most_common, fixedness, by.x="idiom_id" ,by.y="V1")

idiom_features$V2 <- c()
idiom_features$V3 <- c()
colnames(idiom_features) <- c("id","idiom","fixedness")


#idiom_features <- merge(idiom_features,tprop_cross,by.x="idiom",by.y=0)

sprenger_feat <- read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\script files\\sprenger_data_own_ids.csv")
idiom_features <- merge(idiom_features,sprenger_feat,by.x="id",by.y="ID")
#idiom_features[, c(2:27)] <- sapply(idiom_features[, c(2:27)], as.numeric)

#=====================================================================================================================================
#==========================================================ANALYZE IDIOM FEATURES=====================================================
#=====================================================================================================================================

hist(idiom_features$fixedness)
qqnorm(idiom_features$fixedness)
qqline(idiom_features$fixedness)


hist(idiom_features$est.link)
qqnorm(idiom_features$est.link)
qqline(idiom_features$est.link)


#most_common_pos_head <- ddply(idioms, "idiom_id", summarise,
#                              most_common_pos_head=paste(mostfreq(pos_head)))

#idiom_features <- merge(idiom_features,most_common_pos_head,by.x="id",by.y="idiom_id")
#idiom_features$idiom_length <- sapply(strsplit(idiom_features$idiom, " "), length)

#=========================================================================================================================================
#===============================================================TEXT TYPE ANALYSIS========================================================
#=========================================================================================================================================

texttype <- c("discussion lists","e-magazines","e-newsletters","press releases","subtitles","teletext pages","websites","wikipedia","blogs","books","brochures","newsletters","guides manuals","legal texts","newspapers","periodicals magazines","policy documents","proceedings","reports","written assignments","texts for the visually impaired","tweets","chats","sms")
f = c(50.01945724800984, 66.65475989056685, 73.02779864763335, 73.1523267854659, 40.52451228840621, 58.311562135295404, 73.18003543837753, 70.45651852836734, 66.94659427043234, 52.230419066220136, 70.9003326020955, 63.39527680991095, 66.1826968194983, 77.15642454427714, 62.55577194192853, 63.60886727578005, 63.11894123144769, 66.61448814509454, 66.98679742196146, 64.34370590192398, 58.321571417219445,54.36,47.78,43.48)
ttr_lemmafreqlist <- c(0.013303222135847852,0.026542478259377656,0.3813249869587898,0.057401703751558765,0.009689241125244002,0.055542312276519665,0.024249025176525562,0.03766366983543108,0.11896397524415984,0.010572668146432082,0.04049095832969337,0.11157505443049301,0.03752663077776695,0.014107998171320548,0.010236219490373278,0.01398963091384981,0.010867410407170893,0.04554732903431256,0.024833391412856145,0.01861169390999226,0.046154393095949826,0.04369887397239263,0.018766516914988537,0.04383623714558847)
ttr_lemmaposfreqlist <- c(0.017683684714840903,0.03366492593303601,0.42618675013041213,0.07494102976306735,0.013140093001571153,0.07565526383210988,0.03161953587057931,0.04545992067190976,0.15152577540872178,0.0148120772902397,0.055242289732334915,0.14822989054251542,0.052668583941482175,0.017698750785921487,0.012530770339462963,0.018502535017490638,0.015308295847662488,0.06380383727410238,0.034785952539487686,0.02633071376488698,0.06540242518686619,0.05544851922069425,0.028243387717487627,0.05866198078123878)
ttr_wordfreqlist <- c(0.01503489926570883,0.03065284002963977,0.4298382889932186,0.07136525488664193,0.011863730131671048,0.07209071769908547,0.029865126788917173,0.03938271177692418,0.1471183772761421,0.01305071063989422,0.052791289140600405,0.1453666974857586,0.050038331377938915,0.016012825827075663,0.010854456159696471,0.01553254580936268,0.013587477132372868,0.060657590956134065,0.03182322065905908,0.024573470374105663,0.062094678868641145,0.04710152440308449,0.021423962098917636,0.053956755024341184)

textfeats <- data.frame(texttype,f,ttr_lemmafreqlist,ttr_lemmaposfreqlist,ttr_wordfreqlist)


#=====================================================================================================================================
#============================================= NEW: CALCULATE PROPFREQIDIOMS (with zeros) =========================================
#=====================================================================================================================================

cross_table <- as.data.frame.matrix(table(idioms$most_common_lemma,idioms$doc_type_name),)
cross_table$idiom = row.names(cross_table)
withzeros <- pivot_longer(cross_table[1:24], cols=1:23, names_to = "texttype", values_to = "freq")
withzeros <- merge(withzeros,most_common,by.x="idiom",by.y="most_common_lemma")

withzeros = merge(withzeros,freqdf,by.x="texttype",by.y="texttype")
names(withzeros)[names(withzeros) == 'tokenfreq'] <- 'collsize'
withzeros$relfreq = (withzeros$freq/withzeros$collsize)*100000000

withzeros<-merge(withzeros,sprenger_feat[,c("ID","est")],by.x="idiom_id",by.y="ID")
withzeros<-merge(withzeros,textfeats[,c("texttype","f","ttr_wordfreqlist")],by.x="texttype",by.y="texttype")
withzeros<-merge(withzeros,idiom_features[,c("id","fixedness")],by.x="idiom_id",by.y="id")
withzeros<-merge(withzeros,avg_doc_length[,c("doc_type_name","meanlength")],by.x="texttype",by.y="doc_type_name")

names(withzeros)[names(withzeros) == 'ttr_wordfreqlist'] <- 'ttr'

#=====================================================================================================================================
#============================================= NEW: CALCULATE PROPFREQIDIOMS (without zeros) =========================================
#=====================================================================================================================================

gooddata = ddply(idioms,.(idiom_id,doc_type_name),nrow)
gooddata = merge(id_most_common,gooddata,by.x="idioms.idiom_id",by.y="idiom_id")
colnames(gooddata)<-c("idiom_id","most_common_lemma","texttype","absfreq")
gooddata = merge(gooddata,freqdf,by.x="texttype",by.y="texttype")
names(gooddata)[names(gooddata) == 'tokenfreq'] <- 'collsize'
gooddata$relfreq = (gooddata$absfreq/gooddata$collsize)*100000000

gooddata<-merge(gooddata,sprenger_feat[,c("ID","est")],by.x="idiom_id",by.y="ID")
gooddata<-merge(gooddata,textfeats[,c("texttype","f","ttr_wordfreqlist")],by.x="texttype",by.y="texttype")
gooddata<-merge(gooddata,idiom_features[,c("id","fixedness")],by.x="idiom_id",by.y="id")
gooddata<-merge(gooddata,avg_doc_length[,c("doc_type_name","meanlength")],by.x="texttype",by.y="doc_type_name")

names(gooddata)[names(gooddata) == 'ttr_wordfreqlist'] <- 'ttr'

#=========================================================================================================================================
# MAKE TEXTFEATS DF
#=========================================================================================================================================

textfeats <- merge(textfeats,counts_per_collection,by.x="texttype",by.y="doc_type_name",all.x=TRUE)
textfeats <- merge(textfeats,avg_doc_length,by.x="texttype",by.y="doc_type_name")
textfeats$relfreq = (textfeats$V1/textfeats$collsize)*100000000
textfeats$logabsfreq <- log(textfeats$V1)
textfeats$tokenfreq <- textfeats$amountofword

names(textfeats)[names(textfeats) == 'amountofwords'] <- 'collsize'

#=========================================================================================================================================
# PCA ATTEMPT
#=========================================================================================================================================


alldata.pca <- prcomp(alldata[,c(3:15,18:27,29,31)], center = TRUE,scale. = TRUE)
alldata.pca2 <- princomp(alldata[,c(3:15,18:27,29,31)], cor = TRUE,scores = TRUE)

summary(alldata.pca)
fviz_eig(alldata.pca,label=TRUE)



#=====================================================================================================================================
#=========================================================== CLUSTER TEXT TYPES ======================================================
#=====================================================================================================================================

#TODO: ADD ALL FEATURES AGAIN

fviz_nbclust(textfeats[,2:12], kmeans, method = "gap_stat")

set.seed(123)
km.res <- kmeans(textfeats[,2:12], 8, nstart = 100)
# Visualize

fviz_cluster(km.res, data = textfeats[,2:12],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Compute hierarchical clustering
res.hc <- textfeats[,2:12] %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

res.hc$labels <- textfeats$texttype

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 8, # Cut in 8 groups (optimal # of clusters)
          cex = 0.9, # label size
          #k_colors = rainbow(11),
          color_labels_by_k = TRUE # color labels by groups
          #rect = TRUE, # Add rectangle around groups
)

#=====================================================================================================================================
#=======================================================MAKE NEW STATISTICAL MODELS====================================================
#=====================================================================================================================================

gooddata$idiom_id <- as.factor(gooddata$idiom_id)
gooddata$idiom <- as.factor(gooddata$most_common_lemma)
gooddata$texttype <- as.factor(gooddata$texttype)

qqnorm(gooddata$absfreq)

gooddata$logfreq <- log(gooddata$relfreq)
qqnorm(gooddata$logfreq)
qqline(gooddata$logfreq)

m0 <- bam(logfreq ~ s(f) + s(fixedness) + s(est) + ti(f,est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)

plot(m0,scale=0)
summary(m0)
fvisgam(m0,view=c("f","est"),dec=1)
plot_smooth(m0,view="f",cond=list(est=4))
plot_smooth(m0,view="f",cond=list(est=2.5),add=TRUE,col=2)

pvisgam(m0,select=4,view=c("f","est"),dec=1,too.far = 0.03)
plot(m0,scale=0,select=4)
summary(m0)

gam.check(m0)

m1 <- bam(logfreq ~ s(f) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
m2 <- bam(logfreq ~ s(f) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
m3 <- bam(logfreq ~ s(est) + s(fixedness) + s(idiom, bs="re")+s(texttype, bs="re"),data=gooddata)
m4 <- bam(logfreq ~ s(fixedness) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
m5 <- bam(logfreq ~ s(est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
m6 <- bam(logfreq ~ s(est) + s(ttr) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
m7 <- bam(logfreq ~ s(f) + s(ttr) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
m8 <- bam(logfreq ~ s(f) + s(est) + s(meanlength) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
#m9 <- bam(logfreq ~ s(f) + s(est) + + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata[gooddata$doc_type_name!="tweets"&&gooddata$doc_type_name!="chats"&&gooddata$doc_type_name!="sms",])
m10 <- bam(logfreq ~ s(ttr) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)


plot_smooth(m7, view="f")
gam.check(m7)
plot(resid(m7))
hist(resid(m7))

mean(gooddata[gooddata$f > 77 & gooddata$f < 78,]$relfreq,na.rm=TRUE)

AIC(m0,m1,m2,m3,m4,m5,m6,m7,m8,m10)

#=========================================================================================================================================
#PLOTS FOR PAPER/PRESENTATION
#=========================================================================================================================================
i=0

ggscatter(textfeats, x = "V1", y = "f", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Formality Score \n(Pearson)", xlab = "Idiom Frequency", ylab = "Formality Score")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "V1", y = "tokenfreq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Collection Size \n(Pearson)", xlab = "Idiom Frequency", ylab = "Collection Size")


i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "V1", y = "ttr_wordfreqlist", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Type-token Ratio (word level) \n(Pearson)", xlab = "Idiom Frequency", ylab = "Type-token Ratio")


i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "f", y = "ttr_wordfreqlist", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between F-score and Type-token Ratio (word level) \n(Pearson)", xlab = "F-score", ylab = "Type-token Ratio")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))


ggscatter(textfeats, x = "logabsfreq", y = "f", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between log Idiom Freq. and Formality Score \n(Pearson)", xlab = "Log Idiom Frequency", ylab = "Formality Score")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "logabsfreq", y = "tokenfreq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between log Idiom Freq. and Collection Size \n(Pearson)", xlab = "Log Idiom Frequency", ylab = "Collection Size")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

textfeats$logcollsize <- log(textfeats$collsize)

ggscatter(textfeats, x = "logabsfreq", y = "logcollsize", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between log Idiom Freq. and log Collection Size \n(Pearson)", xlab = "Log Idiom Frequency", ylab = "Log Collection Size")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "logabsfreq", y = "ttr_wordfreqlist", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between log Idiom Freq. and Type-token Ratio (word level) \n(Pearson)", xlab = "Idiom Frequency", ylab = "Type-token Ratio")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

plot(textfeats$f, textfeats$ttr_wordfreqlist, xlab="Formality Score", ylab="Type-Token Ratio", main="Type-token Ratio X Formality score")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))


ggscatter(textfeats, x = "V1", y = "collsize", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Collection Size\n(Pearson)", xlab = "Idiom Frequency", ylab = "Collection Size")

ggscatter(textfeats[textfeats$texttype!="newspapers",], x = "V1", y = "collsize", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Collection Size\n (Pearson) -- Newspapers excluded", xlab = "Idiom Frequency", ylab = "Collection Size")


ggscatter(textfeats, x = "V1", y = "collsize", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Collection Size\n(Pearson)", xlab = "Idiom Frequency", ylab = "Collection Size")


#======================================================

ggplot(data=textfeats, aes(x=reorder(texttype,f),y=f,fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Formality scores for all text types") +
  labs(x="Text type",y="F-score", fill="F-score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + My_Theme

ggplot(data=gooddata, aes(x=reorder(texttype,f),y=ttr, fill=f)) + 
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Type-token ratio for all text types") +
  labs(x="Text type",y="Type-token ratio") + My_Theme +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) )

ggplot(data=textfeats, aes(x=reorder(texttype,f),y=ttr_wordfreqlist,fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Type-token ratios for all text types") +
  labs(x="Text type",y="TTR", fill="F-score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + My_Theme

ggplot(textfeats,
       aes(y=log(meanlength), x=reorder(texttype, f),fill=f)) + geom_bar(stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2)) + ggtitle("Normalized average text size of each collection") + My_Theme

ggplot(idiom_features[idiom_features$fixedness<1.60,],
       aes(y=fixedness, x=reorder(idiom,fixedness))) + geom_bar(stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2)) + ggtitle("Lexical Fixedness") + 
  coord_cartesian(ylim=c(-1.1,3.1)) + My_Theme

ggplot(idiom_features[idiom_features$fixedness>1.60,],
       aes(y=fixedness, x=reorder(idiom,fixedness))) + geom_bar(stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2)) + ggtitle("Lexical Fixedness") + 
  coord_cartesian(ylim=c(-1.1,3.1)) 

#==================================

ggplot(gooddata,
       aes(y=absfreq, x=reorder(texttype, f))) + geom_bar(stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2)) + ggtitle("Absolute total frequency of idioms per collection") + My_Theme

ggplot(gooddata[gooddata$texttype!="written assignments",], 
       aes(y=absfreq, x=reorder(texttype, f))) + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text Type",y="Absolute freq.") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Absolute total frequency of idioms per collection\nWritten Assignments excluded") + My_Theme

ggplot(gooddata[gooddata$texttype!="written assignments",], 
       aes(y=relfreq, x=reorder(texttype, f), fill=ttr)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text Type",y="Relative freq.", fill ="TTR") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Relative total frequency of idioms per collection\nWritten Assignments excluded") + My_Theme

ggplot(gooddata[gooddata$texttype!="written assignments",], 
       aes(y=relfreq, x=reorder(texttype, f), fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text Type",y="Relative freq.", fill="F-score") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Relative total frequency of idioms per collection\nWritten Assignments excluded") + My_Theme

ggplot(gooddata[gooddata$texttype!="written assignments",], 
       aes(y=relfreq, x=reorder(texttype, relfreq,FUN = sum))) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text Type",y="Relative freq.", fill ="F-score") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Relative total frequency of idioms per collection\nWritten Assignments excluded") + My_Theme

ggplot(gooddata[gooddata$texttype!="written assignments",], 
       aes(y=relfreq, x=reorder(texttype, f))) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Relative total frequency of idioms per collection\nWritten Assignments excluded") + labs(fill="F-score") + My_Theme

ggplot(gooddata[gooddata$texttype!="written assignments",], 
       aes(y=relfreq, x=reorder(texttype, f), fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Relative total frequency of idioms per collection\nWritten Assignments excluded") + labs(fill="F-score") + My_Theme

ggplot(gooddata[gooddata$texttype!="written assignments",], 
       aes(y=relfreq, x=reorder(texttype, f), fill=ttr)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Relative total frequency of idioms per collection\nWritten Assignments excluded") + labs(fill="ttr") + My_Theme

ggplot(gooddata[gooddata$texttype!="written assignments",], 
       aes(y=log(relfreq), x=reorder(texttype, f), fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Normalized relative total frequency of idioms per collection\nWritten Assignments excluded") + labs(fill="F-score") + My_Theme


