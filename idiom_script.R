dev.off()
library(plyr)
library(stringr)
library(qdap)

#install.packages("spacyr", INSTALL_opts = '--no-lock')
library(spacyr)

setwd("G:/Mijn Drive/Studie informatiekunde/master/master project/project")

#=====================================================================================================================================
#=============================================================ONLY ONE QUERY==========================================================
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




#=====================================================================================================================================
#============================COMBINE QUERY FILE OF STATIC PART WITH A SEARCH FOR VERB FORM IN CONTEXTS================================
#=====================================================================================================================================

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

#======================================================================================================================================


#remove empty columns
idioms <- idioms[c(1,2,3,4,5,6,7,8,32,35,36,37,38)]

#add id
idioms$id <- seq.int(nrow(idioms))

#add column names
colnames(idioms) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id","with_verb","idiom_id","verb","amountofnouns","id")

#add doc_type
idioms$doc_type <- substr(idioms$doc_id,1,8)

#add doc_type_name: doc_type with a meaningful name: e-newsletters (WR-P-P-E) and and 
idioms$doc_type_name <- revalue(idioms$doc_type, c("WR-P-E-A"="discussion lists","WR-P-E-C"="e-magazines","WR-P-E-E"="E-newsletters","WR-P-E-F"="press releases","WR-P-E-G"="subtitles","WR-P-E-H"="teletext pages","WR-P-E-I"="web sites","WR-P-E-J"="wikipedia","WR-P-E-K"="blogs","WR-P-E-L"="tweets","WR-P-P-B"="books","WR-P-P-C"="brochures","WR-P-P-D"="newsletters","WR-P-P-E"="guides manuals","WR-P-P-F"="legal texts","WR-P-P-G"="newspapers","WR-P-P-H"="periodicals magazines","WR-P-P-I"="policy documents","WR-P-P-J"="proceedings","WR-P-P-K"="reports","WR-U-E-A"="chats","WR-U-E-D"="sms","WR-U-E-E"="written assignments","WS-U-E-A"="auto cues","WS-U-T-B"="texts for the visually impaired"))

#change order of columns
#idioms <- idioms[,c(12,11,10,13,14,1,2,3,4,5,6,7,8,9)]

#remove CGN-annotations: IN NEW VERSION NOT IN THE CSV (EXCLUDED VIA OPENSONAR), SO THIS IS NOT NEEDED ANYMORE
#idioms <- idioms[!grepl("CGN document", idioms$doc_name),]

idioms$idiom_lemma <- tolower(idioms$idiom_lemma)

idioms$sentenceid <- paste(idioms$doc_id, word(idioms$xml_id,1,sep = ".w."),sep="-")



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



#================================================================================================================================================================
#====================================================================sample 50 rows for each idiom===============================================================
#===================================================================sampled THE IDIOMS WITH MISTAKES=============================================================
#================================================================================================================================================================


sampledidioms <- data.frame()

datalist = list()
for(i in c(12,74,145,172)){
  newidiom <- data.frame()
  dataframeidiom <- idioms[idioms$idiom_id==i,]
  if(nrow(dataframeidiom) > 50){
    newidiom <- dataframeidiom[sample(nrow(dataframeidiom),50),]
  }else{
    newidiom <- idioms[idioms$idiom_id==i,]
  }
  datalist[[i]]<-newidiom
  
}

sampledidioms <-do.call(rbind,datalist)

#write.csv(sampledidioms,"results\\sampled_idioms_after_changeslalalalalalala.csv") #DIT OPNIEUW GEDAAN EN ALLEEN VOOR DE VERANDERDE 
#QUERIES DE SAMPLED OCCURRENCES AANGEPAST

#================================================================================================================================================================
#===============================================================old sample output==============================================================
#================================================================================================================================================================

#ANALYSIS OF IDIOMATICITY OF IDIOMS IN ORDER TO CHANGE SOME QUERIES/EXCLUDE SOME IDIOMS
idioms_sample_out1 <- read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\sample_check_50_for_each_idiom_output1.csv",header = TRUE,sep=";")
idioms_sample_out1 <- merge(most_common, idioms_sample_out1, by.x="idiom_id" ,by.y="idiom_id", all.x = TRUE, all.y = TRUE)
idiomaticity_before <- as.data.frame(tapply(idioms_sample_out1$Idiomatic.R, idioms_sample_out1$most_common_lemma, mean))
#write.csv(idiomaticity_before,"results\\idiomaticity_before_GOOD_ONE.csv")
#================================================================================================================================================================
#==================================================================new sample output============================================================
#================================================================================================================================================================

#ANALYSIS OF IDIOMATICITY OF IDIOMS IN ORDER TO CHANGE SOME QUERIES/EXCLUDE SOME IDIOMS
idioms_sample_out2 <- read.csv("results\\sample_check_50_for_each_idiom_output2_after_changes.csv",header = TRUE,sep=";")
idioms_sample_out2 <- merge(most_common, idioms_sample_out2, by.x="idiom_id" ,by.y="idiom_id",all.y=FALSE)
idiomaticity_after <- as.data.frame(tapply(idioms_sample_out2$Idiomatic.R, idioms_sample_out2$most_common_lemma, mean))

both_idiomaticities <- merge(idiomaticity_before,idiomaticity_after,by.x=0,by.y=0,all.x=TRUE,all.y=TRUE)
colnames(both_idiomaticities) <- c("idiom","idiomaticity_before","idiomaticity_after")
write.csv(both_idiomaticities,"results\\both_idiomaticities.csv")

#================================================================================================================================================================
#==================================================================try to improve postags using spacy============================================================
#================================================================================================================================================================


spacy_initialize(model="nl", python_executable = "C:\\Users\niels\\AppData\\Local\\Programs\\Python\\Python37\\python.exe")


getpos <- function(x){
  str(spacy_parse(x)$pos)
}

idiomsub = idioms[1:10,]


pos_heads_spacy_df <- ddply(idiomsub, "sentenceid", summarise, 
                            pos_tag_spacy=getpos(idiom_lemma))

names(pos_heads_spacy_df)


help(tapply)

#================================================================================================================================================================
#==================================================================================old===========================================================================
#================================================================================================================================================================

counts_per_collection <- ddply(idioms, .(doc_type_name), nrow)

counts_per_idiom <- ddply(idioms, .(idiom_id), nrow)

counts_per_idiom_collection <- ddply(idioms, .(idiom_id, doc_type_name), nrow)
counts_per_idiom_collection <- merge(counts_per_idiom_collection,id_most_common,by.x="idiom_id",by.y="idioms.idiom_id")
colnames(counts_per_idiom_collection) <- c("idiom_id","collection","freq","lemma")

#========================================================================

texttype <- c("written assignments","policy documents","legal texts","books","subtitles","guides manuals","web sites","reports","sms","chats","brochures","texts for the visually impaired","proceedings","press releases","discussion lists","teletext pages","e-magazines","newspapers","tweets","periodicals magazines","wikipedia","blogs","newsletters")
tokenfreq <- c(357947,8711551,10689681,26184781,28209846,236099,3111589,2218223,723876,11873434,1213382,675082,314025,332795,57070554,448865,8626248,211669748,23197211,93058924,23001184,139765,35446)
freqdf <- data.frame(texttype,tokenfreq)
tfreqdf <- t(freqdf)
colnames(tfreqdf) <- texttype


#========================================================================

#make cross table
cross_table <- as.data.frame.matrix(table(idioms$most_common_lemma,idioms$doc_type_name),)

#add sums and write to file
cross_table_print <- as.data.frame.matrix(addmargins(table(idioms$most_common_lemma,idioms$doc_type_name),))
write.csv(cross_table_print,"results\\cross_table.csv")

#cross_table_sum <- rbind(cross_table,sum=colSums(cross_table))


#add sizes of collections in order to calculate relative idiom frequencies 
tcross_table <- as.data.frame(t(cross_table))

tcross_table$texttype <- row.names(tcross_table)

sort(tcross_table$texttype)
sort(freqdf$texttype)

prop_cross <- merge(tcross_table, freqdf,by.x="texttype", by.y="texttype")
#tcross_table <- tcross_table[-179]

sort(prop_cross$texttype)

#tpropcross: calculate relative idiom frequencies (per 100 million words)
prop_cross_new <- round((prop_cross[2:179]/prop_cross$tokenfreq)*100000000) #rounded freq per 100,000,000 words


tprop_cross <- as.data.frame.matrix(t(prop_cross),stringsAsFactors = FALSE)

colnames(tprop_cross) <- tprop_cross[1,]

tprop_cross <- tprop_cross[-1,]
tprop_cross <- tprop_cross[-183,]
tprop_cross[,1:19] <- sapply(tprop_cross[,1:19],as.numeric)

write.csv(tprop_cross,"results\\cross_table_per_100_million_tokens_rounded.csv")



fixedness_nv <- read.table("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\fixedness\\nounverbidioms-id-out-lowered-dict-set-of-idlists-after.txt", sep="\t")
fixedness_nn <- read.table("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\fixedness\\twonounidioms-id-out-lowered-dict-set-of-idlists-after.txt", sep="\t")

fixedness <- rbind(fixedness_nn,fixedness_nv)


idiom_features <- merge(most_common, fixedness, by.x="idiom_id" ,by.y="V1")
idiom_features$V2 <- c()
idiom_features$V3 <- c()
colnames(idiom_features) <- c("id","idiom","fixedness")
idiom_features <- merge(idiom_features,tprop_cross,by.x="idiom",by.y=0)

sprenger_feat <- read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\script files\\sprenger_data_own_ids.csv")
idiom_features <- merge(idiom_features,sprenger_feat,by.x="id",by.y="ID")

most_common_pos_head <- ddply(idioms, "idiom_id", summarise,
                              most_common_pos_head=paste(mostfreq(pos_head)))


idiom_features <- merge(idiom_features,most_common_pos_head,by.x="id",by.y="idiom_id")

library(mgcv)

correlations <- data.frame(cor(idiom_features[,3:31], method = c("pearson", "kendall", "spearman")))



m1 <- gam(fixedness ~ s(est) + ti(id), data=idiom_features, family=gaussian()) 

m2 <- gam(fixedness ~ s(newspapers) + ti(id, LogFreq) + ti(id, est.link),data=idiom_features)
anova(m1,m2)




summary(m1)


#----------^^^^^^^^------------------HIER NU AAN BEZIG-------------------------^^^^^^^^----------------------


#show frequencies per idiom form (pos-tags) per collection
idiom_structures_collection = as.data.frame(table(idioms$pos_head,idioms$doc_type_name))
idiom_structures_counts = as.data.frame(table(idioms$pos_head))

#show idiom deviations from canonnical form(s)
different_forms_length <- as.data.frame(subset(idioms,idioms$idiom_length_orig!=idioms$idiom_length_this))
different_forms_idiom <- as.data.frame(subset(idioms,idioms$idiom_lemma!=idioms$most_common_lemma))

#=====================================================================================================================================================
#MAKE HEATMAP OF FREQUENCIES


scaled_cross_table <- scale(cross_table)

#install.packages("gplots")
library(gplots)
#heatmap.2(scaled_cross_table)
heatmap.2(scaled_cross_table[1:25,])
heatmap.2(scaled_cross_table[1:25,])
heatmap.2(scaled_cross_table[26:50,])
heatmap.2(scaled_cross_table[51:75,])
heatmap.2(scaled_cross_table[76:100,])
heatmap.2(scaled_cross_table[101:125,])
heatmap.2(scaled_cross_table[126:150,])
heatmap.2(scaled_cross_table[151:175,])
heatmap.2(scaled_cross_table[176:182,])


#=====================================================================================================================================================
#TRY TO CLUSER IDIOMS BASED ON THEIR FREQUENCIES IN DIFFERENT COLLECTIONS
#library(cluster)
#plot(agnes(scaled_cross_table),which.plots=15, cex=1)
#=====================================================================================================================================================

#now used: https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/#:~:text=Distance%20Measures%20Essentials-,Partitioning%20clustering,pre%2Dspecified%20by%20the%20analyst.&text=The%20following%20R%20codes%20show,and%20PAM%20clustering%20in%20R

#install.packages("factoextra")
library("factoextra")
fviz_nbclust(scaled_cross_table, kmeans, method = "gap_stat")

set.seed(123)
km.res <- kmeans(scaled_cross_table, 8, nstart = 100)
# Visualize

fviz_cluster(km.res, data = scaled_cross_table,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


library(dplyr)
# Compute hierarchical clustering
res.hc <- scaled_cross_table %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 10, # Cut in four groups
          cex = 0.5, # label size
          k_colors = rainbow(11),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

res.hc2 <- tcross_table %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in groups and color by groups
fviz_dend(res.hc2, k = 3, # Cut in groups
          cex = 0.5, # label size
          k_colors = rainbow(3),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

#check correlation between ttr and F-score
labels = c("blogs","books ","brochures","discussion lists","e-magazines","guides & manuals","legal texts","newsletters","newspapers","periodicals & magazines","policy documents","press releases","proceedings","reports","subtitles","teletext","texts for the visually impaired","web sites","wikipedia","written assignments")
f = c(66.95,62.76,70.90,50.02,53.94,66.18,77.05,63.87,62.56,63.61,63.12,73.15,66.61,66.11,40.52,58.31,58.32,73.18,70.46,64.34)

fscores <- data.frame(labels,f)

texttypes <- c("discussion lists","e-magazines","e-newsletters","press releases","subtitles","teletext pages","web sites","wikipedia","blogs","books","brochures","newsletters","guides manuals","legal texts","newspapers","periodicals magazines","policy documents","proceedings","reports","written assignments","texts for the visually impaired")
ttr_lemmafreqlist <- c(0.013303222135847852,0.026542478259377656,0.3813249869587898,0.057401703751558765,0.009689241125244002,0.055542312276519665,0.024249025176525562,0.03766366983543108,0.11896397524415984,0.010572668146432082,0.04049095832969337,0.11157505443049301,0.03752663077776695,0.014107998171320548,0.010236219490373278,0.01398963091384981,0.010867410407170893,0.04554732903431256,0.024833391412856145,0.01861169390999226,0.046154393095949826)
ttr_lemmaposfreqlist <- c(0.017683684714840903,0.03366492593303601,0.42618675013041213,0.07494102976306735,0.013140093001571153,0.07565526383210988,0.03161953587057931,0.04545992067190976,0.15152577540872178,0.0148120772902397,0.055242289732334915,0.14822989054251542,0.052668583941482175,0.017698750785921487,0.012530770339462963,0.018502535017490638,0.015308295847662488,0.06380383727410238,0.034785952539487686,0.02633071376488698,0.06540242518686619)
ttr_wordfreqlist <- c(0.01503489926570883,0.03065284002963977,0.4298382889932186,0.07136525488664193,0.011863730131671048,0.07209071769908547,0.029865126788917173,0.03938271177692418,0.1471183772761421,0.01305071063989422,0.052791289140600405,0.1453666974857586,0.050038331377938915,0.016012825827075663,0.010854456159696471,0.01553254580936268,0.013587477132372868,0.060657590956134065,0.03182322065905908,0.024573470374105663,0.062094678868641145)

ttr <- data.frame(texttypes,ttr_lemmafreqlist,ttr_lemmaposfreqlist,ttr_wordfreqlist)

ttrf <- merge(fscores, ttr, by.x = "labels", by.y = "texttypes")



plot(ttrf$f,ttrf$lemmafreqlist)
plot(ttrf$f,ttrf$lemmaposfreqlist)
plot(ttrf$f,ttrf$wordfreqlist)

#=====================================================
#Make one dataframe with total freqs of idioms (proportional per 100,000,000 words) AND ttr- / F-scores to visualize a possible
#correlation

total_nr_idioms_prop <- data.frame(prop_cross$texttype, rowSums(prop_cross[2:18]))
colnames(total_nr_idioms_prop) <- c("texttype","propfreq")

ttrfFreq <- merge(ttrf, total_nr_idioms_prop, by.x = "labels", by.y = "texttype")

plot(ttrfFreq$f,ttrfFreq$propfreq, title(main="F-score * relative idiom frequency"),xlab="F-score",ylab="Relative idiom frequency")

#======================================================

library(ggplot2)
library(RColorBrewer)
ggplot(total_nr_idioms_prop, 
       aes(y=propfreq, x=texttype)) + geom_bar(position="stack", stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2) ) + ggtitle("Proportional total frequency of idioms per collection per 100 million words")

ggplot(counts_per_idiom_collection,
       aes(y=freq, x=collection)) + geom_bar(stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2)) + ggtitle("Absolute total frequency of idioms per collection")

buildgraph <- function(begin,end){
mycolors = colorRampPalette(brewer.pal(8, "Set2"))(22)
ggplot(counts_per_idiom_collection[counts_per_idiom_collection$idiom_id>begin & counts_per_idiom_collection$idiom_id<=end,], 
       aes(fill=collection, y=freq, x=lemma)) + geom_bar(position="stack", stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2)) + ggtitle(paste("Idiom frequencies", begin + 1,"to",end,sep=" "))}

buildgraph(0,50)
buildgraph(50,100)
buildgraph(100,150)
buildgraph(150,185)



library(tidyr)
propfreqidioms <- pivot_longer(prop_cross[1:179], cols=2:179, names_to = "idiom", values_to = "freq")

propfreqidioms <- merge(propfreqidioms,most_common,by.x="idiom",by.y="most_common_lemma")

buildgraphprop <- function(begin,end){
  mycolors = colorRampPalette(brewer.pal(8, "Set2"))(22)
  ggplot(propfreqidioms[propfreqidioms$idiom_id>begin & propfreqidioms$idiom_id<=end,], aes(fill=texttype, y=freq, x=idiom)) + geom_bar(position="stack", stat="identity", width=0.7) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2)) + ggtitle(paste("Relative idiom frequencies", begin + 1,"to",end,sep=" "))}

buildgraphprop(0,50)
buildgraphprop(50,100)
buildgraphprop(100,150)
buildgraphprop(150,185)



