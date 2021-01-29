#================================TEST AMOUNT OF ADJS PER COLLECTION=============================================================# 
# idioms$amountofadjs <- str_count(idioms$pos_head,\"ADJ\")
# library(dplyr)
# 
# idioms %>%
#   group_by(doc_type_name) %>%
#   summarise_at(vars(amountofadjs), funs(sum(amountofadjs, na.rm=TRUE)))
#================================================================================================================================


#=========================================================================================================================================
#OLD DATAFRAMES WITH ALL VALUES FOR THE TEXT TYPES
#=========================================================================================================================================

#Make one dataframe with total freqs of idioms (proportional per 100,000,000 words) AND ttr- / F-scores to visualize a possible
#correlation

#total_nr_idioms_prop <- data.frame(prop_cross$texttype, rowSums(prop_cross[2:18]))
#colnames(total_nr_idioms_prop) <- c("texttype","propfreq")
#ttrfrelFreq <- merge(textfeats, total_nr_idioms_prop, by.x = "texttypes", by.y = "texttype")
#plot(ttrfrelFreq$f,ttrfrelFreq$propfreq, title(main="F-score * relative idiom frequency"),xlab="F-score",ylab="Relative idiom frequency")
#idiom_token_freq <- merge(counts_per_collection,freqdf,by.x = "doc_type_name", by.y="texttype")
#idiom_token_rel_freq <- merge(idiom_token_freq,ttrfrelFreq,by.x="doc_type_name",by.y="labels")

#hist(idiom_token_rel_freq$V1)
#hist(idiom_token_rel_freq$tokenfreq)

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


#================================================================================================================================================================
#==================================================================try to improve postags using spacy============================================================
#================================================================================================================================================================


spacy_initialize(model="nl", python_executable = "C:\\Users\\niels\\AppData\\Local\\Programs\\Python\\Python37\\python.exe")


getpos <- function(x){
  str(spacy_parse(x)$pos)
}

idiomsub = idioms[1:10,]


pos_heads_spacy_df <- ddply(idiomsub, "sentenceid", summarise, 
                            pos_tag_spacy=getpos(idiom_lemma))

names(pos_heads_spacy_df)


#================================================================================================================================================================
#====================================================================sample 50 rows for each idiom===============================================================
#===================================================================sampled THE IDIOMS WITH MISTAKES=============================================================
#================================================================================================================================================================


sampledidioms <- data.frame()

datalist = list()
#added newly tagged idioms to old samples
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
#MAKE HEATMAP OF FREQUENCIES
#=====================================================================================================================================================
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
heatmap.2(scaled_cross_table[151:178,])


#----------^^^^^^^^------------------HIER NU AAN BEZIG-------------------------^^^^^^^^----------------------


#show frequencies per idiom form (pos-tags) per collection
idiom_structures_collection = as.data.frame(table(idioms$pos_head,idioms$doc_type_name))
idiom_structures_counts = as.data.frame(table(idioms$pos_head))

#show idiom deviations from canonnical form(s)
different_forms_length <- as.data.frame(subset(idioms,idioms$idiom_length_orig!=idioms$idiom_length_this))
different_forms_idiom <- as.data.frame(subset(idioms,idioms$idiom_lemma!=idioms$most_common_lemma))


