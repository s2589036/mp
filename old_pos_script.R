
pos=read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\script files\\collection_pos_freq.csv",sep=";",stringsAsFactors = FALSE)
pos$possum1 = pos$ZNW + pos$ADJ + pos$VZ + pos$LID + pos$VNW + pos$WW + pos$BW + pos$TSW
pos$formality_orig = ((pos$ZNW/pos$possum1)*100 + (pos$ADJ/pos$possum1)*100 + (pos$VZ/pos$possum1)*100 + (pos$LID/pos$possum1)*100 - (pos$VNW/pos$possum1)*100 - (pos$WW/pos$possum1)*100 - (pos$BW/pos$possum1)*100 - (pos$TSW/pos$possum1)*100 +100)/2

# 2"ADJ"              3"BW"               4"LID"              5"ZNW"              6"TSW"              7"VNW"             
# 8"VZ"               9"WW"               10"LET"              11"SPEC"             12"TW"               13"VG"



#sum the relevant pos-tag frequencies
pos$possum_chosen = rowSums(pos[, c(2,3,4,5,6,7,8,9)])

pos$possum_all = rowSums(pos[, c(2,3,4,5,6,7,8,9,10,11,12,13)])

#calculate formality score using some formal categories and some informal categories 
pos$formality_chosen = (
  #formal categories
  (rowSums(pos[, c(2,4,5,8)])/pos$possum_chosen)*100 
  
  #informal categories
  - (rowSums(pos[, c(3,6,7,9)])/pos$possum_chosen)*100 
  +100)/2

#FULL ENGLISH NAMES
#colnames(pos)[1:13] <- c("collection","adjective","adverb","determiner","noun","interjection","pronoun","preposition","verb","punctuation","names/unknown","numerator","conjunction")
colnames(pos)[1:13] <- c("collection","adj","adv","det","noun","interj","pron","prep","verb","punct","names","num","conj")
