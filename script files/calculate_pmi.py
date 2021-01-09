import pickle
from statistics import *
from math import *
import gensim
model = gensim.models.KeyedVectors.load_word2vec_format('word_embeddings/server_rug/vectors.bin', binary=True)

def most_similar(inputword):
    similar_words = [word for (word, freq) in model.most_similar(inputword, topn=10)]
    return similar_words

"""
onegramfile = open("freq_lists/1gmstotal/SONAR500.lemmafreqlist.1-gram.total.tsv", encoding="utf-8")
onegramdict = {}
for line in onegramfile.readlines():
    parts = line.split("\t")
    word = parts[0]
    freq = parts[1]
    if(word not in onegramdict):
        onegramdict[word] = freq
    else:
        onegramdict[word] = onegramdict[word] + freq

pickle.dump(onegramdict,open("onegramdict.pickle","wb"))
"""

def countin5gram(word1,word2,fivegramfile):
    totalfreq = 0
    for line in fivegramfile.readlines():
        fivegramwords = line.lower().split("\t")[0].split()
        print("fivegramwords = ", fivegramwords)
        freq = line.split("\t")[1]
        if word1 in fivegramwords and word2 in fivegramwords:
            totalfreq = totalfreq+eval(freq)
    print("totalfreq = ", totalfreq)
    return totalfreq



def calc_pmi(word1,word2, onegramdict, fivegramfile):
    total_amount_of_n_n_pairs = 78943189
    try:
        #newpmi = log((total_amount_of_n_n_pairs * freqofword1andword2indict/(freqword1indict*freqword2indict),10)
        pmi = log((total_amount_of_n_n_pairs * countin5gram(word1,word2,fivegramfile))/(eval(onegramdict[word1])*eval(onegramdict[word2])),10)
    except:
        pmi = 0
    return pmi

def calc_fixedness(word1,word2):
    fivegramfile = open("freq_lists/5gmstotal/SONAR500.lemmafreqlist.5-gram.total.tsv", encoding="utf-8")
    onegramdict = pickle.load(open("onegramdict.pickle", "rb"))

    #USE: https: // github.com / coosto / dutch - word - embeddings
    word1sim = most_similar(word1)
    word2sim = most_similar(word2)

    #VOOR deksel,neus (zelf verzonnen)
    #word1sim = ["doos","bodem","rand"]
    #word2sim = ["oog", "oor", "mond"]

    pmi_list = []

    for simword in word1sim:
        pmi_list.append(calc_pmi(simword,word2,onegramdict,fivegramfile))

    for simword in word2sim:
        pmi_list.append(calc_pmi(simword,word1,onegramdict,fivegramfile))

    print(pmi_list)
    fixedness = (calc_pmi(word1,word2,onegramdict,fivegramfile) - mean(pmi_list)) / stdev(pmi_list)
    print(word1, word2, fixedness)

def main():
    inputwords = "-"
    while inputwords != "":

        inputwords = input("Voer 2 woorden in (gescheiden door ,) ")
        word1 = inputwords.split(",")[0]
        word2 = inputwords.split(",")[1]
        print(calc_fixedness(word1,word2))

main()