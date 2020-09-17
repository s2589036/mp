import pickle
from math import *
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
        fivegramwords = line.split("\t")[0].split()
        #print("fivegramwords = ", fivegramwords)
        freq = line.split("\t")[1]
        if word1 in fivegramwords and word2 in fivegramwords:
            totalfreq = totalfreq+eval(freq)
    print("totalfreq = ", totalfreq)
    return totalfreq

def calc_pmi(word1,word2, onegramdict, fivegramfile):
    total_amount_of_n_n_pairs = 78943189
    try:
        pmi = log((total_amount_of_n_n_pairs * countin5gram(word1,word2,fivegramfile))/(eval(onegramdict[word1])*eval(onegramdict[word2])),10)
    except:
        pmi = 0
    return pmi


def main():

    inputwords = "-"

    while inputwords != "":
        fivegramfile = open("freq_lists/5gmstotal/SONAR500.lemmafreqlist.5-gram.total.tsv", encoding="utf-8")
        onegramdict = pickle.load(open("onegramdict.pickle", "rb"))
        inputwords = input("Voer 2 woorden in (gescheiden door ,) ")
        word1 = inputwords.split(",")[0]
        word2 = inputwords.split(",")[1]
        print(calc_pmi(word1,word2,onegramdict,fivegramfile))

main()