print("Loading modules...",end="")
import pickle
from statistics import *
from math import *
import gensim
import re
print("loaded")


print("Loading model...", end="")
model = gensim.models.KeyedVectors.load_word2vec_format('word_embeddings/server_rug/vectors.bin', binary=True)
print("loaded.")

print("Loading iddict...", end="")
#VERANDER DIT TERUG!!!
iddict = pickle.load(open("iddict.pickle","rb"))
print("loaded.")
#iddict = {"doos": [1, 2, 3], "bodem": [1, 2, 3], "rand": [3, 4, 5], "oog": [3, 6, 7], "oor": [8, 1, 3],"mond": [9, 1, 2], "deksel": [10, 8, 1], "neus": [1, 2, 6]}

def most_similar(inputword):
    #TODO: word.lower() != inputword.lower()
    similar_words = [word for (word, freq) in model.most_similar(inputword, topn=500) if word != inputword]
    good_similar_words = []
    i = 0

    while len(good_similar_words) < 5:
        word = similar_words[i].lower()
        i = i + 1
        #print("try word ", i, "from 100: ",word)
        if word in iddict and re.match('^[\w-]+$', word) is not None:
            if len(iddict[word]) > 5 and word not in good_similar_words:
                good_similar_words.append(word)
            else:
                i=i+1
                pass
        else:
            i = i+1
            pass

    print(good_similar_words)
    return good_similar_words


def calc_pmi(word1,word2,iddict):
    total_amount_of_n_n_pairs = 78943189

    word1list = set(iddict[word1])
    word2list = set(iddict[word2])
    intersectword1word2 = list(word1list.intersection(word2list))

    word1freq = len(word1list)
    word2freq = len(word2list)
    word12freq = len(intersectword1word2)

    print(word1,": ",word1freq)
    print(word2,": ",word2freq)
    print(word1,"&",word2,": ",word12freq)

    try:
        pmi = log(total_amount_of_n_n_pairs * word12freq /(word1freq*word2freq),10)
    except ValueError:
        pmi = 0

    print("Pmi: ",pmi,"\n")
    return pmi


def calc_fixedness(word1,word2,iddict):
    word1sim = most_similar(word1)
    word2sim = most_similar(word2)

    #VOOR deksel,neus (zelf verzonnen)
    #word1sim = ["doos","bodem","rand"]
    #word2sim = ["oog", "oor", "mond"]

    pmi_list = []
    print("PMI-SCORES WITH", word1)
    print("similar words to ", word2, ": ", word2sim)
    for simword in word2sim:
        pmi = calc_pmi(word1, simword, iddict)
        #print(word1, simword, pmi)
        pmi_list.append(pmi)

    print("\n")
    print("PMI-SCORES WITH", word2)
    print("similar words to ", word1, ": ", word1sim)
    for simword in word1sim:
        try:
            pmi = calc_pmi(simword,word2,iddict)
        except ValueError:
            pmi = 0
        pmi_list.append(pmi)

    print("PMI-SCORE OF ORIGINAL WORDS: ")
    try:
        original_pmi = calc_pmi(word1,word2,iddict)
    except ValueError:
        original_pmi = 0
    pmi_list.append(original_pmi) #klopt dit?

    print("(", original_pmi, "- mean(", pmi_list, ")) / stdev(",pmi_list,")")
    try:
        fixedness = (original_pmi - mean(pmi_list)) / stdev(pmi_list)
    except ValueError:
        fixedness = 0
    return fixedness

def main():
    inputwords = "-"
    while inputwords != "":
        inputwords = input("Enter 2 words (separated by ,) ")
        word1 = inputwords.split(",")[0]
        word2 = inputwords.split(",")[1]
        print("Fixedness of ", word1, " with ", word2, ":", calc_fixedness(word1,word2,iddict))
main()