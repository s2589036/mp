import pickle
def makeword2id_dict():
    iddict = {}
    print("file read")
    i = 0

    with open("word_embeddings/server_rug/lemmas", "r", encoding="utf-8") as f:
        lines = [next(f) for i in range(10000)]
        for line in lines:
            i += 1
            for word in line.split():
                if word not in iddict:
                    iddict[word] = [i]
                else:
                    iddict[word].append(i)


    pickle.dump(iddict,open("iddict-mini.pickle","wb"))

makeword2id_dict()

#iddict = pickle.load(open("iddict-klein.pickle","rb"))
#print(iddict["de"])
