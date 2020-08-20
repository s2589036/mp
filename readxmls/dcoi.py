
from bs4 import BeautifulSoup as bs
poslist = []
shortposlist = []
wordlist = []
lemmalist = []

#FOR DOC IN COLLECTIONFOLDER:
doc = bs(open('WR-P-P-B-0000000004.dcoi.xml','r'),features="lxml")
for item in doc.find_all("w"):
    txt = "{:<80} {:<5} {:<25} {:<25}"
    pos = item.get("pos")
    shortpos = pos.split("(")[0]
    lemma = item.get("lemma")
    word = item.text

    poslist.append(pos)
    shortposlist.append(shortpos)
    wordlist.append(word)
    lemmalist.append(lemma)

uniquelemmalist = list(set(lemmalist))
uniquewordlist = list(set(wordlist))

print("word_level_ttr:", len(uniquewordlist)/len(wordlist))
print("lemma_level_ttr:", len(uniquelemmalist)/len(lemmalist))

