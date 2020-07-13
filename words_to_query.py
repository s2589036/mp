def query(wordlist):
    string = ""
    for word in wordlist:
        string += "[lemma=\""+word+"\"]"
    return string


def main():
    words = "-"
    file = open("all-queries.txt","w")
    while(words!=""):
        words = input("words (separated by spaces). ")
        wordlist = []
        for word in words.split():
            wordlist.append(word)
        querystring = query(wordlist)+"\n"
        print(querystring)
        file.write(querystring)
    file.close()

main()
