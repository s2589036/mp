import csv


with open("G:\Mijn Drive\Studie informatiekunde\master\master project\project\\all_idioms_for_python.csv", encoding="ANSI") as csv_file:
    #csv_reader = csv.reader(csv_file, delimiter=',', quotechar= '"')
    csv_reader = csv.DictReader(csv_file, delimiter=',', quotechar= '"')
    lemma_dict = {}
    idlist = []
    idiom_id = 1
    for row in csv_reader:
        #print(row["id"],row["idiom_lemma"],row["pos_head"])
        pos_head = row["pos_head"].split(" ")
        idiom_lemma = row["idiom_lemma"].split(" ")
        good_lemma_list = []
        for i in range(len(pos_head)):
            if pos_head[i] == "N" or pos_head[i] == "VZ"  or pos_head[i] == "WW":
                good_lemma_list.append(idiom_lemma[i])
        good_lemma = " ".join(good_lemma_list)
        if(good_lemma in lemma_dict):
            idlist.append(str(lemma_dict[good_lemma]))
        else:
            lemma_dict[good_lemma] = idiom_id
            idlist.append(str(idiom_id))
            idiom_id = idiom_id + 1
