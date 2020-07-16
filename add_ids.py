import csv
import itertools


with open("G:\Mijn Drive\Studie informatiekunde\master\master project\project\\all_idioms_for_python.csv", encoding="ANSI") as csv_file:
    #csv_reader = csv.reader(csv_file, delimiter=',', quotechar= '"')
    csv_reader = csv.DictReader(csv_file, delimiter=',', quotechar= '"')
    lemma_dict = {}
    idlist = []
    idiom_id = 0
    while idiom_id < 10:
        for row in csv_reader:
            #print(row["id"],row["idiom_lemma"],row["pos_head"])
            pos_head = row["pos_head"].split(" ")
            idiom_lemma = row["idiom_lemma"].split(" ")
            good_lemma_list = []
            for i in range(len(pos_head)):
                if pos_head[i] == "N" or pos_head[i] == "VZ"  or pos_head[i] == "WW":
                    good_lemma_list.append(idiom_lemma[i])
            good_lemma = " ".join(good_lemma_list)

            permutations = list(itertools.permutations(good_lemma_list))

            idiom_in_dict = 0

            for permutationtuple in permutations:
                permutation = " ".join(permutationtuple)
                if permutation in lemma_dict:
                    idiom_in_dict = 1

            if idiom_in_dict == 1:
                idlist.append(lemma_dict[permutation])
            else:
                idiom_id = idiom_id + 1
                for permutationtuple in permutations:
                    print(permutationtuple)
                    permutation = " ".join(permutationtuple)
                    lemma_dict[permutation] = idiom_id
                idlist.append(idiom_id)
            #print(lemma_dict)
    #print(idlist)

