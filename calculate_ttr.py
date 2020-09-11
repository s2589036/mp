import os
allfiles = os.listdir("freq_lists/1gms/")

filetypes = ["lemmafreqlist","lemmaposfreqlist","wordfreqlist"]
for filetype in filetypes:
    ttrlist = []
    text_typelist = []
    for filename in sorted(allfiles):
        if filetype in filename:
            file = open("freq_lists/1gms/"+filename, encoding="utf-8")
            amountwords = 0
            freqtot = 0
            for line in file.readlines():
                freq = line.split("\t")[1]
                amountwords += 1
                freqtot += eval(freq)

            ttr = str(amountwords/freqtot)
            ttrlist.append(ttr)
            code_and_texttype = filename[:-4].split(".")[0]
            code = code_and_texttype.split("_")[0]
            text_type = " ".join(code_and_texttype.split("_")[1:])
            text_typelist.append("\"" + text_type + "\"")
            list_type = filename[:-4].split(".")[1]

            #print("\t".join([code,text_type,list_type,str(ttr)]))

    print(filetype,"<-","c("+",".join(ttrlist)+")")
print("texttypes <-","c("+",".join(text_typelist)+")")
