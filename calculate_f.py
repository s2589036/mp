import os
allfiles = os.listdir("freq_lists/1gms/lemmapos")

for filename in sorted(allfiles):
    if filename[-3:] == "tsv":
        onegramfile = open("freq_lists/1gms/lemmapos/"+filename, encoding="utf-8")
        tagdict = {"n":0,"adj":0,"vz":0,"lid":0,"vnw":0,"ww":0,"bw":0,"tsw":0}
        for line in onegramfile.readlines():
            tag = line.split(" ")[1].split("(")[0].lower()

            if tag in tagdict:
                tagdict[tag]+=1
            else:
                tagdict[tag] = 1

        possum = tagdict['n'] + tagdict['adj'] + tagdict['vz'] + tagdict['lid']  + tagdict['vnw'] + tagdict['ww'] + tagdict['bw'] + tagdict['tsw']

        fscore = ((tagdict['n']/possum)*100 +
                   (tagdict['adj']/possum)*100 +
                   (tagdict['vz']/possum)*100 +
                   (tagdict['lid']/possum)*100  -
                   (tagdict['vnw']/possum)*100  -
                   (tagdict['ww']/possum)*100 -
                   (tagdict['bw']/possum)*100 -
                   (tagdict['tsw']/possum)*100)

        print(filename,'\t', fscore)