def main():
    title = "-"
    while title != "":
        title = input("title. ")
        authors = input("authors (seperated by comma)." )
        authorlist = authors.strip(" ").split(",")
        year = input("year")
        titleprint = ''.join(e for e in title if e.isalnum() or e == " ").replace(" ","_").lower()
        while "__" in titleprint:
            titleprint.lower()
        authorprint = "_".join(authorlist).lower()
        filename = year+"_"+authorprint+"_"+titleprint
        print(filename.replace("__","_"))

main()
