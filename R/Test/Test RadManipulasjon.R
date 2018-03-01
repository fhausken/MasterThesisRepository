rm(list=ls()) #Clears environment

a=c(1,2,4)
b=c("a","b","c")
c=3


d=list(a,b,c)

d[[1]] #Henter ut rad 1. Trenger ikke bruke dobbel brackets n?r man skal hente ut hele raden, men det er mer korrekt notasjon.
e = d[c(1,2)] #Henter ut rad 1 og rad 2
d[[1]][1] #Henter ut rad 1, kolonne 1. Merk dobbel brackets p? kolonne for ? kunne aksessere noe inni en liste. d[1][1] fungerer ikke

e=list(Test1=a,Test2=b)
d=list(a,b,c,e)

d[[4]][[1]][1]
d[[4]]$Test1[1] # M? v?re navngitte kolonner/rader for a $ skal funke som indeksering


length(d) #Antall kolonner. Antall rader gir ikke mening da disse kan v?re forskjellige i hver kolonne.

f=list(1,2,3,4)
f=list(a,b)
f=list(f,a)
s=unlist(f)

l=list(a,b)
ku=unlist(l)
test=data.frame(matrix(unlist(l), ncol=3, byrow=TRUE),stringsAsFactors=FALSE)
names(test) = c("A","B","C")
row.names(test) = c("23",",23")


bb = array(1:12, dim = c(1,3,1,1,2,1,2))
print(bb)
