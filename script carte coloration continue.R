#permet de faire une carte choroplèthe avec coloration continue.
library(RColorBrewer)
colors <- brewer.pal(6, "RdBu") #couleur qu'on veut pour sa palette. Palette séquentielle ou divergente
pal <- colorRampPalette(colors) #fonction qui crée une palette de n valeurs à partir d'un certain nombre de points. On passe pal(n) en argument de carte.prop

## exemple 
carte.prop2(PACA, pres2012comm, "Sarkozy.ins2", sp.key="CodeInsee", data.key="CodeInsee", at=as.integer(levels(as.factor(pres2012comm$Sarkozy.ins2))), border="transparent", palette=pal(length(levels(as.factor(pres2012comm$Sarkozy.ins2)))), NA.white = T)
#le truc consiste à passer aurant de breaks, et de couleurs, à la fonction que de valeurs différentes de la variable continue représentée. 