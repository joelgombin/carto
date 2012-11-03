lissage.loess <- function(carte, donnees, by.x, by.y, contours=F, points=F, xrange=40, yrange=40, span=0.75, frame=NA, col="RdBu", unit ="", north.arrow=T) { 
  #carte doit être un objet de type SpatialPolygons, donnees un dataframe avec l'id et le z UNIQUEMENT
  #frame doit être un objet de type SpatialPolygons, représentant le contour extérieur de la carte. Penser à fournir ce paramètre si carte comporte beaucoup de polygones (sinon il est calculé, mais ça peut être long)!
  #le paramètre span sert à régler l'étendue des points pris en compte dans le calcul. Entre 0 et 1, c'est la proportion des points pris en compte.
  #attention, une valeur égale ou supérieure à 1 rend le calcul très long. Plus la valeur est petite, plus le calcul est aisé.
  #unit est l'unité à indiquer éventuellement dans l'échelle de la légende.
  #north.arrow indique s'il faut mettre une rose des vents 
  merge(carte@data, donnees, by.x = by.x, by.y=by.y, all.x=T, all.y=F)->carte@data
  #on fusionne les dataframe 
  dim(carte@data)[2]->n
  carte[!is.na(carte@data[,n]),]->carte 
  #on élimine les points avec données manquantes
  coordinates(carte)->coord 
  
  library(RColorBrewer)
  colors <- brewer.pal(6, col) #couleur qu'on veut pour sa palette. Palette séquentielle ou divergente
  pal <- colorRampPalette(colors)
  
  #on récupère les coordonnées des centroïdes 
  data.frame(z=rep(0,dim(coord)[1]))->data
  as.numeric(carte@data[,n])->data$z #on récupère les valeurs de z
  coord[,1]->data$x
  coord[,2]->data$y 
  library(maptools)
  gpclibPermit()
  if (is.na(frame)) unionSpatialPolygons(carte, rep(1, length(carte)), avoidGEOS=T)->frame
  #on récupère le polygone extérieur s'il n'est pas fourni par l'utilisateur
  bbox(frame)->b
  #et ses coordonnées extrêmes, qui serviront pour le calcul de la grille
  #library(akima)
  loess(z ~ x + y + x*y, data, model=T, normalize = F, span = span, control=loess.control(surface="direct", statistics="approximate", trace.hat="approximate"))->inter
  
  #on fait l'interpolation sur une grille aussi large et haute que les dimensions maximales du polygone. C'est une extrapolation de type spline. 
  library(spatialkernel)  
  rep(seq(b[1,1], b[1,2], length=xrange), times = yrange)->x
  rep(seq(b[2,1], b[2,2], length=yrange), each = xrange)->y
  SpatialPoints(data.frame(x,y))->spInter #on crée une grille pour éliminer les points extrapolés en dehors du cadre
  predict(inter,coordinates(spInter))->z
  over(spInter, frame)->contour #la fonction qui évalue
  c(xrange, yrange)->dim(contour) #on transforme le vecteur en matrice
  c(xrange, yrange)->dim(z)
  z[is.na(contour)]<-NA #on met des NA dans le résultat de SP pour les pixels en dehors du polygone
  #data.frame(x,y,z)[order(x,y),]->i
  list()->inter
  seq(b[1,1], b[1,2], length=xrange)->inter$x
  seq(b[2,1], b[2,2], length=yrange)->inter$y
  z->inter$z
  pal(99)->pal.n
  
  image(inter, asp=1, xaxt="n", yaxt="n", bty="n", col=pal.n) #représente la grille interpolée (sans axes ni boîte)
  plot(frame, add=T) #on représente aussi le polygone extérieur
  if (contours) contour(inter, add=T) #les contours des courbes
  if (points) with(data, points(x,y)) #les points de mesure initiaux
  carte.prop.legende2(at=seq(min(z, na.rm=T), max(z, na.rm=T),length.out=100), palette=pal.n, na.leg=F, posleg="topleft", unit = unit)
  return(inter) #on récupère les valeurs
}


# idéalement il faudrait pouvoir établir un variogramme de manière préalable afin d'évaluer le lag pertinent
# (package gstat, fonctions variogram et plot.variogram)