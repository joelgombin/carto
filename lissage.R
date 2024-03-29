lissage <- function(carte, donnees, by.x, by.y, contours=F, points=F, xrange=40, yrange=40, frame=NA) { 
  #carte doit �tre un objet de type SpatialPolygons, donnees un dataframe avec l'id et le z
  #frame doit �tre un objet de type SpatialPolygons, repr�sentant le contour de la carte. Penser � fournir ce param�tre si carte comporte beaucoup de polygones (sinon il est calcul�, mais �a peut �tre long)!
  merge(carte@data, donnees, by.x = by.x, by.y=by.y, all.x=T, all.y=F)->carte@data
  #on fusionne les dataframe 
  dim(carte@data)[2]->n
  carte[!is.na(carte@data[,n]),]->carte 
  #on �limine les points avec donn�es manquantes
  coordinates(carte)->coord 
  #on r�cup�re les coordonn�es des centro�des 
  list()->data
  as.numeric(carte@data[,n])->data$z #on r�cup�re les valeurs de z
  coord[,1]->data$x
  coord[,2]->data$y #data est la liste au format n�cessaire pour akima
  library(maptools)
  gpclibPermit()
  if (is.na(frame)) unionSpatialPolygons(carte, rep(1, length(carte)), avoidGEOS=T)->frame
  #on r�cup�re le polygone ext�rieur s'il n'est pas fourni par l'utilisateur
  bbox(frame)->b
  #et ses coordonn�es extr�mes, qui serviront pour le calcul de la grille
  library(akima)
  with(data, interp(x, y, z, xo=seq(b[1,1], b[1,2], length=xrange), yo=seq(b[2,1], b[2,2], length=yrange), linear=F, extrap=T, duplicate="mean"))->inter
  #on fait l'interpolation sur une grille aussi large et haute que les dimensions maximales du polygone. C'est une extrapolation de type spline. 
  library(spatialkernel)  
  rep(inter$x, times = length(inter$y))->x
  rep(inter$y, each=length(inter$x))->y
  SpatialPoints(data.frame(x,y))->spInter #on cr�e une grille pour �liminer les points extrapol�s en dehors du cadre
  over(spInter, frame)->contour #la fonction qui �value
  c(length(inter$x), length(inter$y))->dim(contour) #on transforme le vecteur en matrice
  inter$z[is.na(contour)]<-NA #on met des NA dans le r�sultat de SP pour les pixels en dehors du polygone
  
  image(inter, asp=1, xaxt="n", yaxt="n", bty="n") #repr�sente la grille interpol�e (sans axes ni bo�te)
  plot(frame, add=T) #on repr�sente aussi le polygone ext�rieur
  if (contours) contour(inter.li, add=T) #les contours des courbes
  if (points) with(data, points(x,y)) #les points de mesure initiaux
  return(inter) #on r�cup�re les valeurs
}


# id�alement il faudrait pouvoir �tablir un variogramme de mani�re pr�alable afin d'�valuer le lag pertinent
# (package gstat, fonctions variogram et plot.variogram)