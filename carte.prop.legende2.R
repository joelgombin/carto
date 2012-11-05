# A reprendre pour modifier en cas d'échelle avec beaucoup de subdivisions
#fonction carte.prop.legende
carte.prop.legende2 <- function (posleg = "topleft", at, palette, rect.width = 0.03, 
    rect.height = 0.03, inset = c(0.05, 0.05), na.leg = TRUE, 
    na.string = "NA", cex = 0.8, unit = "", north.arrow=T) 
{
    usr <- par("usr")
    nb.rect <- length(at) - 1
  
    inset.x <- (usr[2] - usr[1]) * inset[1]
    inset.y <- (usr[4] - usr[3]) * inset[2]
    rect.width <- (usr[2] - usr[1]) * rect.width
    text.width <- max(strwidth(at))
    if (na.leg) 
        text.width <- max(text.width, strwidth(na.string))
    space.width <- rect.width/3
    total.width <- rect.width + space.width + text.width
    rect.height <- (usr[4] - usr[3]) * rect.height
    total.height <- (nb.rect + 1) * rect.height
    if (na.leg) 
        total.height <- total.height + 2 * rect.height
    left <- switch(posleg, bottomright = , topright = , right = usr[2] - 
        total.width - inset.x, bottomleft = , left = , topleft = usr[1] + 
        inset.x, bottom = , top = , center = (usr[1] + usr[2] - 
        total.width)/2)
    top <- switch(posleg, bottomright = , bottom = , bottomleft = usr[3] + 
        total.height + inset.y, topleft = , top = , topright = usr[4] - 
        inset.y, left = , right = , center = (usr[3] + usr[4] + 
        total.height)/2)
    if (nb.rect <= 8) { 
    rects <- 1:nb.rect
    rect(left, top - rects * rect.height, left + rect.width, 
        top - (rects + 1) * rect.height, col = rev(palette))
    text(left + rect.width + space.width, top - (1:(nb.rect + 
        1)) * rect.height, labels = paste(format(rev(at), digits=2), unit), adj = c(0, 0.5), 
        cex = cex)
    if (na.leg) {
        rect(left, top - (nb.rect + 2) * rect.height, left + 
            rect.width, top - (nb.rect + 3) * rect.height, density = 30, 
            angle = 45)
        text(left + rect.width + space.width, top - (nb.rect + 
            2.5) * rect.height, labels = na.string, adj = c(0, 
            0.5), cex = cex)
    }
    }
    else {
      nb.rect <- 100
     rect.height <- 0.4/nb.rect
      rect.height <- (usr[4] - usr[3]) * rect.height
      rects <- 1:nb.rect  
      rect(left, top - rects * rect.height, left + rect.width, top - (rects + 1) * rect.height, col = palette[as.integer(seq(length(at),1, -length(at)/nb.rect))], border=NA)
      rect(left, top - 1 * rect.height, left + rect.width, top - (nb.rect + 1) * rect.height, border="black")
      text(left + rect.width + space.width, top - (c(1,(nb.rect + 
        1))) * rect.height, labels = paste(format(c(max(at), min(at)), digits=2), unit), adj = c(0, 0.5), 
        cex = cex)
       if (na.leg) {
        rect(left, top - (nb.rect + 10) * rect.height, left + 
            rect.width, top - (nb.rect + 16) * rect.height, density = 30, 
            angle = 45)
        text(left + rect.width + space.width, top - (nb.rect + 
            13) * rect.height, labels = na.string, adj = c(0, 
            0.5), cex = cex)
    }
  }
  if (north.arrow) northarrow(loc = c(left + rect.width, top - (nb.rect + 30) * rect.height), size = (usr[4]-usr[3])/25, cex = 0.6)
}