# Author: Raphael Champeimont
# UMR 7238 Biologie Computationnelle et Quantitative

# General-purpose functions potentially useful for any R script

auxfunctions <- "loaded"

#cat("auxfunctions\n")

# Colors that are good for a white background (unlike lightcolors)
clearcolors <- colorRampPalette(c("blue","darkgreen","orange","red"))(1000)

bluered <- colorRampPalette(c("blue","red"))(1000)
ntColors <- c("#009900","blue","red","orange")
names(ntColors) <- c("A","T","G","C")

# Many different colors, good for printing on white background (unlike rainbow),
# but not adapted for showing intensity (no idea of "strong" and "low").
manycolors <- colorRampPalette(c(c("#773300", "#EE0000", "#FF6600", "#00AA00", "#00AAFF", "#0000EE", "#FF00FF")))(1000)

# A nice color scale from blue to red, with yellow as the center.
# Like matlabcolors, but with yellow as the middle value instead of green.
# I think it is better than matlab which has more green/blue than orange/red.
yellowdivide <- colorRampPalette(c("darkblue","blue","cyan","yellow","orange","red","darkred"))(1000)
lightcolors <- yellowdivide
defaultcolors <- yellowdivide

# A set of nice looking colors on white background
niceColorSet <- c("#0000EE", # blue
                  "#EE0000", # red
                  "#00AA00", # green
                  "#FF6600", # orange
                  "#FF00FF", # magenta
                  "#00AAFF", # light blue
                  "#773300", # brown
                  "#DDBB00" # yellow
                  )

# Colors used in MATLAB "jet".
# Good for showing "low" versus "strong" values.
matlabcolors <- c("#000081", "#000082", "#000083", "#000084", "#000085", "#000086", 
                  "#000087", "#000088", "#000089", "#00008A", "#00008B", "#00008C", 
                  "#00008D", "#00008E", "#00008F", "#000090", "#000091", "#000092", 
                  "#000093", "#000094", "#000095", "#000096", "#000097", "#000098", 
                  "#000099", "#00009A", "#00009B", "#00009C", "#00009D", "#00009E", 
                  "#00009F", "#0000A0", "#0000A1", "#0000A2", "#0000A3", "#0000A4", 
                  "#0000A5", "#0000A6", "#0000A7", "#0000A8", "#0000A9", "#0000AA", 
                  "#0000AB", "#0000AC", "#0000AD", "#0000AE", "#0000AF", "#0000B0", 
                  "#0000B1", "#0000B3", "#0000B4", "#0000B5", "#0000B6", "#0000B7", 
                  "#0000B8", "#0000B9", "#0000BA", "#0000BB", "#0000BC", "#0000BD", 
                  "#0000BE", "#0000BF", "#0000C0", "#0000C1", "#0000C2", "#0000C3", 
                  "#0000C4", "#0000C5", "#0000C6", "#0000C7", "#0000C8", "#0000C9", 
                  "#0000CA", "#0000CB", "#0000CC", "#0000CD", "#0000CE", "#0000CF", 
                  "#0000D0", "#0000D1", "#0000D2", "#0000D3", "#0000D4", "#0000D5", 
                  "#0000D6", "#0000D7", "#0000D8", "#0000D9", "#0000DA", "#0000DB", 
                  "#0000DC", "#0000DD", "#0000DE", "#0000DF", "#0000E0", "#0000E1", 
                  "#0000E2", "#0000E3", "#0000E4", "#0000E6", "#0000E7", "#0000E8", 
                  "#0000E9", "#0000EA", "#0000EB", "#0000EC", "#0000ED", "#0000EE", 
                  "#0000EF", "#0000F0", "#0000F1", "#0000F2", "#0000F3", "#0000F4", 
                  "#0000F5", "#0000F6", "#0000F7", "#0000F8", "#0000F9", "#0000FA", 
                  "#0000FB", "#0000FC", "#0000FD", "#0000FE", "#0000FF", "#0001FF", 
                  "#0002FF", "#0003FF", "#0004FF", "#0005FF", "#0006FF", "#0007FF", 
                  "#0008FF", "#0009FF", "#000AFF", "#000BFF", "#000CFF", "#000DFF", 
                  "#000EFF", "#000FFF", "#0010FF", "#0011FF", "#0012FF", "#0013FF", 
                  "#0014FF", "#0015FF", "#0016FF", "#0017FF", "#0018FF", "#001AFF", 
                  "#001BFF", "#001CFF", "#001DFF", "#001EFF", "#001FFF", "#0020FF", 
                  "#0021FF", "#0022FF", "#0023FF", "#0024FF", "#0025FF", "#0026FF", 
                  "#0027FF", "#0028FF", "#0029FF", "#002AFF", "#002BFF", "#002CFF", 
                  "#002DFF", "#002EFF", "#002FFF", "#0030FF", "#0031FF", "#0032FF", 
                  "#0033FF", "#0034FF", "#0035FF", "#0036FF", "#0037FF", "#0038FF", 
                  "#0039FF", "#003AFF", "#003BFF", "#003CFF", "#003DFF", "#003EFF", 
                  "#003FFF", "#0040FF", "#0041FF", "#0042FF", "#0043FF", "#0044FF", 
                  "#0045FF", "#0046FF", "#0047FF", "#0048FF", "#0049FF", "#004AFF", 
                  "#004BFF", "#004DFF", "#004EFF", "#004FFF", "#0050FF", "#0051FF", 
                  "#0052FF", "#0053FF", "#0054FF", "#0055FF", "#0056FF", "#0057FF", 
                  "#0058FF", "#0059FF", "#005AFF", "#005BFF", "#005CFF", "#005DFF", 
                  "#005EFF", "#005FFF", "#0060FF", "#0061FF", "#0062FF", "#0063FF", 
                  "#0064FF", "#0065FF", "#0066FF", "#0067FF", "#0068FF", "#0069FF", 
                  "#006AFF", "#006BFF", "#006CFF", "#006DFF", "#006EFF", "#006FFF", 
                  "#0070FF", "#0071FF", "#0072FF", "#0073FF", "#0074FF", "#0075FF", 
                  "#0076FF", "#0077FF", "#0078FF", "#0079FF", "#007AFF", "#007BFF", 
                  "#007CFF", "#007DFF", "#007EFF", "#0080FF", "#0081FF", "#0082FF", 
                  "#0083FF", "#0084FF", "#0085FF", "#0086FF", "#0087FF", "#0088FF", 
                  "#0089FF", "#008AFF", "#008BFF", "#008CFF", "#008DFF", "#008EFF", 
                  "#008FFF", "#0090FF", "#0091FF", "#0092FF", "#0093FF", "#0094FF", 
                  "#0095FF", "#0096FF", "#0097FF", "#0098FF", "#0099FF", "#009AFF", 
                  "#009BFF", "#009CFF", "#009DFF", "#009EFF", "#009FFF", "#00A0FF", 
                  "#00A1FF", "#00A2FF", "#00A3FF", "#00A4FF", "#00A5FF", "#00A6FF", 
                  "#00A7FF", "#00A8FF", "#00A9FF", "#00AAFF", "#00ABFF", "#00ACFF", 
                  "#00ADFF", "#00AEFF", "#00AFFF", "#00B0FF", "#00B1FF", "#00B3FF", 
                  "#00B4FF", "#00B5FF", "#00B6FF", "#00B7FF", "#00B8FF", "#00B9FF", 
                  "#00BAFF", "#00BBFF", "#00BCFF", "#00BDFF", "#00BEFF", "#00BFFF", 
                  "#00C0FF", "#00C1FF", "#00C2FF", "#00C3FF", "#00C4FF", "#00C5FF", 
                  "#00C6FF", "#00C7FF", "#00C8FF", "#00C9FF", "#00CAFF", "#00CBFF", 
                  "#00CCFF", "#00CDFF", "#00CEFF", "#00CFFF", "#00D0FF", "#00D1FF", 
                  "#00D2FF", "#00D3FF", "#00D4FF", "#00D5FF", "#00D6FF", "#00D7FF", 
                  "#00D8FF", "#00D9FF", "#00DAFF", "#00DBFF", "#00DCFF", "#00DDFF", 
                  "#00DEFF", "#00DFFF", "#00E0FF", "#00E1FF", "#00E2FF", "#00E3FF", 
                  "#00E4FF", "#00E6FF", "#00E7FF", "#00E8FF", "#00E9FF", "#00EAFF", 
                  "#00EBFF", "#00ECFF", "#00EDFF", "#00EEFF", "#00EFFF", "#00F0FF", 
                  "#00F1FF", "#00F2FF", "#00F3FF", "#00F4FF", "#00F5FF", "#00F6FF", 
                  "#00F7FF", "#00F8FF", "#00F9FF", "#00FAFF", "#00FBFF", "#00FCFF", 
                  "#00FDFF", "#00FEFF", "#00FFFF", "#01FFFE", "#02FFFD", "#03FFFC", 
                  "#04FFFB", "#05FFFA", "#06FFF9", "#07FFF8", "#08FFF7", "#09FFF6", 
                  "#0AFFF5", "#0BFFF4", "#0CFFF3", "#0DFFF2", "#0EFFF1", "#0FFFF0", 
                  "#10FFEF", "#11FFEE", "#12FFED", "#13FFEC", "#14FFEB", "#15FFEA", 
                  "#16FFE9", "#17FFE8", "#18FFE7", "#1AFFE6", "#1BFFE4", "#1CFFE3", 
                  "#1DFFE2", "#1EFFE1", "#1FFFE0", "#20FFDF", "#21FFDE", "#22FFDD", 
                  "#23FFDC", "#24FFDB", "#25FFDA", "#26FFD9", "#27FFD8", "#28FFD7", 
                  "#29FFD6", "#2AFFD5", "#2BFFD4", "#2CFFD3", "#2DFFD2", "#2EFFD1", 
                  "#2FFFD0", "#30FFCF", "#31FFCE", "#32FFCD", "#33FFCC", "#34FFCB", 
                  "#35FFCA", "#36FFC9", "#37FFC8", "#38FFC7", "#39FFC6", "#3AFFC5", 
                  "#3BFFC4", "#3CFFC3", "#3DFFC2", "#3EFFC1", "#3FFFC0", "#40FFBF", 
                  "#41FFBE", "#42FFBD", "#43FFBC", "#44FFBB", "#45FFBA", "#46FFB9", 
                  "#47FFB8", "#48FFB7", "#49FFB6", "#4AFFB5", "#4BFFB4", "#4DFFB3", 
                  "#4EFFB1", "#4FFFB0", "#50FFAF", "#51FFAE", "#52FFAD", "#53FFAC", 
                  "#54FFAB", "#55FFAA", "#56FFA9", "#57FFA8", "#58FFA7", "#59FFA6", 
                  "#5AFFA5", "#5BFFA4", "#5CFFA3", "#5DFFA2", "#5EFFA1", "#5FFFA0", 
                  "#60FF9F", "#61FF9E", "#62FF9D", "#63FF9C", "#64FF9B", "#65FF9A", 
                  "#66FF99", "#67FF98", "#68FF97", "#69FF96", "#6AFF95", "#6BFF94", 
                  "#6CFF93", "#6DFF92", "#6EFF91", "#6FFF90", "#70FF8F", "#71FF8E", 
                  "#72FF8D", "#73FF8C", "#74FF8B", "#75FF8A", "#76FF89", "#77FF88", 
                  "#78FF87", "#79FF86", "#7AFF85", "#7BFF84", "#7CFF83", "#7DFF82", 
                  "#7EFF81", "#80FF80", "#81FF7E", "#82FF7D", "#83FF7C", "#84FF7B", 
                  "#85FF7A", "#86FF79", "#87FF78", "#88FF77", "#89FF76", "#8AFF75", 
                  "#8BFF74", "#8CFF73", "#8DFF72", "#8EFF71", "#8FFF70", "#90FF6F", 
                  "#91FF6E", "#92FF6D", "#93FF6C", "#94FF6B", "#95FF6A", "#96FF69", 
                  "#97FF68", "#98FF67", "#99FF66", "#9AFF65", "#9BFF64", "#9CFF63", 
                  "#9DFF62", "#9EFF61", "#9FFF60", "#A0FF5F", "#A1FF5E", "#A2FF5D", 
                  "#A3FF5C", "#A4FF5B", "#A5FF5A", "#A6FF59", "#A7FF58", "#A8FF57", 
                  "#A9FF56", "#AAFF55", "#ABFF54", "#ACFF53", "#ADFF52", "#AEFF51", 
                  "#AFFF50", "#B0FF4F", "#B1FF4E", "#B3FF4D", "#B4FF4B", "#B5FF4A", 
                  "#B6FF49", "#B7FF48", "#B8FF47", "#B9FF46", "#BAFF45", "#BBFF44", 
                  "#BCFF43", "#BDFF42", "#BEFF41", "#BFFF40", "#C0FF3F", "#C1FF3E", 
                  "#C2FF3D", "#C3FF3C", "#C4FF3B", "#C5FF3A", "#C6FF39", "#C7FF38", 
                  "#C8FF37", "#C9FF36", "#CAFF35", "#CBFF34", "#CCFF33", "#CDFF32", 
                  "#CEFF31", "#CFFF30", "#D0FF2F", "#D1FF2E", "#D2FF2D", "#D3FF2C", 
                  "#D4FF2B", "#D5FF2A", "#D6FF29", "#D7FF28", "#D8FF27", "#D9FF26", 
                  "#DAFF25", "#DBFF24", "#DCFF23", "#DDFF22", "#DEFF21", "#DFFF20", 
                  "#E0FF1F", "#E1FF1E", "#E2FF1D", "#E3FF1C", "#E4FF1B", "#E6FF1A", 
                  "#E7FF18", "#E8FF17", "#E9FF16", "#EAFF15", "#EBFF14", "#ECFF13", 
                  "#EDFF12", "#EEFF11", "#EFFF10", "#F0FF0F", "#F1FF0E", "#F2FF0D", 
                  "#F3FF0C", "#F4FF0B", "#F5FF0A", "#F6FF09", "#F7FF08", "#F8FF07", 
                  "#F9FF06", "#FAFF05", "#FBFF04", "#FCFF03", "#FDFF02", "#FEFF01", 
                  "#FFFF00", "#FFFE00", "#FFFD00", "#FFFC00", "#FFFB00", "#FFFA00", 
                  "#FFF900", "#FFF800", "#FFF700", "#FFF600", "#FFF500", "#FFF400", 
                  "#FFF300", "#FFF200", "#FFF100", "#FFF000", "#FFEF00", "#FFEE00", 
                  "#FFED00", "#FFEC00", "#FFEB00", "#FFEA00", "#FFE900", "#FFE800", 
                  "#FFE700", "#FFE600", "#FFE400", "#FFE300", "#FFE200", "#FFE100", 
                  "#FFE000", "#FFDF00", "#FFDE00", "#FFDD00", "#FFDC00", "#FFDB00", 
                  "#FFDA00", "#FFD900", "#FFD800", "#FFD700", "#FFD600", "#FFD500", 
                  "#FFD400", "#FFD300", "#FFD200", "#FFD100", "#FFD000", "#FFCF00", 
                  "#FFCE00", "#FFCD00", "#FFCC00", "#FFCB00", "#FFCA00", "#FFC900", 
                  "#FFC800", "#FFC700", "#FFC600", "#FFC500", "#FFC400", "#FFC300", 
                  "#FFC200", "#FFC100", "#FFC000", "#FFBF00", "#FFBE00", "#FFBD00", 
                  "#FFBC00", "#FFBB00", "#FFBA00", "#FFB900", "#FFB800", "#FFB700", 
                  "#FFB600", "#FFB500", "#FFB400", "#FFB300", "#FFB100", "#FFB000", 
                  "#FFAF00", "#FFAE00", "#FFAD00", "#FFAC00", "#FFAB00", "#FFAA00", 
                  "#FFA900", "#FFA800", "#FFA700", "#FFA600", "#FFA500", "#FFA400", 
                  "#FFA300", "#FFA200", "#FFA100", "#FFA000", "#FF9F00", "#FF9E00", 
                  "#FF9D00", "#FF9C00", "#FF9B00", "#FF9A00", "#FF9900", "#FF9800", 
                  "#FF9700", "#FF9600", "#FF9500", "#FF9400", "#FF9300", "#FF9200", 
                  "#FF9100", "#FF9000", "#FF8F00", "#FF8E00", "#FF8D00", "#FF8C00", 
                  "#FF8B00", "#FF8A00", "#FF8900", "#FF8800", "#FF8700", "#FF8600", 
                  "#FF8500", "#FF8400", "#FF8300", "#FF8200", "#FF8100", "#FF8000", 
                  "#FF7E00", "#FF7D00", "#FF7C00", "#FF7B00", "#FF7A00", "#FF7900", 
                  "#FF7800", "#FF7700", "#FF7600", "#FF7500", "#FF7400", "#FF7300", 
                  "#FF7200", "#FF7100", "#FF7000", "#FF6F00", "#FF6E00", "#FF6D00", 
                  "#FF6C00", "#FF6B00", "#FF6A00", "#FF6900", "#FF6800", "#FF6700", 
                  "#FF6600", "#FF6500", "#FF6400", "#FF6300", "#FF6200", "#FF6100", 
                  "#FF6000", "#FF5F00", "#FF5E00", "#FF5D00", "#FF5C00", "#FF5B00", 
                  "#FF5A00", "#FF5900", "#FF5800", "#FF5700", "#FF5600", "#FF5500", 
                  "#FF5400", "#FF5300", "#FF5200", "#FF5100", "#FF5000", "#FF4F00", 
                  "#FF4E00", "#FF4D00", "#FF4B00", "#FF4A00", "#FF4900", "#FF4800", 
                  "#FF4700", "#FF4600", "#FF4500", "#FF4400", "#FF4300", "#FF4200", 
                  "#FF4100", "#FF4000", "#FF3F00", "#FF3E00", "#FF3D00", "#FF3C00", 
                  "#FF3B00", "#FF3A00", "#FF3900", "#FF3800", "#FF3700", "#FF3600", 
                  "#FF3500", "#FF3400", "#FF3300", "#FF3200", "#FF3100", "#FF3000", 
                  "#FF2F00", "#FF2E00", "#FF2D00", "#FF2C00", "#FF2B00", "#FF2A00", 
                  "#FF2900", "#FF2800", "#FF2700", "#FF2600", "#FF2500", "#FF2400", 
                  "#FF2300", "#FF2200", "#FF2100", "#FF2000", "#FF1F00", "#FF1E00", 
                  "#FF1D00", "#FF1C00", "#FF1B00", "#FF1A00", "#FF1800", "#FF1700", 
                  "#FF1600", "#FF1500", "#FF1400", "#FF1300", "#FF1200", "#FF1100", 
                  "#FF1000", "#FF0F00", "#FF0E00", "#FF0D00", "#FF0C00", "#FF0B00", 
                  "#FF0A00", "#FF0900", "#FF0800", "#FF0700", "#FF0600", "#FF0500", 
                  "#FF0400", "#FF0300", "#FF0200", "#FF0100", "#FF0000", "#FE0000", 
                  "#FD0000", "#FC0000", "#FB0000", "#FA0000", "#F90000", "#F80000", 
                  "#F70000", "#F60000", "#F50000", "#F40000", "#F30000", "#F20000", 
                  "#F10000", "#F00000", "#EF0000", "#EE0000", "#ED0000", "#EC0000", 
                  "#EB0000", "#EA0000", "#E90000", "#E80000", "#E70000", "#E60000", 
                  "#E40000", "#E30000", "#E20000", "#E10000", "#E00000", "#DF0000", 
                  "#DE0000", "#DD0000", "#DC0000", "#DB0000", "#DA0000", "#D90000", 
                  "#D80000", "#D70000", "#D60000", "#D50000", "#D40000", "#D30000", 
                  "#D20000", "#D10000", "#D00000", "#CF0000", "#CE0000", "#CD0000", 
                  "#CC0000", "#CB0000", "#CA0000", "#C90000", "#C80000", "#C70000", 
                  "#C60000", "#C50000", "#C40000", "#C30000", "#C20000", "#C10000", 
                  "#C00000", "#BF0000", "#BE0000", "#BD0000", "#BC0000", "#BB0000", 
                  "#BA0000", "#B90000", "#B80000", "#B70000", "#B60000", "#B50000", 
                  "#B40000", "#B30000", "#B10000", "#B00000", "#AF0000", "#AE0000", 
                  "#AD0000", "#AC0000", "#AB0000", "#AA0000", "#A90000", "#A80000", 
                  "#A70000", "#A60000", "#A50000", "#A40000", "#A30000", "#A20000", 
                  "#A10000", "#A00000", "#9F0000", "#9E0000", "#9D0000", "#9C0000", 
                  "#9B0000", "#9A0000", "#990000", "#980000", "#970000", "#960000", 
                  "#950000", "#940000", "#930000", "#920000", "#910000", "#900000", 
                  "#8F0000", "#8E0000", "#8D0000", "#8C0000", "#8B0000", "#8A0000", 
                  "#890000", "#880000", "#870000", "#860000", "#850000", "#840000", 
                  "#830000", "#820000", "#810000", "#800000")


# An inch is XXX mm
inchMM <- 25.4

# This function will exist in R 2.15 so test if it is present,
# else define it in a forward-compatible way.
if (!exists("paste0")) {
  paste0 <- function(..., collapse=NULL) {
    return(paste(..., sep="", collapse=collapse))
  }
}


niceSize <- function(size, digits=1) {
  if (size < 1024) {
    return(paste(round(size, digits=digits), "B"))
  } else if (size < 1024*1024) {
    return(paste(round(size/1024, digits=digits), "KB"))
  } else if (size < 1024*1024*1024) {
    return(paste(round(size/(1024*1024), digits=digits), "MB"))
  } else if (size < 1024*1024*1024*1024) {
    return(paste(round(size/(1024*1024*1024), digits=digits), "GB"))
  } else {
    return(paste(round(size/(1024*1024*1024*1024), digits=digits), "TB"))
  }
}


niceFileSize <- function(path) {
  return(niceSize(file.info(path)$size))
}


niceObjectSize <- function(object) {
  return(niceSize(object.size(object)))
}


mapColors <- function(values, colors=defaultcolors, a=min(values, na.rm=TRUE), b=max(values, na.rm=TRUE)) {
  if (a == b) {
    res <- rep(colors[1], length(values))
  } else {
    res <- rep("", length(values))
    for (i in seq(along=values)) {
      if (is.na(values[i])) {
        res[i] <- NA
      } else {
        colorIndex <- round((values[i]-a)/(b-a) * length(colors))
        if (colorIndex < 0) {
          res[i] <- colors[1]
        } else if (colorIndex >= length(colors)) {
          res[i] <- colors[length(colors)]
        } else {
          res[i] <- colors[colorIndex+1]
        }
      }
    }
    return(res)
  }
}

showColors <- function(col) {
  if (is.function(col)) {
    col <- col(1000)
  }
  image(matrix(1:1000), col=col, xaxt="n", yaxt="n")
}


randomColors <- function(n=1) {
  return(rgb(runif(n), runif(n), runif(n)))
}

randomBrightColors <- function(n=1) {
  return(hsv(runif(n), 1, 1))
}
#showColors(randomBrightColors(10))

drawColorScale <- function(x=NULL, y=NULL, w=NULL, h=NULL, colors=defaultcolors, text=NULL) {
  usr <- par("usr")
  UW <- usr[2] - usr[1]
  UH <- usr[4] - usr[3]
  if (is.null(x))
    x <- par("usr")[1]
  if (is.null(y))
    y <- usr[3]-UH/7
  if (is.null(w))
    w <- UW/3
  if (is.null(h))
    h <- UH/20
  mycolors <- colors[seq(1,length(colors),length.out=20)]
  dx <- w/length(mycolors)
  for (i in seq(along=mycolors)) {
    x1 <- x + (i-1)*dx
    rect(x1, y, x1+dx, y+h, col=mycolors[[i]], border=NA, xpd=TRUE)
  }
  if (!is.null(text)) {
    for (i in seq(along=text)) {
      x1 <- x+w/(length(text)-1)*(i-1)
      lines(c(x1,x1), c(y, y+h/2), xpd=TRUE)
      text(x1, y-h/2, text[[i]], xpd=TRUE, cex=0.7)
    }
  }
}
showColorScale <- drawColorScale

imageAndColorScale <- function(x, col=defaultcolors, zlim=NULL, ...) {
  if (is.null(zlim)) {
    zlim <- range(x, finite=TRUE)
  }
  image(x, col=col, zlim=zlim, ...)
  drawColorScale(colors=col, text=seq(zlim[[1]], zlim[[2]], length.out=5))
  invisible(NULL)
}

readMatrix <- function(filepath, ...) {
  M <- read.table(filepath, ...)
  M <- as.matrix(M)
  return(M)
}

lowerTriangle <- function(M, diag=FALSE) {
  return(M[lower.tri(M, diag=diag)])
}

# Return the indices of the elements of the logical matrix M
# which are true. The result is given as a matrix where the
# first column are the line indices in M and the second column are
# the column indices in M.
whichMatrix <- function(M) {
  n <- nrow(M)
  m <- ncol(M)
  indices <- which(M)
  cols <- floor((indices-1)/n) + 1
  lines <- (indices-1) - (cols-1)*n + 1
  O <- order(lines)
  R <- data.frame(lines=lines[O], cols=cols[O])
  return(R)
}



# User-friendly function to run a Fisher test between
# 2 boolean variables.
testRelation <- function(X, Y) {
  xdesc <- deparse(substitute(X))
  ydesc <- deparse(substitute(Y))
  M <- matrix(rep(0,4), 2, dimnames=list(c(paste(xdesc, "= F"),paste(xdesc, "= T")), c(paste(ydesc, "= F"),paste(ydesc, "= T"))))
  X <- as.logical(X)
  Y <- as.logical(Y)
  if (length(X) != length(Y)) stop("length(X) != length(Y)")
  M[1,1] <- sum(!X & !Y)
  M[1,2] <- sum(!X & Y)
  M[2,1] <- sum(X & !Y)
  M[2,2] <- sum(X & Y)
  print(M)
  FR <- fisher.test(M)
  print(FR)
  return(invisible(FR))
}



# X = numeric
# truth = boolean, same size as X
# The idea is to test how much (X >= xth) = truth
# We try different X thresholds and report precision and recall
benchmarkPrediction <- function(X, truth, X.thresholds=NULL) {
  if (is.null(X.thresholds)) {
    # Automatically compute them
    X.thresholds <- unique(sort(X))
  }
  M <- data.frame(X.threshold=X.thresholds,
                  TP=rep(0,length(X.thresholds)),
                  TN=rep(0,length(X.thresholds)),
                  FP=rep(0,length(X.thresholds)),
                  FN=rep(0,length(X.thresholds)),
                  precision=rep(0,length(X.thresholds)),
                  recall=rep(0,length(X.thresholds)))
  for (i in seq(along=X.thresholds)) {
    xth <- X.thresholds[[i]]
    M[i,"TP"] <- sum((X >= xth) & truth)
    M[i,"TN"] <- sum((X < xth) & !truth)
    M[i,"FP"] <- sum((X >= xth) & !truth)
    M[i,"FN"] <- sum((X < xth) & truth)
  }
  M$precision <- M$TP / (M$TP + M$FP)
  M$recall <- M$TP / (M$TP + M$FN)
  return(M)
}


# Like benchmarkPrediction, but faster when using
# all possible threshold across the values of X
# (like when making a full ROC curve).
# WARNING: Unlike benchmarkPrediction, result is sorted by
# decreasing threshold instead of increasing.
benchmarkPredictionFast <- function(X, truth) {
  if (any(is.na(X))) stop("benchmarkPredictionFast: NA scores forbidden")
  if (any(is.nan(X))) stop("benchmarkPredictionFast: NaN scores forbidden")
  stopifnot(length(X) == length(truth))
  M <- list()
  o <- order(X, decreasing=TRUE)
  XS <- X[o]
  NDUP <- !duplicated(XS, fromLast=TRUE)
  X.threshold <- XS[NDUP]
  TP <- cumsum(truth[o])
  FP <- cumsum(!truth[o])
  TP <- TP[NDUP]
  FP <- FP[NDUP]
  TN <- sum(!truth) - FP
  FN <- length(X) - TP - FP - TN
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  return(data.frame(X.threshold, TP, TN, FP, FN, precision, recall, FPR=FP/(FP+TN), TPR=TP/(TP+FN)))
}

# Rprof()
# X <- rep(1:1000, times=2)
# X <- sample(X)
# truth <- rep(c(0,1), length(X)/2)
# print(system.time(M <- benchmarkPredictionFast(X, truth)))
# Rprof(NULL)
# print(summaryRprof())


benchmarkMini <- function(predictor, truth, verbose=TRUE) {
  L <- c()
  L$TP <- sum(predictor & truth)
  L$FP <- sum(predictor & !truth)
  L$FN <- sum(!predictor & truth)
  L$TN <- sum(!predictor & !truth)
  if (verbose) {
    cat("TP =", L$TP, "  FP =", L$FP, "  FN =", L$FN, "  TN =", L$TN, "\n")
  }
  L$precision <- L$TP / (L$TP + L$FP)
  L$recall <- L$TP / (L$TP + L$FN)
  if (verbose) {
    cat("Precision =", L$precision, "\n")
    cat("Recall =", L$recall, "\n")
  }
  return(invisible(L))
}




plotCorrelation <- function(X, Y,
                            xdesc=deparse(substitute(X)),
                            ydesc=deparse(substitute(Y)),
                            method="pearson", ...) {
  testResult <- cor.test(X, Y, method=method)
  plot(X, Y, pch=4,
       main=paste(method, " ", names(testResult$estimate), "=", round(testResult$estimate, 2), " p-value=", signif(testResult$p.value, 2), sep=""),
       xlab=xdesc, ylab=ydesc,
       ...)
  print(testResult)
  return(invisible(testResult))
}




# Plot an histogram of a given numeric variable
# split according to a factor variable.
plotHistFactor <- function(X, f, breaks, col=NULL) {
  B <- breaks
  f <- as.factor(f)
  levs <- levels(f)
  M <- NULL
  i <- 1
  n <- 0
  if (length(levs) == 0) stop("no levels for factor")
  for (l in levs) {
    h <- hist(X[f == l], plot=FALSE, breaks=B)$count
    n <- length(h)
    if (is.null(M)) {
      M <- matrix(0, nrow=length(levs), ncol=n)
    }
    M[i,] <- h
    i <- i + 1
  }
  if (is.null(col)) {
    mycolors <- mapColors(1:length(levs))
  } else {
    mycolors <- col
  }
  barplot(M, beside=TRUE,
          col=mycolors,
          cex.names=0.7, 
          names.arg=paste(B[1:(length(B)-1)], "-",  B[2:length(B)], sep=""))
  legend("topright", legend=levs, fill=mycolors)
}



# My barplot function
rlcBarplot <- function(M, binwidth=1, col=NULL) {
  if (is.null(col)) {
    col <- mapColors(1:nrow(M))
  }
  A <- matrix(0, nrow=nrow(M), ncol=ncol(M))
  for (j in 1:ncol(M)) {
    A[,j] <- cumsum(M[,j])
  }
  for (j in 1:ncol(M)) {
    for (i in nrow(M):1) {
      rect((j-1)*binwidth, 0, j*binwidth, A[i,j], col=col[[i]])
    }
  }
}

# Same with continuous function
rlcCumLines <- function(M, X=1:ncol(M), col=NULL) {
  if (is.null(col)) {
    col <- mapColors(1:nrow(M))
  }
  A <- matrix(0, nrow=nrow(M), ncol=ncol(M))
  for (j in 1:ncol(M)) {
    A[,j] <- cumsum(M[,j])
  }
  for (i in nrow(M):1) {
    polygon(c(X[1],X,X[length(X)]), c(0,A[i,],0), col=col[[i]], border=NA)
  }
}

rlcCumPlot <- function(M, X=1:ncol(M), col=NULL, ...) {
  if (is.null(col)) {
    col <- mapColors(1:nrow(M))
  }
  A <- matrix(0, nrow=nrow(M), ncol=ncol(M))
  for (j in 1:ncol(M)) {
    A[,j] <- cumsum(M[,j])
  }
  plot(c(min(X), max(X)), c(0, max(A[nrow(A),])), type="n", ...)
  for (i in nrow(M):1) {
    polygon(c(X[1],X,X[length(X)]), c(0,A[i,],0), col=col[[i]], border=NA)
  }
}


rlcMultiTranslateClasses <- function(C, classes=NULL) {
  if (is.numeric(C)) {
    C <- as.integer(C)
    if (any(C < 1)) stop("If C is numeric, it must be integers from 1 to N.")
    if (is.null(classes)) {
      classes <- 1:as.integer(max(C))
    }
  } else if (is.factor(C)) {
    classes <- levels(C)
    C <- as.integer(C)
  } else {
    if (is.null(classes)) {
      classes <- unique(C)
    } else {
      found <- unique(C)
      if (any(! found %in% classes)) {
        k <- which(! found %in% classes)[[1]]
        stop(paste("C contains \"", found[[k]], "\" but it is not in specified classes", sep=""))
      }
    }
    C <- match(C, classes)
  }
  return(list(C=C, classes=classes, cls=1:length(classes)))
}
stopifnot(rlcMultiTranslateClasses(c("a","a","b","d","b"))$C == c(1,1,2,3,2))
stopifnot(rlcMultiTranslateClasses(c("a","a","b","d","b"), classes=c("b","a","c","d"))$C == c(2,2,1,4,1))


# Make an histogram that show contribution of several classes
# X is a vector
# C is a vector with the same size such that
# C[i] is the class of x[i]
# Typically C will be vector of integers or a factor
# ... arguments passed to hist()
rlcMultiHist <- function(X, C, breaks="Sturges", col=NULL, main=NULL, xlab=NULL, add.legend=TRUE, xlim=range(breaks), classes=NULL) {
  if (length(X) != length(C)) stop("X and C must have the same length")
  tmp <- rlcMultiTranslateClasses(C, classes)
  C <- tmp$C
  classes <- tmp$classes
  cls <- tmp$cls
  xdesc <- deparse(substitute(X))
  if (is.numeric(breaks) && length(breaks) > 1) {
    r <- range(breaks)
    keep <- X >= r[[1]] & X <= r[[2]]
    X <- X[keep]
    C <- C[keep]
  }
  h <- hist(X, plot=FALSE, breaks=breaks)
  breaks <- h$breaks
  M <- matrix(0, nrow=length(cls), ncol=length(h$counts))
  i <- 1
  for (cl in cls) {
    hc <- hist(X[C==cl], breaks=breaks, plot=FALSE)
    M[i,] <- hc$counts
    i <- i + 1
  }
  
  if (is.null(main)) {
    mainTitle <- paste("Histogram of", xdesc)
  } else {
    mainTitle <- main
  }
  if (is.null(xlab)) {
    xlab <- xdesc
  }
  plot(h, main=mainTitle, xlab=xlab, xlim=xlim)
  
  if (is.null(col)) {
    col <- rainbow(length(cls), v=0.8)
  }
  A <- matrix(0, nrow=nrow(M), ncol=ncol(M))
  for (j in 1:ncol(M)) {
    A[,j] <- cumsum(M[,j])
  }
  for (j in 1:ncol(M)) {
    for (i in nrow(M):1) {
      rect(breaks[j], 0, breaks[j+1], A[i,j], col=col[[i]])
    }
  }
  
  if (add.legend) {
    legend(x="topright", legend=rev(classes), col=rev(col), pch=15)
  }
}
# Example:
#rlcMultiHist(iris$Petal.Length, iris$Species)



# Show cumulative density colored by class.
rlcMultiDens <- function(X, C, bw="nrd0", kernel="gaussian", weights=NULL, col=NULL, main=NULL, xlab=NULL, ylab="Density", add.legend=TRUE, xlim=NULL, showProgress=FALSE, classes=NULL) {
  if (showProgress) {
    progressBarInit()
  }
  if (length(X) != length(C)) stop("X and C must have the same length")
  if (is.null(weights)) {
    W <- 1
    adjWeights <- NULL
  } else {
    W <- sum(weights)
    adjWeights <- weights/W
  }
  tmp <- rlcMultiTranslateClasses(C, classes)
  C <- tmp$C
  classes <- tmp$classes
  cls <- tmp$cls
  xdesc <- deparse(substitute(X))
  if (is.null(xlim)) {
    h <- density(X, weights=adjWeights, bw=bw, kernel=kernel)
  } else {
    h <- density(X, weights=adjWeights, bw=bw, kernel=kernel, from=xlim[1], to=xlim[2])
  }
  h$y <- h$y * W
  M <- matrix(0, nrow=length(cls), ncol=length(h$x))
  i <- 1
  for (cl in cls) {
    keep <- which(C == cl)
    if (is.null(weights)) {
      WL <- length(keep)/length(X)
      adjWeights <- NULL
    } else {
      WL <- sum(weights[keep])
      adjWeights <- weights[keep]/WL
    }
    if (WL > 0) {
      hc <- density(X[keep], weights=adjWeights, bw=h$bw, kernel=kernel, from=h$x[1], to=h$x[length(h$x)])
      stopifnot(all(h$x == hc$x))
      hc$y <- hc$y * WL
      M[i,] <- hc$y
    }
    i <- i + 1
    if (showProgress) {
      progressBarNext(i/length(cls))
    }
  }
  
  if (is.null(main)) {
    mainTitle <- paste("Density of", xdesc)
  } else {
    mainTitle <- main
  }
  if (is.null(xlab)) {
    xlab <- xdesc
  }
  plot(h$x, h$y, main=mainTitle, xlab=xlab, ylab=ylab, ylim=c(0, max(h$y)), type="n")
  
  if (is.null(col)) {
    col <- rainbow(length(cls), v=0.8)
  }
  
  rlcCumLines(M, h$x, col)
  
  lines(h$x, h$y)
  
  if (add.legend) {
    legend(x="topright", legend=rev(classes), col=rev(col), pch=15)
  }
  
  if (showProgress) {
    progressBarEnd()
  }
}
# Example:
#rlcMultiDens(iris$Petal.Length, iris$Species)
#rlcMultiDens(iris$Petal.Length, iris$Species, weights=rep(1/nrow(iris), nrow(iris)))
#rlcMultiDens(iris$Petal.Length, iris$Species, weights=ifelse(iris$Species == "virginica", 2, 1))
#rlcMultiDens(iris$Petal.Length, iris$Species, weights=ifelse(iris$Species == "virginica", 10, 1))



rlcCompareDens <- function(X, C, bw="nrd0", kernel="gaussian", weights=NULL, col=NULL, main=NULL, xlab=NULL, ylab="Density", add.legend=TRUE, xlim=NULL, showProgress=FALSE, ylim=NULL, classes=NULL, n=512) {
  if (showProgress) {
    progressBarInit()
  }
  if (length(X) != length(C)) stop("X and C must have the same length")
  
  tmp <- rlcMultiTranslateClasses(C, classes)
  C <- tmp$C
  classes <- tmp$classes
  cls <- tmp$cls
  
  xdesc <- deparse(substitute(X))

  if (is.character(bw)) {
    # We need to guess which bandwidth to use.
    Nelem <- c()
    for (cl in cls) {
      k <- sum(C == cl)
      Nelem <- c(Nelem, k)
    }
    # Use the class with the most elements
    cl0 <- cls[[which.max(Nelem)]]
    # Let density() find the optimal bandwidth.
    bw <- density(X[C==cl0], bw=bw, kernel=kernel, na.rm=TRUE)$bw
  }
  
  # Guess which xlim to use
  if (is.null(xlim)) {
    r <- range(X, na.rm=TRUE)
    # Use the same rule density() uses:
    xlim <- c(r[[1]] - 3*bw, r[[2]] + 3*bw)
  }
  
  M <- matrix(0, nrow=length(cls), ncol=n)
  ymax <- 0
  ymin <- Inf
  lgd <- c()
  for (i in seq(along=cls)) {
    cl <- cls[[i]]
    keep <- which(C == cl)
    XCL <- X[keep]
    if (is.null(weights)) {
      adjWeights <- NULL
      m <- mean(XCL, na.rm=TRUE)
    } else {
      wcl <- weights[keep]
      adjWeights <- wcl/sum(wcl)
      m <- weighted.mean(XCL, adjWeights, na.rm=TRUE)
    }
    lgd <- c(lgd, classes[[i]])
    if (length(XCL) != 0) {
      lgd[[length(lgd)]] <- paste(lgd[[length(lgd)]], " (mean=", format(m, digits=3), ")", sep="")
      h <- density(XCL, weights=adjWeights, bw=bw, kernel=kernel, from=xlim[[1]], to=xlim[[2]], n=n, na.rm=TRUE)
      M[i,] <- h$y
    }
    ymax <- max(ymax, max(h$y))
    ymin <- min(ymin, min(h$y))
    if (showProgress) {
      progressBarNext(i/length(cls))
    }
  }
  
  if (is.null(main)) {
    mainTitle <- paste("Density of", xdesc)
  } else {
    mainTitle <- main
  }
  if (is.null(xlab)) {
    xlab <- paste("Ntotal =", length(X), "  Bandwidth =", formatC(bw))
  }

  if (is.null(ylim)) {
    ylim <- c(ymin, ymax)
  }
  plot(NULL, NULL, main=mainTitle, xlab=xlab, ylab=ylab, ylim=ylim, type="n", xlim=xlim)
  
  if (is.null(col)) {
    col <- rainbow(length(cls), v=0.8)
  }
  
  for (i in nrow(M):1) {
    lines(h$x, M[i,], col=col[[i]])
  }
  
  if (add.legend) {
    legend(x="topright", legend=lgd, col=col, pch=15)
  }
  
  
  if (showProgress) {
    progressBarEnd()
  }
  
  return(list(classes=classes, col=col, legend=lgd))
}
# Example:
#rlcCompareDens(c(1,1,10,10,10,10,10,10), c(1,1,2,2,2,2,2,2))
#rlcCompareDens(iris$Petal.Length, iris$Species)
#rlcCompareDens(iris$Petal.Length, iris$Species, weights=rep(1/nrow(iris), nrow(iris)))
#rlcCompareDens(iris$Petal.Length, iris$Species, weights=ifelse(iris$Species == "virginica", 2, 1))
#rlcCompareDens(iris$Petal.Length, iris$Species, weights=c(100, rep(1, nrow(iris)-1)))


# Compare the distribution of columns in a matrix
rlcCompareDensMatrix <- function(M, ...) {
  M <- as.matrix(M)
  V <- as.vector(M)
  C <- rep(1:ncol(M), each=nrow(M))
  rlcCompareDens(M, C, classes=colnames(M), ...)
}
# Example:
# rlcCompareDensMatrix(iris[,1:4])

# Nice descriptions of intervals if hist is used with the given breaks
# Assumed right=TRUE (default for hist()).
rlcNiceHistIntervals <- function(breaks, integer.data=FALSE, digits=2) {
  lsign <- c("[", rep("]", length(breaks)-2))
  lnum <- breaks[1:(length(breaks)-1)]
  if (integer.data) {
    # "from k excluded" is like "from k+1 included" with integers
    w <- lsign == "]"
    lnum[w] <- lnum[w] + 1L
    lsign[w] <- "["
  }
  rsign <- rep("]", length(breaks)-1)
  rnum <- breaks[2:length(breaks)]
  s <- paste(lsign, signif(lnum,digits), ",", signif(rnum,digits), rsign, sep="")
  # Replace intervals of the form [x,x] by simply "x"
  w <- lnum == rnum
  s[w] <- paste(lnum[w])
  return(s)
}

# Plot an histogram of a given numeric variable
# split according to a factor variable.
# relative = divide frequencies by each class size
rlcCompareHist <- function(X, C, breaks, col=NULL, classes=NULL, relative=FALSE, add.legend=TRUE) {
  M <- NULL
  tmp <- rlcMultiTranslateClasses(C, classes)
  C <- tmp$C
  classes <- tmp$classes
  cls <- tmp$cls
  n <- 0
  for (i in seq(along=cls)) {
    cl <- cls[[i]]
    h <- hist(X[C == cl], plot=FALSE, breaks=breaks)$count
    n <- length(h)
    if (is.null(M)) {
      M <- matrix(0, nrow=length(cls), ncol=n)
    }
    if (relative) {
      h <- h/sum(h)
    }
    M[i,] <- h
  }
  if (is.null(col)) {
    col <- rainbow(length(cls), v=0.8)
  }
  barplot(M, beside=TRUE,
          col=col,
          cex.names=0.7, 
          names.arg=rlcNiceHistIntervals(breaks, is.integer(X)))
  if (add.legend) {
    legend("topright", legend=classes, fill=col)
  }
}
#rlcCompareHist(iris$Petal.Length, iris$Species, seq(0,8,by=1))



# Functions to display progress bars
progressBarInit <- function() {
  for (i in 1:10) {
    cat(" ")
  }
  for (i in 1:50) {
    cat("_")
  }
  cat("\n")
  for (i in 1:10) {
    cat(" ")
  }
  .progressBarLast <<- 0
}

# x is in [0,1]
progressBarNext <- function(x) {
  if (length(x) == 0) {
    stop("progressBarNext: invalid argument")
  }
  if (x > 1) x <- 1
  while (50*x >= .progressBarLast + 1) {
    cat("X")
    .progressBarLast <<- .progressBarLast + 1
  }
}

progressBarEnd <- function() {
  while (.progressBarLast < 50) {
    cat("X")
    .progressBarLast <<- .progressBarLast + 1
  }
  cat(" done.\n")
}



pvalueStars <- function(pvalues) {
  return(ifelse(pvalues <= 0.001, "***", ifelse(pvalues <= 0.01, "**", ifelse(pvalues <= 0.05, "*", ""))))
}


alternateInt <- function(n) {
  l <- rep(0L, n)
  for (i in 1:n) {
    if (i %% 2 == 1) {
      l[i] <- as.integer(ceiling(i/2))
    } else {
      l[i] <- as.integer(ceiling(n/2) + i/2)
    }
  }
  return(l)
}


alternateColors <- function(n, col=defaultcolors) {
  return(mapColors(1:n, col=col)[alternateInt(n)])
}

mixedColorsNumbers <- function(n) {
  x <- rep(0, length(n))
  for (i in 1:n) {
    x[i] <- (i %% 4)/4 + i/n/4
  }
  return(x)
}

mixedColors <- function(n, col=defaultcolors) {
  return(mapColors(mixedColorsNumbers(n), col=col))
}



# Read CSV file as written by OpenOffice.org with defaults settings.
readOOoCSV <- function(filename) {
  return(read.table(filename, header=TRUE, sep=",", quote="\"", stringsAsFactors=FALSE))
}


# Execute several commands in parallel (or scripts),
# executing at most nproc
# processes at the same time (so we wait for the first nproc
# to finish before starting the next ones).
# If nproc is not provided, the number of CPUs is used.
rlcSystemParallel <- function(commands, nproc=NULL) {
  if (is.null(nproc)) {
    library(parallel)
    nproc <- detectCores()
  }
  # 0 = not started, 1 = waiting for finish, 2 = finished
  states <- rep(0, length(commands))
  finishedFiles <- rep("", length(states))
  while (!all(states == 2)) {
    # Look if some commands have finished
    for (i in which(states == 1)) {
      if (file.exists(finishedFiles[[i]])) {
        states[i] <- 2
      }
    }
    # Do we start a new command?
    if (sum(states == 1) < nproc && sum(states == 0) > 0) {
      startThis <- which(states == 0)[[1]]
      scriptFile <- tempfile(fileext=".sh")
      finishedFiles[[startThis]] <- tempfile(fileext=".txt")
      cat(commands[[startThis]], "\n\n",
          "echo FINISHED >", finishedFiles[[startThis]], "\n",
          file=scriptFile, sep="")
      system2("bash", scriptFile, wait=FALSE)
      states[[startThis]] <- 1
    } else {
      Sys.sleep(0.01)
    }
  }
}



# Call function f() n times and return time it took in seconds.
benchmarkRunNTimes <- function(f, n=100) {
  a <- proc.time()[3]
  i <- 0
  while (i < n) {
    invisible(f())
    i <- i + 1
  }
  b <- proc.time()[3]
  return(b - a)
}



# Smoothing

# Fast smooth based on FFT.
# Makes sense when "outside" X is assumed Y = 0
rlcSmoothNormalFast <- function(X, Y, kernelSD, n=length(X), from=min(X), to=max(X)) {
  if (any(is.na(X)) || any(is.na(Y))) stop("NA not allowed")
  W <- sum(Y)
  dens <- density(X, weights=Y/W, bw=kernelSD, n=n, from=from, to=to)
  dens$y <- dens$y * W
  return(dens)
}

# Same with triangular kernel
rlcSmoothTriangularFast <- function(X, Y, triangleWidth, kernelSD=triangleWidth/(2*sqrt(6)), n=length(X), from=min(X)+kernelSD*(2*sqrt(6)), to=max(X)-kernelSD*(2*sqrt(6))) {
  W <- sum(Y)
  Y <- Y / W
  dens <- density(X, weights=Y, kernel="triangular", bw=kernelSD, n=n, from=from, to=to)
  dens$y <- dens$y * W
  return(dens)
}

# Same with rectangular kernel
rlcSmoothRectangularFast <- function(X, Y, rectangleWidth, kernelSD=rectangleWidth/(2*sqrt(3)), n=length(X), from=min(X)+kernelSD*(2*sqrt(3)), to=max(X)-kernelSD*(2*sqrt(3))) {
  W <- sum(Y)
  Y <- Y / W
  dens <- density(X, weights=Y, kernel="rectangular", bw=kernelSD, n=n, from=from, to=to)
  dens$y <- dens$y * W
  return(dens)
}


# For smoothing where the function is not density-like
# but like measures of a real function at
# some points X.
# range.x and n.points are exclusive with x.points
rlcSmoothNormalAverage <- function(X,
                                   Y,
                                   kernelSD,
                                   range.x=range(X),
                                   n.points=max(100, length(X)),
                                   x.points=NULL) {
  if (is.null(x.points)) {
    ksmooth(X, Y, kernel="normal", bandwidth=4*qnorm(0.75, sd=kernelSD), range.x=range.x, n.points=n.points)
  } else {
    ksmooth(X, Y, kernel="normal", bandwidth=4*qnorm(0.75, sd=kernelSD), range.x=range.x, n.points=n.points, x.points=x.points)
  }
}


# Computes the same thing as ksmooth(), but using density()
# and making a correction, ie. it smoothes the curve but uses
# the fast FFT-based method available in density().
# Advantages to ksmooth: faster and more kernels available.
# Possible drawback: the values are equal only for the first
# few significant digits so if you need very high precision
# you will still need ksmooth().
# kernelSD = the bandwith argument (bw) of density()
#            (ie. the standard deviation of the kernel used)
# kernel, n, from, to correspond to the arguments of
# the same names in density()
rlcSmoothAverageFast <- function(X, Y, kernelSD, kernel="gaussian", n=length(X), from=min(X), to=max(X)) {
  if (any(is.na(X)) || any(is.na(Y))) stop("NA not allowed")
  W <- sum(Y)
  dens <- density(X, weights=Y/W, kernel=kernel, bw=kernelSD, n=n, from=from, to=to)
  xdens <- density(X,             kernel=kernel, bw=kernelSD, n=n, from=from, to=to)
  dens$y <- (dens$y * W) / (xdens$y * length(M$position))
  return(dens)
}


# Return the max of the density when using kernel with kernelSD
# ie. what would be the max if a simple point of weight 1
# was given to density with bw=kernelSD and kernel=kernel
densityMax <- function(kernelSD, kernel="gaussian") {
  if (kernel == "gaussian") {
    return(dnorm(0, sd=kernelSD))
  } else {
    stop()
  }
}
#kernelSD <- 2.3
#ymax <- densityMax(kernelSD)
#plot(density(1:10, weights=c(0,0,0,0,1,0,0,0,0,0), bw=kernelSD))
#abline(h=ymax, col="red")

checkFileExists <- function(files) {
  for (f in files) {
    if (!file.exists(f)) stop(paste("File", f, "not found"))
  }
}

saveDataFrame <- function(dataFrame, file, verbose=TRUE) {
  # Text format
  if (verbose) cat("Saving data to ", file, "...", sep="")
  write.table(dataFrame, file=file, quote=FALSE, row.names=FALSE, sep="\t")
  if (verbose) {
    cat(" done. (", niceFileSize(file), ")\n", sep="")
  }
}

loadDataFrame <- function(file, verbose=TRUE) {
  checkFileExists(file)
  if (verbose) cat("Loading data from ", file, " (", niceFileSize(file), ")...", sep="")
  M <- read.table(file=file, header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE)
  if (verbose) cat(" done.\n", sep="")
  return(M)
}

rlcLoad <- function(file, envir=parent.frame(), verbose=TRUE) {
  if (verbose) cat("Loading ", file, "...", sep="")
  load(file=file, envir=envir)
  if (verbose) cat(" done.\n")
}

rlcLoadExtract <- function(file, varname) {
  tmpEnv <- new.env()
  rlcLoad(file, envir=tmpEnv)
  return(tmpEnv[[varname]])
}

# Load (from .RData file) only the specified variables from the file.
rlcLoadOnly <- function(file, varNameList, envir=parent.frame(), verbose=TRUE) {
  tmpEnv <- new.env()
  rlcLoad(file, envir=tmpEnv, verbose=verbose)
  for (varName in varNameList) {
    if (varName %in% ls(tmpEnv)) {
      envir[[varName]] <- tmpEnv[[varName]]
    }
  }
  invisible()
}

changeYscale <- function(ymin, ymax) {
  par(usr=c(par("usr")[1:2], ymin-0.04*(ymax-ymin), ymax+0.04*(ymax-ymin)))
}


# Plot with 2 lines with a different scale (one on left, one on right)
doublePlot <- function(X1, Y1, X2, Y2, type="l", type2=type, color1="blue", color2="red", xlim=range(c(X1,X2)), xlab="", ylab="", ylab1=ylab, ylab2="", ylim1=NULL, ylim2=NULL, ...) {
  if (length(X1) == 0) stop("X1 is empty")
  if (length(X2) == 0) stop("X2 is empty")
  oldmar <- par("mar")
  par(mar=c(5.1,4.1,4.1,4.1))
  if (is.null(ylim1)) {
    ymin <- min(Y1, na.rm=TRUE)
    ymax <- max(Y1, na.rm=TRUE)
  } else {
    ymin <- ylim1[[1]]
    ymax <- ylim1[[2]]
  }
  plot(X1, Y1, type="n", xlab=xlab, ylab=ylab1, yaxt="n", xaxt="n", col.lab=color1, ...)
  par(usr=c(xlim[[1]], xlim[[2]], ymin-0.04*(ymax-ymin), ymax+0.04*(ymax-ymin)))
  axis(1)
  points(X1, Y1, type=type, col=color1)
  axis(2, col.ticks=color1, col.axis=color1)
  if (is.null(ylim2)) {
    ymin <- min(Y2, na.rm=TRUE)
    ymax <- max(Y2, na.rm=TRUE)
  } else {
    ymin <- ylim2[[1]]
    ymax <- ylim2[[2]]
  }
  if (ymin == ymax) ymax <- ymin+1
  par(usr=c(par("usr")[1:2], ymin-0.04*(ymax-ymin), ymax+0.04*(ymax-ymin)))
  points(X2, Y2, type=type2, col=color2)
  axis(4, col.ticks=color2, col.axis=color2)
  par(mar=oldmar)
  mtext(ylab2, side=4, col=color2)
}
#X <- seq(0, 50, by=0.1)
#doublePlot(X, sin(X), X, cos(X)/2, ylab1="aa", ylab2="bb")


# Plot several lines corresponding to different columns in a matrix.
# First columns is X values
# Next columns are Y values
rlcPlotManyFromMatrix <- function(M, lineColors=NULL, ymin=NULL, ymax=NULL) {
  if (ncol(M) < 2) {
    stop("Matrix must have at least 2 columns (for X and Y1)")
  }
  if (is.null(lineColors)) {
    lineColors <- mapColors(1:(ncol(M)-1), col=clearcolors)
  }
  if (is.null(ymin)) ymin <- min(M[,2:ncol(M)])
  if (is.null(ymax)) ymax <- max(M[,2:ncol(M)])
  plot(M[,1], M[,2], type="l", col=lineColors[[1]], xlab=NA, ylab=NA, ylim=c(ymin,ymax))
  if (ncol(M) > 2) {
    for (j in 3:ncol(M)) {
      lines(M[,1], M[,j], col=lineColors[[j-1]])
    }
  }
}

rlcPlotMany <- function(X, ..., lineColors=NULL) {
  Y <- as.list(substitute(c(...))[-1])
  rlcPlotManyFromMatrix(cbind(X, data.frame(Y)), lineColors=lineColors)
}

#rlcPlotMany((1:100)^2, 1:100, 100:1)

#rlcPlotManyFromMatrix(cbind((1:100)^2, 1:100, 100:1))


# A bit like an histogram, but return an
# array of integers, which for each x in X,
# contains the number of the bin in which it is.
histIndex <- function(X, bins) {
  if (any(X > bins[length(bins)] | X < bins[1])) {
    stop("value outside bins")
  }
  A <- rep(0, length(X))
  for (i in 1:(length(bins)-1)) {
    A <- A + (X >= bins[i])
  }
  return(A)
}
#print(histIndex(c(1, 150, 200, 250, 150, 950, 1000), seq(0,1000, by=100)))


# Given a list [x1,x2,...,xn]
# compute the centers of each [x(i),x(i+1)]
makeHistMids <- function(breaks) {
  n <- length(breaks)
  return((breaks[2:n] + breaks[1:(n-1)])/2)
}


# Check X is sorted
isSorted <- function(X) {
  if (length(X) < 2) return(TRUE)
  for (i in 1:(length(X)-1)) {
    if (X[i] > X[i+1]) return(FALSE)
  }
  return(TRUE)
}


# Approximate a curve (X,Y) with an histogram, ie.
# given a list (= argument breaks) of n increasing values
# we compute n-1 values such that the i-th value
# is the average if Y for which breaks[i] <= X < breaks[i+1]
makeHistGeneralAverageNew <- function(X, Y, breaks, verbose=FALSE) {
  if (length(breaks) < 1) stop("breaks in empty")
  if (!isSorted(X)) {
    if (verbose) cat("Sorting values...\n")
    o <- order(X)
    X <- X[o]
    Y <- Y[o]
    if (!isSorted(X)) stop("sorting error")
  }
  if (verbose) cat("Computing histogram...")
  Ysum <- rep(0, length(breaks)-1)
  Xcount <- rep(0, length(breaks)-1)
  j <- 1
  for (i in 1:(length(breaks)-1)) {
    if (!(breaks[i] < breaks[i+1])) stop("breaks must be in increasing order")
    while (j <= length(X) && X[j] < breaks[i+1]) {
      if (X[j] >= breaks[i]) {
        Ysum[i] <- Ysum[i] + Y[j]
        Xcount[i] <- Xcount[i] + 1
      }
      j <- j + 1
    }
  }
  Ysum <- Ysum / Xcount
  Y[Xcount == 0] <- NA # cannot estimate bins for which no X falls in it
  RES <- list(breaks=breaks, counts=Ysum, mids=makeHistMids(breaks))
  class(RES) <- "histogram"
  if (verbose) cat(" done.\n")
  return(RES)
}

# Provided for backward compatibility
makeHistGeneralAverage <- function(X, Y, breaks, verbose=FALSE) {
  return(makeHistGeneralAverageNew(X, Y, breaks, verbose)$counts)
}

.makeHistGeneralAverage.test <- function() {
  X <- runif(50)
  Y <- cos(X*(2*pi))
  Xunif <- seq(0, 1, by=0.001)
  h <- makeHistGeneralAverage(X, Y, seq(0, 1, by=0.1), verbose=TRUE)
  plot(X, Y, pch=3) # sampled values
  lines(Xunif, cos(Xunif*(2*pi)), type="l", col="blue") # real curve
  lines(h$mids, h$counts, type="o", col="red") # estimation
}






# A peak is a i such that
# X[i] > X[i-1]
# The first j after i such that X[j] != X[i] is such that X[j] < X[i].
# Consequences:
# A plateau between two valleys counts as a peak.
# In this case the first index of the plateau is taken.
# A plateau where it goes up after does not count.
rlcFindPeaksRaw <- function(X) {
  if (length(X) < 3) {
    return(NULL)
  }
  n <- length(X)
  peak <- -1
  indices <- c()
  for (i in 2:n) {
    if (X[i] > X[i-1]) {
      peak <- i
    } else if (X[i] < X[i-1] && peak != -1) {
      indices <- c(indices, peak)
      peak <- -1
    }
  }
  return(indices)
}

# Find extrema
# Returns a data frame with two columns:
# indices: indices of the extrema
# types: types of the extrema, TRUE for a max, FALSE for a min
# values: the values themselves
# If an extrema forms a plateau, it is reported once only.
# The main drawback of this function is that it will for instance
# consider as a peak a value of 1E-15 between two 0,
# even if the order of magnitude of the data is units.
rlcFindExtremaRaw <- function(X) {
  dir <- 0
  extremaIndices <- c()
  extremaTypes <- c()
  n <- length(X)
  for (i in 2:n) {
    if (X[i] > X[i-1]) {
      if (dir == -1) {
        extremaIndices <- c(extremaIndices, i-1)
        extremaTypes <- c(extremaTypes, FALSE)
      }
      dir <- 1
    } else if (X[i] < X[i-1]) {
      if (dir == 1) {
        extremaIndices <- c(extremaIndices, i-1)
        extremaTypes <- c(extremaTypes, TRUE)
      }
      dir <- -1
    }
  }
  return(data.frame(indices=extremaIndices, types=extremaTypes, values=X[extremaIndices]))
}


# Smart method to find extrema. We ignore maxima that
# are not "enough" above their base (surounding minima),
# and the same reversed for minima.
# How much is enough is defined by epsilon,
# default is range of X / 1000.
rlcFindExtrema <- function(X, epsilon=NULL, verbose=FALSE) {
  extrema <- rlcFindExtremaRaw(X)
  if (is.null(epsilon)) {
    r <- range(X, na.rm=TRUE)
    epsilon <- (r[2]-r[1])/1000
  }
  found <- TRUE
  # This implementation is kind of stupid
  # because it is in O(n^2) while O(n)
  # would be possible. But at least
  # it is so simple that we can be sure it is correct.
  while (found && nrow(extrema) >= 3) {
    i <- 1
    found <- FALSE
    while (!found && i < (nrow(extrema)-1)) {
      # Do we delete i and i+1?
      if (abs(extrema$values[i] - extrema$values[i+1]) < epsilon) {
        extrema <- extrema[c(-i, -(i+1)),]
        found <- TRUE
      }
      i <- i + 1
    }
  }
  return(extrema)
}


#X <- c(3,2,1,0,1,2,3,2,1,2,3,3,3,3,2,1,1,2,3,2.91,3,4,4,5,5,4,3,3.2,2,2.05,1,0,0.09,0,0.5,0,0,5)
#extrema <- rlcFindExtrema(X, epsilon=0.1, verbose=TRUE)
#plot(X, type="l")
#points(extrema$indices, extrema$values, col=ifelse(extrema$types, "red", "blue"))



# Smart method to find peaks We ignore peaks that
# are not "enough" above their base (surounding minima).
# How much is enough is defined by epsilon,
# default is range of X / 1000.
rlcFindPeaks <- function(X, epsilon=NULL) {
  extrema <- rlcFindExtrema(X, epsilon=epsilon)
  return(extrema[extrema$types, "indices"])
}



#X <- c(3,2,1,0,1,2,3,2,1,2,3,3,3,3,2,1,1,2,3,2.91,3,4,4,5,5,4,3,3.2,2,2.05,1,0,0.09,0,0.5,0,0,5)
#plot(X,type="l")
#peaks <- rlcFindPeaks(X, epsilon=0.1)
#points(peaks, X[peaks], col="red")





testEval <- function(expr, expectedResult) {
  cat("Test:", deparse(substitute(expr)), "==", expectedResult)
  cat(" -> ")
  if (expr != expectedResult) {
    cat("********* FAIL: got", expr)
  } else {
    cat("OK")
  }
  cat("\n")
}

# Remove extensions in a list of strings
rlcRemoveExt <- function(sl) {
  RES <- rep("", length(sl))
  for (i in 1:length(sl)) {
    s <- sl[[i]]
    pos <- gregexpr(".", s, fixed=TRUE)[[1]]
    pos <- pos[[length(pos)]]
    if (pos < 0) {
      RES[[i]] <- s
    } else {
      RES[[i]] <- substring(s, 1, pos-1)
    }
  }
  return(RES)
}
#print(rlcRemoveExt(c("abc.txt", "abc.tar.gz", "abc.hmtl", "abc")))


rlcNearlyEqual <- function(X, Y, relativeError=1E-5) {
  return(abs(X - Y)/pmax(abs(X), abs(Y)) < relativeError)
}
#X <- c(40, 1E10, 20)
#Y <- c(40.00000001, 1E10+1, 20.05)
#print(X == Y)
#print(rlcNearlyEqual(X, Y))


# Function to help create the bin ranges for an histogram.
# n = number of bins
# returns the breakpoints between the bins
# (what hist() expects as the breaks parameter)
rlcMakeBreaks <- function(x, n) {
  stopifnot(n >= 2)
  r <- range(x, na.rm=TRUE)
  a <- r[1]
  b <- r[2]
  return(c(a, a + (1:(n-1))/n*(b-a), b))
}



rlcHeatMatrix <- function(X, Y, xbreaks=10, ybreaks=10, col=defaultcolors, xlab=NULL, ylab=NULL, ...) {
  if (is.null(xlab)) {
    xlab <- deparse(substitute(X))
  }
  if (is.null(ylab)) {
    ylab <- deparse(substitute(Y))
  }
  hx <- hist(X, breaks=rlcMakeBreaks(X, xbreaks), plot=FALSE)
  hy <- hist(Y, breaks=rlcMakeBreaks(Y, ybreaks), plot=FALSE)
  hxi <- histIndex(X, hx$breaks)
  hyi <- histIndex(Y, hy$breaks)
  nx <- length(hx$mids)
  ny <- length(hy$mids)
  M <- matrix(0, nrow=nx, ncol=ny)
  for (i in 1:nx) {
    for (j in 1:ny) {
      M[i,j] <- sum(hxi == i & hyi == j)
    }
  }
  #cat(nx, ny, "\n")
  r <- range(M, na.rm=TRUE)
  image(hx$breaks, hy$breaks, M, col=col, xlab=xlab, ylab=ylab, zlim=r, ...)
  showColorScale(colors=col, text=signif(round(seq(r[1], r[2], length.out=5), 1), 3))
}
#X <- seq(-1,1, by=0.01)
#rlcHeatMatrix(X, X*X, xbreaks=100, ybreaks=5)



# For the current plot, takes a position as a number in [0,1]
# and return the position as units of the current plot.
# Example: if range is [100,200], passing 0.5 gives 150.
# Also work for <0 and >1 for out-of-plot things.
usrFromRelativeX <- function(x) {
  usr <- par("usr")
  return(x * (usr[2] - usr[1]) + usr[1])
}

# Same for Y
usrFromRelativeY <- function(y) {
  usr <- par("usr")
  return(y * (usr[4] - usr[3]) + usr[3])
}

# Like rank(), but return a number between [0,1]
rank01 <- function (x, na.last = TRUE, ties.method = c("average", "first", 
                                                       "random", "max", "min")) {
  if (is.matrix(x)) {
    return(matrix(rank01(as.vector(x)), nrow=nrow(x), ncol=ncol(x)))
  } else {
    if (length(x) == 0) {
      return(NULL)
    } else if (length(x) == 1) {
      return(0)
    } else {
      res <- rank(x, na.last, ties.method)
      return((res - 1)/(length(x)-1))
    }
  }
}

stringToCharArray <- function(s) {
  return(strsplit(s, "", fixed=TRUE)[[1]])
}

charArrayToString <- function(a) {
  return(paste(a, collapse=""))
}

removeSpaces <- function(x) {
  return(gsub("(^\\s+|\\s+$)", "", x))
}

# Is "newer" (strictly) more recent than "older"?
fileIsMoreRecent <- function(newer, older) {
  if (!file.exists(newer)) stop(paste("File", newer, "does not exist"))
  if (!file.exists(older)) stop(paste("File", older, "does not exist"))
  newer.info <- file.info(newer)
  older.info <- file.info(older)
  return(newer.info$mtime > older.info$mtime)
}

source.if.exists <- function(file, ...) {
  if (file.exists(file)) {
    source(file, ...)
  }
}

# If (x,y) is a cruve, that you want to draw with
# lines(x,y), then this function will remove points that
# unecessary because they are aligned horizontaly.
simplifyXYcurveHoriz <- function(x, y) {
  stopifnot(length(x) == length(y))
  N <- length(x)
  if (N >= 3) {
    keep <- c(TRUE, y[2:(N-1)] != y[1:(N-2)] | y[2:(N-1)] != y[3:N], TRUE)
    return(list(x=x[keep], y=y[keep]))
  } else {
    return(list(x=x, x=y))
  }
}


# If (x,y) is a curve, that you want to draw with
# lines(x,y), then this function will remove points that
# unecessary because they are almost aligned.
simplifyXYcurveApprox <- function(x, y, precisionInDegrees=0.1) {
  stopifnot(length(x) == length(y))
  N <- length(x)
  if (N >= 3) {
    # Round slopes to 1 degree
    slopes <- round(atan(diff(y)/diff(x))/pi*180/precisionInDegrees)
    # Remove points that are "nearly" aligned
    keep <- c(TRUE, (diff(slopes) != 0), TRUE)
    return(list(x=x[keep], y=y[keep]))
  } else {
    return(list(x=x, x=y))
  }
}


# If (x,y) is a curve, that you want to draw with
# lines(x,y), then this function will remove points that
# unecessary because they are aligned.
simplifyXYcurveExact <- function(x, y) {
  stopifnot(length(x) == length(y))
  N <- length(x)
  if (N >= 3) {
    slopes <- diff(y)/diff(x)
    # Remove points that are aligned
    keep <- c(TRUE, (diff(slopes) != 0), TRUE)
    return(list(x=x[keep], y=y[keep]))
  } else {
    return(list(x=x, x=y))
  }
}
# Example: simplifyXYcurveExact(1:10, c(1:4, 7:2))

# Remove extension of file name(s)
removeExt <- function(filenames) {
  parts <- strsplit(filenames, split=".", fixed=TRUE)
  return(sapply(parts, function(x) paste(x[1:(length(x)-1)], collapse=".")))
}


# Plot a matrix on a colored table where the colors represent the 
plotColoredMatrix <- function(M, row.names=rownames(M), col.names=colnames(M), col=rev(heat.colors(1000)), ...) {
  image(t(M[nrow(M):1,]), xaxt="n", yaxt="n", col=col, ...)
  for (i in 1:nrow(M)) {
    text(x=seq(0,1,length.out=ncol(M)), y=(nrow(M)-i)/(nrow(M)-1), labels=M[i,])
  }
  if (!is.null(row.names) && length(row.names) > 0) {
    axis(side=2, at=seq(nrow(M)-1, 0)/(nrow(M)-1), labels=row.names, lwd=0, las=1)
  }
  if (!is.null(col.names) && length(col.names) > 0) {
    axis(side=1, at=seq(0, ncol(M)-1)/(ncol(M)-1), labels=col.names, lwd=0, las=2)
  }
  return(invisible(NULL))
}
# Example:
#mymat <- matrix(as.integer(runif(20, min=0, max=10)), 4, dimnames=list(c("Anatole","Bris","Celine","Daniel"), c("chien","chat","souris","hamster","lapin")))
#print(mymat)
#plotColoredMatrix(mymat, main="test")
