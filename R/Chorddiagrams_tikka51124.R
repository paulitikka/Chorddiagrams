# Making a package for Steroid evaluations. Tikka, 1.11.24.

#https://ourcodingclub.github.io/tutorials/writing-r-package/
# https://docs.posit.co/ide/user/ide/guide/pkg-devel/writing-packages.html

# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))a

#' Chord diagram
#'
#' Converts data to forestplots.
#' Written by Pauli Tikka. University of Turku. Updated 4.11.24.
#' @param vars The dataset with steroids and cases
#' @param gend All, Female, Male (typically a gender column)
#' @param n_level The case (e.g. steatosis). Typically a column name in the dataset
#' @param fig_name An header add (of the case) for the forestplot's header
#' @param oute The label title. Here, almost the same as case.
#' @return The forestplots. Yes.
#' @examples
#' Outcome='Steatosis.Grade.0.To.3';Out='Steatosis'; oute='Steatosis';first=TRUE; e='P4';ordera=c();
#' Group='All';name1=paste("Forest plot of",Group, "Steroid Ratios in",Out);ordera=forestplots(NAFLD,Outcome,Group,name1,ordera,oute,first,e,xlim)
#' Afterwards: first=FALSE;
#' @export
# Some further definitions are needed, yes, yes, yes aa

# This works with the autoscaled (raw if loge=1 and remove 1 in the means) data NAFLD as well...


group_chords=function(vars,n_level,fig_name, big,rem,modi,colt,gend,colors,a,b,c,d,e,f) {

  classes=5;
  tot=rownames(resulta)[2:dim(resulta)[1]];
  range=1:(a+b+c+e+f)
  layout(matrix(1:1, 1, 1));
  title='Sex'#
  genders= gend
  windowsFonts(A = windowsFont("Calibri (Body)"))
  i=1
  tes_t=vars #vars[[1]]
  if (gend=='All') {colnames(tes_t)=rownames(resulta);rownames(tes_t)=rownames(resulta)} else {colnames(tes_t)=rownames(resultaf);rownames(tes_t)=rownames(resultaf)}

  g1=c(rep('Clinical', a),rep('Steroids', b), rep('BA_liver', c),rep('Contaminants', e),rep('Lipids', f)) #rep('BA_serum', d)

  # removing self-correlation;
  tes_t[1:a,1:a]=0
  tes_t[(a+1):(a+b),(a+1):(a+b)]=0
  tes_t[(a+b+1):(a+b+c),(a+b+1):(a+b+c)]=0
  tes_t[(a+b+c+1):(a+b+c+e),(a+b+c+1):(a+b+c+e)]=0
  tes_t[(a+b+c+e+1):(a+b+c+e+f),(a+b+c+e+1):(a+b+c+e+f)]=0 #if you have more groups... make this automatic, now it is not (18.1.24), yes:
  # https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html#orders-of-links


  group = structure(g1, names = colnames(tes_t));#group
  grid.col = structure(c(rep('#93c29f', a),rep('#a83277', b), rep('red', c),  rep('grey', e), rep('black', f)),
                       # c(rep('blue', a),rep('red', b), rep('green', c),  rep('orange', e), rep('#756BB1', f)),
                       #(c(rep('#3283a8', a),rep('#a83277', b), rep('#a86932', c),  rep('#32a898', e), rep('#756BB1', f)),
                       names = rownames(tes_t)); ##https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
  # insert 'color hex' in google

  tes_t=tes_t[range,range];grid.col = grid.col[range] #tes_t=resulta
  g <- graph.adjacency(tes_t, mode="upper", weighted=TRUE, diag=FALSE)
  e <- get.edgelist(g); df <- as.data.frame(cbind(e,E(g)$weight)); #
  df[,3]=as.numeric(df[, 3])

  # Edge.Thickness <- (abs(Correlation.Strength) * Thickness.Multiplier)

  # rango <- function(x){((x-min(x))/(max(x)-min(x)))*2-1} #just a function for the -1 to 1 thing..
  rango <- function(x){((x-min(x))/(max(x)-min(x)))*2} #just a function for the -1 to 1 thing..
  # if (gend=='All') {col_fun = colorRamp2(c(-0.8, 0, 0.8), c("blue",'white', "orange"))} else {col_fun = colorRamp2(c(-1, 0,1), c("blue",'white', "orange"))} #
  col_fun=colorRamp2(c(-1, 0,1), c("blue",'white', "orange"),transparency = 0.25)
  # col_fun = colorRamp2(range(mat), c("#FFEEEE", "#FF0000"), transparency = 0.5)
  # if (gend=='All') {col_fun = colorRamp2(c(min(df$V3), 0,max(df$V3)), c("blue",'white', "orange"))} else {col_fun = colorRamp2(c(min(df$V3), 0,max(df$V3)), c("blue",'white', "orange"))}
  df=df[!df$V1 %in% rem,];df=df[!df$V2 %in% rem,] #e.g.rem=x4
  # df$V3=rango(df$V3);


  # for (i in 1:2) {
  # df[,i]=  gsub("\\.", "-", df[,i])
  # df[,i] <- gsub("X11", "11", df[,i])
  # df[,i] <- gsub("X17", "17", df[,i]); df[,i][df[,i]=="T-Epi-T"]="T/Epi-T"
  # df[,i][df[,i]=="Steatosis.Grade"]="Steatosis Grade"
  # df[,i][df[,i]=="Steatosis-Grade"]="Steatosis Grade"
  # df[,i][df[,i]=="Fibrosis.Stage"]="Fibrosis Stage"
  # df[,i][df[,i]=="Fibrosis-Stage"]="Fibrosis Stage"
  # df[,i][df[,i]=="17aOH.P4"]="17a-OHP4"
  # df[,i][df[,i]=="HOMA.IR"]="HOMA-IR"
  # # df[,i][df[,i]=="Gender"]="Sex"

  # }

  classes=modi #modi=4
  namesh=unique(g1)    #[c(1:6)[1:6 != modi]];
  cola=unique(grid.col)#[c(1:6)[1:6 != modi]]

  if (gend=='All') {
    ccu=df[,3] > 0.2
    ccu2=df[,3] < -0.2
    cx=ccu | ccu2} else {
      ccu=df[,3] > 0.25
      ccu2=df[,3] < -0.25
      cx=ccu | ccu2}

  # lgd_group = Legend(at = gend, type = "points", legend_gp = gpar(col = colors),  title_position = "topleft", title = title)
  #
  # lgd_points = Legend(at = namesh, type = "points", legend_gp = gpar(col = cola), title_position = "topleft", title = "Class")
  # lgd_lines = Legend(at = c("Positive", "Negative"), type = "points", legend_gp = gpar(col = c('orange','blue')), title_position = "topleft", title = "Correlation")
  # lgd_edges= Legend(at = c(-1,1), col_fun = col_fun,  title_position = "topleft", title = "Edges") #lgd_edges= Legend(at = c(round(min(df$V3),1), round(max(df$V3),1)), col_fun = col_fun,  title_position = "topleft", title = "Edges")
  # lgd_list_vertical = packLegend(lgd_group,lgd_points,  lgd_lines,lgd_edges) #lgd_lines,
  # # circos.par(gap.after = grid.col)
  #   chordDiagram(df, annotationTrack = c("grid"),  grid.col=grid.col, directional = FALSE,symmetric = TRUE, scale=FALSE, #link.decreasing = FALSE,
  #                link.lwd = 0.3, link.border = "white",
  #                # link.arr.width=abs(df[,3])*rango(df[,3])*50, #this is for arrows
  #                order = rownames(tes_t), preAllocateTracks = 1, col = col_fun,transparency = 0.25,big.gap = 10, small.gap = 1, #link.zindex = rank(df[,3]),
  #                link.visible = cx)
  #   circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  #     xlim = get.cell.meta.data("xlim"); ylim = get.cell.meta.data("ylim")
  #     sector.name = get.cell.meta.data("sector.index")
  #     circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  #     circos.axis(h = "top", labels.cex = 0.000001, major.tick.length = 0.2, sector.index = sector.name, track.index = 2)}, bg.border = NA) #https://stackoverflow.com/questions/31943102/rotate-labels-in-a-chorddiagram-r-circlize
  #   windowsFonts(A = windowsFont("Calibri (Body)"))
  #   draw(lgd_list_vertical, x = unit(5, "mm"), y = unit(5, "mm"), just = c("left", "bottom"))#}
  #   # These ten above lines have been already done, and now just show separately
  #   dev.copy(jpeg,paste0(gend,'hie_0.01.jpg'),width=9, height=12, units="in", res=1000);dev.off() # This is already done

  knitr::include_graphics(paste0(gend,'hie_0.01.jpg'))
}






#
# library(roxygen2); # Read in the roxygen2 R package
# gc()
# roxygenise();      # Builds the help files; read these separately not in a project I guess....
