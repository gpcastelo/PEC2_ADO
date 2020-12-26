plotPCA <- function ( X, labels=NULL, colors=NULL, dataDesc="", scale=FALSE)
{
  # Gráfico de componentes principales PCA
  # Queremos detectar agrupaciones 'naturales',
  # es decir, entre muestras del mismo grupo
  # o si no hay correspondencia clara y proximidad
  # (esto puede indicar defectos técnicos).
  pcX<-prcomp(t(X), scale=scale) # o prcomp(t(X))
  loads<- round(pcX$sdev^2/sum(pcX$sdev^2)*100,1)
  xlab<-c(paste("PC1",loads[1],"%"))
  ylab<-c(paste("PC2",loads[2],"%"))
  if (is.null(colors)) colors=1
  plot(
    pcX$x[,1:2],xlab=xlab,ylab=ylab, col=colors,
    xlim=c(min(pcX$x[,1])-10, max(pcX$x[,1])+10),
    ylim=c(min(pcX$x[,2])-10, max(pcX$x[,2])+10),
  )
  text(pcX$x[,1],pcX$x[,2], labels, pos=3, cex=0.8)
  title(paste("Plot of first 2 PCs for expressions in", dataDesc, sep=" "), cex=0.8)
}
