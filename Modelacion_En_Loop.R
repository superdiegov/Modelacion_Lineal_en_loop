########EJEMPLO PISOS 72 - 45 - 40  ################################################################################
########################################################################################################
library(lpSolve)
rm(list=ls(all=TRUE))
Metas_AICHI<-read.table(file.choose(),header=T)
Metas_AICHI

run <-list.files(path = "C:\\Users\\dvalencia\\Documents\\GitHub\\Modelacion_Lineal_en_loop\\raw", pattern = NULL, all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = FALSE)
run
maxrec<-length(run)

t=1

path = "C:/Users/dvalencia/Documents/GitHub/Modelacion_Lineal_en_loop/Salida_"

while (t < (maxrec + 1)) {
  
  data<-read.table(run[t], header=T)
  
  nr <- toString(t)
  txt = ".txt"
  outpath <- paste(path, nr, txt, sep="")
  
  
  f.obj<-(data$Costo*data$Sup_ha)+(data$DistPoly*data$DistAPP*data$DistAPP)
  n_x<-length(data$Parche)
  Diagonal<-diag(n_x)
  f.con<-rbind(data$Sup_ha, Diagonal, deparse.level = 0)            
  f.dir<-rep (c(">=", "<="), c(1, n_x))
  f.rhs<-rep (c(Metas_AICHI[t,2], 1), c(1, n_x))
  n_x
  
  Result<-lp("min", f.obj, f.con, f.dir, f.rhs, num.bin.solns=TRUE)$solution
  Result
  
  Parches<-data$Parche
  Parches
  
  Tabla<-data.frame(Parches,Result)
  write.csv(Tabla,file=outpath)
  
  
  t = t + 1
}




