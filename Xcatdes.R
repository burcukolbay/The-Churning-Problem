#Xcatdes: Show interactions of categorical variables that associate with
#         modalities of a target variable
# Input:
# targettable - table
# targetcolnum - column number of the target variable
# degree - number of variables constituting an interaction
# alpha - level of significance (default: 0.05)
#
# Output:
# Summary Table
#  Note:
#  Var# variables - Variables in the interaction
#  X# variables - Values corresponding to Var variables
library("sqldf")

Xcatdes <- function(targettable,targetcolnum,degree,alpha=0.05){
  ## factor_find: input data frame and return indices of the factors in the frame
  factor_find <- function(frame){
    n <- ncol(frame)
    seq <- 1:n
    factor_locs <- sapply(1:ncol(frame), function(x) is.factor(frame[,x]))
    return(seq[factor_locs])
  }
  
  # Initialize variables
  varnames <- colnames(targettable)
  churn_df <- targettable
  target <- targetcolnum
  locs <- factor_find(targettable)                   #factor locations
  loc_names <- colnames(targettable)[factor_find(targettable)] #names
  x_cands <- setdiff(locs,target)
  deg <- degree
  combin2 <- t(combn(x_cands, deg))
  targname <- varnames[target]
  n <- nrow(targettable)
  
  # If target column is a factor, imitate FactoMineR.catdes
  if (is.factor(targettable[,target])){
    table.setup_cat <- function(x){
      lab <- varnames[x]
      
      # Query Renaming function
      rename_query <- function(x){
        x <- gsub("##jl.selection##",jl.selection,x)
        x <- gsub("##jl.group##",jl.group,x)
        x <- gsub("##sumtab.selection##",sumtab.selection,x)
        x <- gsub("##join.cond##",join.cond,x)
        x <- gsub("##sumtab.group##",sumtab.group,x)
        x <- gsub("##VAR0##",targname,x)
        for (i in 1:deg){
          x <- gsub(paste("##VAR",as.character(i),"##",sep=""),lab[i],x)
        }
        x <- gsub("##ORIGTAB##",deparse(substitute(targettable)),x)
        return(x)
      }
      
      jl.selection <- ""
      for (i in 1:deg){
        jl.selection <- paste(jl.selection,"##VAR",as.character(i),"##, ",sep="")
      }
      jl.group <- substr(jl.selection, 1, nchar(jl.selection)-2)
      pred_tab <- data.frame(table(targettable[,1]))
      jl.query <- "SELECT ##jl.selection## COUNT(*) AS njl 
      FROM ##ORIGTAB##
      GROUP BY ##jl.group##"
      
      sumtab.selection <- "a.##VAR0## as target, "
      for (i in 1:deg){
        sumtab.selection <- paste(sumtab.selection,"a.##VAR",as.character(i),"## as Var",as.character(i),", ",sep="")
      }
      join.cond <- ""
      for (i in 1:deg){
        join.cond <- paste(join.cond,"a.##VAR",as.character(i),"##=jl.##VAR",as.character(i),"## AND ",sep="")
      }
      join.cond <- substr(join.cond, 1, nchar(join.cond)-4)
      sumtab.group <- ""
      for (i in 0:deg){
        sumtab.group <- paste(sumtab.group,"a.##VAR",as.character(i),"##, ",sep="")
      }
      sumtab.group <- substr(sumtab.group, 1, nchar(sumtab.group)-2)
      jl.query <- rename_query(jl.query)
      jl<-sqldf(jl.query)
      sumtab.query <- "SELECT ##sumtab.selection##
      b.Freq AS nk, jl.njl, COUNT(*) AS nkjl
      FROM ##ORIGTAB## a 
      left join
      (select var1, Freq from pred_tab) b on (##VAR0##=Var1)
      left join
      (select * from jl) jl on (##join.cond##)
      GROUP BY ##sumtab.group##"
      sumtab.query <- rename_query(sumtab.query)
      sumtab<-sqldf(sumtab.query)
      numcol <- ncol(sumtab)
      for (i in 1:deg){
        sumtab[,numcol+i] <- lab[i]
        colnames(sumtab)[numcol+i] <- paste("X",as.character(i),sep="")
      }
      sumtab$group <-""
      for (i in 1:deg){
        sumtab$group <-paste(sumtab$group,sumtab[,i+1+3+deg],' = "',sumtab[,i+1],'" AND ',sep="")
      }
      sumtab$group <- substr(sumtab$group,1,nchar(sumtab$group)-4)
      
      # Compute summary columns
      sumtab$ClaMod <- sumtab$nkjl / sumtab$njl *100
      sumtab$ModCla <- sumtab$nkjl / sumtab$nk *100
      sumtab$Global <- sumtab$njl / n *100
      sumtab$v.test <- 2/sqrt(n-1)*(sqrt(sumtab$nkjl+1)*sqrt(n-sumtab$nk-sumtab$njl+sumtab$nkjl+1)-sqrt(sumtab$nk-sumtab$nkjl)*sqrt(sumtab$njl-sumtab$nkjl))
      sumtab$p.value <- 1-pnorm(abs(-sumtab$v.test))
      return(sumtab)
    }
    summary.table<-do.call(rbind,apply(combin2,1,table.setup_cat))
    filtered <- subset(summary.table, p.value <= alpha/2)
    filtered <- filtered[with(filtered,order(target, -v.test)),]
  }
  # If target column is numeric, imitate FactoMineR.condes
  else{
    table.setup_con <- function(x){
      lab <- varnames[x]
      
      # Rename Query
      rename_query <- function(x){
        x <- gsub("##jl.selection##",jl.selection,x)
        x <- gsub("##jl.group##",jl.group,x)
        x <- gsub("##sumtab.selection##",sumtab.selection,x)
        x <- gsub("##join.cond##",join.cond,x)
        x <- gsub("##sumtab.group##",sumtab.group,x)
        x <- gsub("##VAR0##",targname,x)
        for (i in 1:deg){
          x <- gsub(paste("##VAR",as.character(i),"##",sep=""),lab[i],x)
        }
        x <- gsub("##ORIGTAB##",deparse(substitute(targettable)),x)
        return(x)
      }
      jl.selection <- ""
      for (i in 1:deg){
        jl.selection <- paste(jl.selection,"##VAR",as.character(i),"##,",sep="")
      }
      jl.group <- substr(jl.selection, 1, nchar(jl.selection)-2)
      jl.query <- "SELECT ##jl.selection## COUNT(*) AS njl, AVG(##VAR0##) as xbar_jl
      FROM ##ORIGTAB##
      GROUP BY ##jl.group###"
      
      sumtab.selection <- ""
      for (i in 1:deg){
        sumtab.selection <- paste(sumtab.selection,"a.##VAR",as.character(i),"## as Var",as.character(i),", ",sep="")
      }
      join.cond <- ""
      for (i in 1:deg){
        join.cond <- paste(join.cond,"a.##VAR",as.character(i),"##=jl.##VAR",as.character(i),"## AND ",sep="")
      }
      join.cond <- substr(join.cond, 1, nchar(join.cond)-4)
      sumtab.group <- ""
      for (i in 0:deg){
        sumtab.group <- paste(sumtab.group,"a.##VAR",as.character(i),"##, ",sep="")
      }
      sumtab.group <- substr(sumtab.group, 1, nchar(sumtab.group)-2)
      jl.query <- rename_query(jl.query)
      sumtab<-sqldf(jl.query)
      numcol <- ncol(sumtab)
      for (i in 1:deg){
        colnames(sumtab)[i] <- paste("Var",as.character(i),sep="")
        sumtab[,numcol+i] <- lab[i]
        colnames(sumtab)[numcol+i] <- paste("X",as.character(i),sep="")
      }
      sumtab$group <-""
      for (i in 1:deg){
        sumtab$group <-paste(sumtab$group,sumtab[,i+2+deg],' = "',sumtab[,i],'" AND ',sep="")
      }
      sumtab$group <- substr(sumtab$group,1,nchar(sumtab$group)-4)
      
      # Summary Column Computation
      xbar <- mean(targettable[,targetcolnum])
      sumtab$v.test <- (sumtab$xbar_jl-xbar)/sqrt((n-sumtab$njl)/(n-1)*sd(targettable[,targetcolnum])^2/sumtab$njl)
      sumtab$p.value <- 1-pt(abs(sumtab$v.test),df=n-1)
      return(sumtab)
    }
    summary.table<-do.call(rbind,apply(combin2,1,table.setup_con))
    
    # Remove non-significant interactions
    filtered <- subset(summary.table, p.value <= alpha/2)
    filtered <- filtered[with(filtered,order(-v.test)),]
  }
  return(filtered)
}
