#call the function, no need to change anything in the next few lines
biplot_values<-function(data, # data frame, it must have at least two columns with the values of the variables you want to plot. Each row is a cell of your raster
                        minvaluex, #the minimum value of the variable you want to plot on the x axis
                        minvaluey, #the minimum value of the variable you want to plot on the y axis
                        maxvaluex, #the maximum value of the variable you want to plot on the x axis
                        maxvaluey, #the maximum value of the variable you want to plot on the y axis
                        col.ll, #vector of color that will be used in the bottom left corner of your plot
                        col.ul,#vector of color that will be used in the upper left corner of your plot
                        col.lr,#vector of color that will be used in the bottom right corner of your plot
                        col.ur,
                        cells.x, #number of cells you want in the x axis to show your gradient
                        cells.y,#number of cells you want in the y axis to show your gradient
                        c.x = NULL,
                        c.y = NULL) 
  
{ 
  n <- cells.x * cells.y
  
  cols <- rep(NA, n)
  
  z<-1
  for (j in 1:(n/cells.y)){
    w2 <- (j - 1) / ((n/cells.y) - 1)
    v<-(((1 - w2) * col.ll + w2 * col.lr))
    cols[z] <- rgb(v[1], v[2], v[3])
    z<-z +cells.y
  }
  
  z<-cells.y
  for (j in 1:(n/cells.y)){
    w2 <- (j - 1) / ((n/cells.y) - 1)
    v2<-(((1 - w2) * col.ul + w2 * col.ur))
    cols[z] <- rgb(v2[1], v2[2], v2[3])
    z<-z +cells.y
  }
  
  #First column
  
  z<-1
  for (j in 1:(cells.y)){
    w2 <- (j - 1) / ((cells.y) - 1)  
    v2<-(((1 - w2) * col.ll + w2 * col.ul))
    cols[z] <- rgb(v2[1], v2[2], v2[3])
    z<-z +1
  }
  
  #last column
  
  z<-n - cells.y + 1
  for (j in 1:cells.y){
    w2 <- (j - 1) / (cells.y - 1)
    v2<-(((1 - w2) * col.lr + w2 * col.ur))
    cols[z] <- rgb(v2[1], v2[2], v2[3])
    z<-z +1
  }
  
  
  for (y in 2:(cells.x - 1)){
    #z<-(cells.x * (y - 1)) + 1
    z<-(cells.y * (y - 1)) + 1
    for(j in 1:cells.y){
      col.l<-as.numeric(col2rgb(cols[(cells.y * (y - 1)) + 1])/255)
      col.u<-as.numeric(col2rgb(cols[(cells.y * (y - 1)) + cells.y])/255)
      w2 <- (j - 1) / (cells.y - 1)
      v2<-(((1 - w2) * col.l + w2 * col.u))
      cols[z] <- rgb(v2[1], v2[2], v2[3])
      z<-z +1
    }
    #image_plot<-image(matrix(c(1:n), nrow=cells.x, ncol=cells.y, byrow=TRUE), col=cols, axes=F)
  }
  
  # #function to save plot
  # z1<-rbeta(10000,5,5)
  # z2<-rbeta(10000,20,20)
  # plotit<-function(vector,alpha,beta){
  #   plot(density(vector),xlim=c(0,1))
  #   abline(v=alpha/(alpha+beta),lty="longdash")
  # }
  # 
  # plotit<-function(n, cells.x,cells.y,cols){
  #   image(matrix(c(1:n), cells.x, cells.y, byrow=TRUE), col=cols, axes=F)
  # }
  # 
  # z.plot1<-function(){plotit(n,cells.x,cells.y,cols)}
  # z.plot1()
  
  # then plot the coordinates on top of this grid
  
  if(is.null(c.y)){
  c.y <- seq(minvaluey, maxvaluey, length=cells.y +1)
  c.y<-c.y[2:(cells.y +1)]
  }
  
  if(is.null(c.x)){
    c.x <- seq(minvaluex, maxvaluex, length = cells.x + 1)
    c.x<-c.x[2:(cells.x +1)]
  }
  
  cols.df<-expand.grid(c.y,c.x)
  cols.df$color<-cols
  cols.df$rastvalue<-c(1:nrow(cols.df))
  #cols.df$tomultiply<-rep(1:cells.y, each = nrow(cols.df)/cells.y)
  cols.df$tomultiply<-rep(1:cells.x,each = cells.y)
  
  extrarow<-data.frame(maxvaluex + 1,maxvaluey + 1,NA,NA,NA)
  colnames(extrarow)<-colnames(cols.df)
  
  cols.df<-rbind(cols.df,extrarow)
  
  cols.df
  
  pb <- txtProgressBar(min = 0, max = nrow(cols.df)-1, style = 3) #To check the progress
  colorgrid<-list()
  
  for (i in 1:(nrow(cols.df)-1))
  {
    setTxtProgressBar(pb, i)
    if(i == 1){
      colorgrid[[i]]<-subset(data,data[3] <=cols.df[i ,"Var1"] & 
                               data[4] <= cols.df[i ,"Var2"])
      range(colorgrid[[i]][4])
      range(colorgrid[[i]][3])
      #print("hola")
    }else{
      if(cols.df[i,"tomultiply"]==1){
        if(cols.df[i ,"Var1"]>cols.df[i -1 ,"Var1"]){
          colorgrid[[i]]<-subset(data,data[3] <=cols.df[i ,"Var1"] & 
                                   data[3]>cols.df[i - 1,"Var1"] &
                                   data[4] <= cols.df[i,"Var2"])
          
          range(colorgrid[[i]][4])
          range(colorgrid[[i]][3])
          #print("1")
        }else{
          colorgrid[[i]]<-subset(data,data[3] <cols.df[i + 1,"Var1"] & 
                                   data[4] < cols.df[1 + (cells.y)*cols.df[i,"tomultiply"],"Var2"]&
                                   data[4] > cols.df[1 + (cells.y)*(cols.df[i, "tomultiply"]-1),"Var2"]) 
          #print("2")
        }
      }else{
        if(cols.df[i ,"Var1"]>cols.df[i -1 ,"Var1"]){
          minval<-subset(cols.df,cols.df$tomultiply==cols.df[i,"tomultiply"]-1)["Var2"][[1]][1]
          colorgrid[[i]]<-subset(data,data[3] <=cols.df[i ,"Var1"] & 
                                   data[3]>cols.df[i - 1,"Var1"] &
                                   data[4] <= cols.df[i,"Var2"]&
                                   data[4] > minval)
          #print("3")
        }else{
          minval<-subset(cols.df,cols.df$tomultiply==cols.df[i,"tomultiply"]-1)["Var2"][[1]][1]
          colorgrid[[i]]<-subset(data,data[3] <=cols.df[i,"Var1"] & 
                                   data[4] <= cols.df[i,"Var2"]&
                                   data[4] > minval)
          #print("estas")
        }
      }
    }
    if (nrow(colorgrid[[i]])>0){
      colorgrid[[i]]$color<-cols.df[i,"color"]
      colorgrid[[i]]$rastvalue<-cols.df[i,"rastvalue"]
    }else{
      colorgrid[[i]]$color<-as.character()
      colorgrid[[i]]$rastvalue<-as.numeric()
    }
  }
  return(list(do.call(rbind,colorgrid),cols.df[1:n,1:4]))
}

#EXTRACT VALUES OF RASTERS HERE
