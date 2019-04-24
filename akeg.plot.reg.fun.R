require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
require(Ake) || stop("Package Ake non disponible.")
# creation de la fenetre d'acceuil
rm(list=ls())

akeg.plot.reg.fun<-function(){
  
  #-----------Fenetre principale--------------
  win11<-tktoplevel()
  tkwm.title(win11,"plot.reg.fun: Plot for associated kernel regression")
  tkwm.resizable(win11,FALSE,FALSE)
  tkwm.geometry(win11,"850x500+230+80")
  tkwm.minsize(win11,200,100)
  
  #fonction quitter
  quitter=function(){
    quit=tkmessageBox(title="Info...",message="VOULLEZ-VOUS FERMER CETTE FENETRE ?",
                      icon="question", type="yesno")
    if(tclvalue(quit)=="yes"){
      tkdestroy(win11)
      source("akeg.package.R")
      akeg.package()
    }
    
  }
  
  #-----------Mise en forme--------------
  police.bouton <- tkfont.create(family="arial", size=12,weight="bold")
  police.label <- tkfont.create(family="arial", size=10,weight="bold")
  police.label1 <- tkfont.create(family="arial", size=11)
  police.label2 <- tkfont.create(family="arial", size=12,weight="bold")
  police.label3 <- tkfont.create(family="arial", size=10,weight="bold",underline=T)
  police.menu <- tkfont.create(family="arial", size=11, weight="bold")
  police.titre  <- tkfont.create(family="arial", size=18, weight="bold",slant="italic")
  police.titre1  <- tkfont.create(family="arial", size=13, weight="bold",slant="italic")
  police.sous.titre <- tkfont.create(family="arial", size=12,underline=F)
  
  #-------------Les Frames------------------------------
  frm1 <- tk2frame(win11, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "n",row=2,column=0)
  
  frm0 <- tk2frame(win11,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "s",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "PLOT FOR ASSOCIATED KERNEL ESTIMATION OF REGRESSION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "s",columnspan=4,row=0,column=0)
  
  
  label_type_data<-tk2label(frm1, text = "The sample data type : ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("discrete", "continuous")
  select_type_data <- tclVar("discrete")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "The associated kernel : ", justify = "left",font=police.label)
  tab_ker<- c("bino","triang","dirDU","BE","GA","LN","RIG")
  select_ker<- tclVar("bino")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label)
  
  label_h<-tk2label(frm1, text = "Smoothing parameter(h) : ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.10")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 
  
  
  #géneration du vecteur vec
  vec_fun <- function(){
    data(milk)
    list(xx=milk$week,yy=milk$yield)
  }
  
  #Fonction kern_fun
  plot.reg.fun1d <-function(){
    rm(list=ls())
    Vec<-vec_fun()
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    h<-as.numeric(tclvalue(zone_saisie_h))
    x=Vec$xx
    y=Vec$yy
    source("reg.fun.R")
    f<-reg.fun(x,y,type_data,ker,h)
    main=NULL
    sub = NULL
    xlab=NULL
    ylab=NULL
    pch=18
    las=1
    lwd=1
      class(f) <- "reg.fun"
      kernel <- f$ker
      if(is.null(xlab)) xlab <- "x"
      if(is.null(ylab)) ylab <- "y" 
      if(is.null(main)){main <- "kernel regression"}
      
      plotTk <- function(){
        plot.default(f$data,f$y,pch=pch,col="grey",las=las,lwd=lwd,xlab=xlab,ylab=ylab,
                     main=main,sub=sub,font.main=2,cex.main=0.9,font.sub=2,cex.sub=0.7)
      }
      pl <- tkrplot(win11, fun = plotTk)
      
      tkgrid(pl, sticky = "w",columnspan=4,rowspan=4,row=1,column=2)
      
      plot.default(f$data,f$y,pch=pch,col="grey",las=las,lwd=lwd,xlab=xlab,ylab=ylab,
                   main=main,sub=sub,font.main=2,cex.main=0.9,font.sub=2,cex.sub=0.7)
      
      
  }
  
  
  
  print.reg.fun <-function(){
    rm(list=ls())
    Vec<-vec_fun()
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    h<-as.numeric(tclvalue(zone_saisie_h))
    xx=Vec$xx
    y=Vec$yy
    source("reg.fun.R")
    x<-reg.fun(xx,y,type_data,ker,h)
    digits=NULL
    class(x) <- "reg.fun"
    if(x$kernel=="GA") 		kerf="gamma"
    else if(x$kernel=="BE") 	kerf= " extended beta "
    else if(x$kernel=="LN") 	kerf= " lognormal"
    else if(x$kernel=="RIG")	kerf= " reciprocal inverse Gaussian"
    else if(x$kernel=="bino") 	kerf=" Binomial"
    else if(x$kernel=="triang") 	kerf= " Triangular "
    else if(x$kernel=="dirDU") 	kerf= "DiracDU"
    cat("\nBandwidth h:",formatC(x$h,digits=digits), "\tCoef_det = ",x$Coef_det,"\n",
        "\nNumber of points: ",x$n,";","\tKernel = ",kerf, "\n\n",sep="")
    print(summary(as.data.frame(x[c("data","y")])), digits=digits)
    print(summary(as.data.frame(x[c("eval.points","m_n")])), digits=digits)
    #invisible(x)
  }
  
  bouton_regplotfun<-tkbutton(frm1,width = "35", text = "plot.reg.fun",font=police.bouton, command = plot.reg.fun1d)
  bouton_printregfun<-tkbutton(frm1,width = "35", text = "print.reg.fun",font=police.bouton, command = print.reg.fun)
  
  #Positionnement
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=3,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=3,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=4,column=0)
  tkgrid(combo_type_data,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=4,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=5,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=1)
  tkgrid(bouton_regplotfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=6,column=0)
  tkgrid(bouton_printregfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=7,column=0)
  
  
}

