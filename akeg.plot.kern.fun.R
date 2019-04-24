require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
# creation de la fenetre d'acceuil
rm(list=ls())

akeg.plot.kern.fun<-function(){
  
  #-----------Fenetre principale--------------
  win10<-tktoplevel()
  tkwm.title(win10,"plot.kern.fun:  Plot of associated kernel function")
  tkwm.resizable(win10,FALSE,FALSE)
  tkwm.geometry(win10,"850x500+230+80")
  tkwm.minsize(win10,200,100)
 
  
  #fonction quitter
  quitter=function(){
    quit=tkmessageBox(title="Info...",message="VOULLEZ-VOUS FERMER CETTE FENETRE ?",
                      icon="question", type="yesno")
    if(tclvalue(quit)=="yes"){
      tkdestroy(win10)
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
  frm1 <- tk2frame(win10, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "n",row=2,column=0)
  
  frm0 <- tk2frame(win10,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "s",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "PLOT OF ASSOCIATED KERNEL FUNCTION", 
                  justify = "center", font=police.titre),padx = 5,pady = c(5, 10), sticky = "s",columnspan=4,row=0,column=0)
  
  frm2 <- tk2frame(win10, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm2,padx = 5, pady = c(5, 5), sticky = "n",row=3,column=0)
  
  
  #Les labels
  label_t<-tk2label(frm1, text = "A single value or the grid(t): ", justify = "left",font=police.label)
  zone_saisie_t0 <- tclVar("0")
  zone_saisie_tN <- tclVar("10")
  saisie_t0 <-tk2entry(frm1, width = "10", textvariable =zone_saisie_t0,font=police.label)
  saisie_tN <-tk2entry(frm1, width = "10", textvariable =zone_saisie_tN,font=police.label)
  
  label_type_data<-tk2label(frm1, text = "The sample data type : ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("discrete", "continuous")
  select_type_data <- tclVar("discrete")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "The associated kernel : ", justify = "left",font=police.label)
  tab_ker<- c("bino","triang","dirDU","BE","GA","LN","RIG")
  select_ker<- tclVar("bino")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 
  
  label_h<-tk2label(frm1, text = "Smoothing parameter(h) : ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.2")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 
  
  label_x<-tk2label(frm1, text = "The target(x) : ", justify = "left",font=police.label)
  zone_saisie_x <- tclVar("5")
  saisie_x<-tk2entry(frm1, width = "23", textvariable =zone_saisie_x,font=police.label) 
  
  #géneration du vecteur vec
  vec_fun <- function(){
    t0<-as.integer(tclvalue(zone_saisie_t0))
    tN<-as.integer(tclvalue(zone_saisie_tN))
    tt<-t0:tN
  }
  
  #Fonction kern_fun
  plot.hcvc.fun1d <-function(){
    rm(list=ls())
    t<-vec_fun()
    xx<-as.numeric(tclvalue(zone_saisie_x))
    h<- as.numeric(tclvalue(zone_saisie_h))
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    source("kern.fun.R")
    x<-kern.fun(xx,t,h,type_data,ker)
    main=NULL
    sub = NULL
    xlab=NULL
    ylab=NULL
    pch=18
    las=1
    lwd=1
    class(x) <- "kern_fun"
    kernel <- x$ker
    if(is.null(xlab)) xlab <- "y"
    if(is.null(ylab)) ylab <- "Prob(y)" 
    if(is.null(main)){ main <- "Kernel function"}
    
    plotTk <- function(){
      plot.default(x$t,x$kx,pch=pch,las=las,lwd=lwd,xlab=xlab,ylab=ylab,
                   main=main,sub=sub,font.main=2,cex.main=0.9,font.sub=2,cex.sub=0.7)
    }
    
    pl <- tkrplot(win10, fun = plotTk)
    
    tkgrid(pl, sticky = "nsew",columnspan=4,rowspan=4,row=1,column=1)
    plot.default(x$t,x$kx,pch=pch,las=las,lwd=lwd,xlab=xlab,ylab=ylab,
                 main=main,sub=sub,font.main=2,cex.main=0.9,font.sub=2,cex.sub=0.7)
  }
  
  
  bouton_kernplotfun<-tkbutton(frm1,width = "35", text = "plot.kern.fun",font=police.bouton,command =plot.hcvc.fun1d )
  
  #Positionnement
  tkgrid(label_t, padx = 5, pady = c(5, 10),sticky = "w", row=0,column=0)
  tkgrid(saisie_t0, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=1)
  tkgrid(saisie_tN, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=2)
  
  tkgrid(label_x,padx = 5, pady = c(5, 10), sticky = "w", row=2,column=0)
  tkgrid(saisie_x,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=2,column=1)
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=3,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=3,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=4,column=0)
  tkgrid(combo_type_data, padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=4,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=5,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=1)
  
  tkgrid(bouton_kernplotfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=0)
  
  
}

