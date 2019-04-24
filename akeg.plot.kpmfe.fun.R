require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
# creation de la fenetre d'acceuil
rm(list=ls())

akeg.plot.kpmfe.fun<-function(){
  
  #-----------Fenetre principale--------------
  win11<-tktoplevel()
  tkwm.title(win11,"plot.kern.fun:  Plot of associated kernel function")
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
  police.titre1  <- tkfont.create(family="arial", size=14, weight="bold",slant="italic")
  police.sous.titre <- tkfont.create(family="arial", size=12,underline=F)
  
  #-------------Les Frames------------------------------
  frm0 <- tk2frame(win11,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "n",columnspan=6,row=0,column=0)
  tkgrid(tk2label(frm0, text = "PLOT OF THE FUNCTION FOR ASSOCIATED KERNEL ESTIMATION OFTHE P.M.F", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "s",columnspan=4,row=0,column=0)
  
  frm1 <- tk2frame(win11, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "nsew",row=1,column=0)
  
  
  #Les labels
  label_type_data<-tk2label(frm1, text = "Data sample type : ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("discrete", "continuous")
  select_type_data <- tclVar("discrete")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "The associated kernel: ", justify = "left",font=police.label)
  tab_ker<- c("bino","triang","dirDU")
  select_ker<- tclVar("bino")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 
  
  label_h<-tk2label(frm1, text = "Bandwidth(h): ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.081")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 
  
  #géneration du vecteur vec
  vec_fun <- function(){
    V<-c(10,0,1,0,4,0,6,0,0,0,1,1,1,2,4,4,5,6,6,6,6,7,1,7,0,
         7,7,7,8,0,8,12,8,8,9,9,0,9,9,10,10,10,10,0,10,10,11,
         12,12,10,12,12,13,14,15,16,16,17,0,12)
  }
  
  #Fonction kern_fun
  plot.kpmfe.fun1d <-function(){
    rm(list=ls())
    V<-vec_fun()
    h<- as.numeric(tclvalue(zone_saisie_h))
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    source("kpmfe.fun.R")
    x<-kpmfe.fun(V,h,type_data,ker)
    print(x)
    main=NULL
    sub = NULL
    xlab=NULL
    ylab=NULL
    type="h"
    las=1
    lwd=1
      class(x) <- "kpmfe.fun"
      kernel <- x$ker
      if(is.null(xlab)) xlab <- "y"
      if(is.null(ylab)) ylab <- "Prob(y)" 
      if(is.null(main)){ 
        
        main <- "Discrete kernel estimaton"
      }   
      
      plotTk <- function(){
        plot.default(x$eval.points,x$est.fn, type="h",las=las,lwd=lwd,xlab=xlab,ylab=ylab,
                     main=main,sub=sub,font.main=2,cex.main=0.9,font.sub=2,cex.sub=0.7)
      }
      pl <- tkrplot(win11, fun = plotTk)
      
      tkgrid(pl, sticky = "w",columnspan=4,rowspan=4,row=1,column=2)
      
      plot.default(x$eval.points,x$est.fn, type="h",las=las,lwd=lwd,xlab=xlab,ylab=ylab,
                   main=main,sub=sub,font.main=2,cex.main=0.9,font.sub=2,cex.sub=0.7)
      
    }
  
  bouton_kpmfeplotfun<-tkbutton(frm1,width = "35", text = "Plot.kpmfe.fun",font=police.bouton, command =plot.kpmfe.fun1d)
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=2,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=2,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=3,column=0)
  tkgrid(combo_type_data, padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=3,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=4,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=4,column=1)
  
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=2,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=3,column=0)
  tkgrid(combo_type_data,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=3,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=4,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=4,column=1)
  
  tkgrid(bouton_kpmfeplotfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=0)
  
  
}

