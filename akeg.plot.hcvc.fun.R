require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
# creation de la fenetre d'acceuil
rm(list=ls())

akeg.plot.hcvc.fun<-function(){
  
  #-----------Fenetre principale--------------
  win9<-tktoplevel()
  tkwm.title(win9,"plot.hcvc.fun: Plot of cross-validation function for bandwidth selection in density or p.m.f. estimation.")
  tkwm.resizable(win9,FALSE,FALSE)
  tkwm.geometry(win9,"900x500+230+80")
  tkwm.minsize(win9,200,100)
  
  
  #fonction quitter
  quitter=function(){
    quit=tkmessageBox(title="Info...",message="VOULLEZ-VOUS FERMER CETTE FENETRE ?",
                      icon="question", type="yesno")
    if(tclvalue(quit)=="yes"){
      tkdestroy(win9)
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
  frm1 <- tk2frame(win9, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "n",row=1,column=0)
  
  frm0 <- tk2frame(win9,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "s",columnspan=6,row=0,column=0)
  tkgrid(tk2label(frm0, text = "PLOT OF CROSS-VALIDATION FUNCTION FOR BANDWIDTH SELECTION IN DENSITY OR P.M.F ESTIMATION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "S",columnspan=4,row=0,column=0)
  
  
  frm2 <- tk2frame(win9, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm2,padx = 5, pady = c(5, 5), sticky = "n",row=3,column=0)
  
  
  label_vec<-tk2label(frm1, text = "Data sample (Vec) : ", justify = "left",font=police.label)
  zone_saisie_vec1 <- tclVar("100")
  zone_saisie_vec2 <- tclVar("1.5")
  zone_saisie_vec3 <- tclVar("2.6")
  saisie_vec1 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec1,font=police.label)
  saisie_vec2 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec2,font=police.label)
  saisie_vec3 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec3,font=police.label)
  
  label_type_data<-tk2label(frm1, text = "Data sample type: ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("continuous","discrete")
  select_type_data <- tclVar("continuous")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "Smoothing kernel : ", justify = "left",font=police.label)
  tab_ker<- c("GA","BE","LN", "RIG")
  select_ker<- tclVar("GA")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 
  
  label_h<-tk2label(frm1, text = "Bandwidth(h) : ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.052")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 
  
  
  #géneration du vecteur vec
  vec_fun <- function(){
    v1<- as.numeric(tclvalue(zone_saisie_vec1))
    v2<- as.numeric(tclvalue(zone_saisie_vec2))
    v3<- as.numeric(tclvalue(zone_saisie_vec3))
    v<-rgamma(v1,v2,v3)
  }
  
  #Fonction dke.fun
  plot.hcvc.fun1d <-function(){
    rm(list=ls())
    V<-vec_fun()
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    source("hcvc.fun.R")
    f<-hcvc_fun(V,NULL,type_data,ker)
    main=NULL
    sub = NULL
    xlab=NULL
    ylab=NULL
    type="l"
    las=1
    lwd=1
    class(f) <- "hcvc.fun"
    kernel <- f$ker
    if(is.null(xlab)) xlab <- "h"
    if(is.null(ylab)) ylab <- "CV(h)" 
    if(is.null(main)){ 
      
      main <- "Cross-validation"
    } 
    plotTk <- function(){
      plot.default(f$seq_h,f$CV, type="l",las=las,lwd=lwd,xlab=xlab,ylab=ylab,
                   main=main,sub=sub,font.main=2,cex.main=0.9,font.sub=2,cex.sub=0.7)
    }
    pl <- tkrplot(win9, fun = plotTk)
    
    tkgrid(pl, sticky = "nsew",columnspan=4,rowspan=2,row=1,column=1)
    
    plot.default(f$seq_h,f$CV, type="l",las=las,lwd=lwd,xlab=xlab,ylab=ylab,
                 main=main,sub=sub,font.main=2,cex.main=0.9,font.sub=2,cex.sub=0.7)
    invisible(NULL)
  }
  
  
  bouton_hcvcplotfun<-tkbutton(frm1,width = "35", text = "plot.hcvc.fun",font=police.bouton,command =plot.hcvc.fun1d)
  
  #Positionnement
  tkgrid(label_vec, padx = 5, pady = c(5, 10),sticky = "w", row=0,column=0)
  tkgrid(saisie_vec1, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=1)
  tkgrid(saisie_vec2, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=2)
  tkgrid(saisie_vec3, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=3)
  #tkgrid(bouton_vec, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=4)
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=2,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=2,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=3,column=0)
  tkgrid(combo_type_data,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=3,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=4,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=4,column=1)
  
  tkgrid(bouton_hcvcplotfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=0)
  
  
}

