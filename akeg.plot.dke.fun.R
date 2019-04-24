require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
# creation de la fenetre d'acceuil
rm(list=ls())

akeg.plot.dke.fun<-function(){
  
  #-----------Fenetre principale--------------
  win8<-tktoplevel()
  tkwm.title(win8,"dke.fun: Function for density estimation")
  tkwm.resizable(win8,FALSE,FALSE)
  tkwm.geometry(win8,"850x500+230+80")
  tkwm.minsize(win8,200,100)
  
  #fonction quitter
  quitter=function(){
    quit=tkmessageBox(title="Info...",message="VOULLEZ-VOUS FERMER CETTE FENETRE ?",
                      icon="question", type="yesno")
    if(tclvalue(quit)=="yes"){
      tkdestroy(win3)
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
  frm1 <- tk2frame(win8, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "nsew",rowspan=2,row=1,column=0)
  
  frm0 <- tk2frame(win8,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "n",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "PLOT OF DENSITY FUNCTION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "n",columnspan=4,row=0,column=0)
  
  
  #-------------Les labels---------------
  # label_arguments<-tk2label(frm1, text = "Arguments", justify = "center",font=police.label)
  label_vec<-tk2label(frm1, text = "Data sample (Vec) : ", justify = "left",font=police.label)
  zone_saisie_vec1 <- tclVar("100")
  zone_saisie_vec2 <- tclVar("1.5")
  zone_saisie_vec3 <- tclVar("2.6")
  saisie_vec1 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec1,font=police.label)
  saisie_vec2 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec2,font=police.label)
  saisie_vec3 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec3,font=police.label)
  
  label_type_data<-tk2label(frm1, text = "Data sample type : ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("continuous","discrete")
  select_type_data <- tclVar("continuous")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label)
  
  label_ker<-tk2label(frm1, text = "Smoothing kernel : ", justify = "left",font=police.label)
  tab_ker<- c("GA","BE","LN", "RIG")
  select_ker<- tclVar("GA")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 
  
  label_h<-tk2label(frm1, text = " Bandwidth(h) : ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.052")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 
  
  
  #géneration du vecteur vec
  vec_fun <- function(){
    # print("-------------------Vecteur des données---------------------")
    v1<- as.numeric(tclvalue(zone_saisie_vec1))
    v2<- as.numeric(tclvalue(zone_saisie_vec2))
    v3<- as.numeric(tclvalue(zone_saisie_vec3))
    v<-rgamma(v1,v2,v3)
  }
  
  #Fonction dke.fun
  plot.dk.fun <-function(){
    rm(list=ls())
    v<-vec_fun()
    h<- as.numeric(tclvalue(zone_saisie_h))
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    source("dke.fun.R")
    x<-dke_fun(v,h,type_data,ker)
    xlab=NULL
    ylab=NULL
    main=NULL
    sub = NULL
    type="l"
    las=1
    lwd=1
    col="blue"
    lty=1
    class(x) <- "dke.fun"
    
    if(is.null(xlab)) xlab <- "x"
    if(is.null(ylab)){
      ylab <- "density function"
    }
    if(is.null(main)){ 
      main <- "Kernel density estimate"
      
    }
    if(is.null(sub)){
      if(x$kernel=="GA") kerf="gamma"
      else if(x$kernel=="BE") 		kerf= "extended beta"
      else if(x$kernel=="LN") 		kerf= "lognormal"
      else if(x$kernel=="RIG") 		kerf= "reciprocal inverse Gaussian"
      
      sub <- paste("Kernel = ",kerf,";", " h_n = ", formatC(x$h),";"," C_n = ",formatC(x$C_n))
    }  
    
    histo <- function(){
      his<-hist(x$data,xlab=xlab,ylab=ylab,sub=sub,probability=TRUE,main=main,border ="gray" )
      line<-lines(x$eval.points,x$est.fn,lwd=2,col=col,ylim=c(min(x$est.fn,na.rm = TRUE),max(x$est.fn,na.rm = TRUE)))
    }
    
    hist(x$data,xlab=xlab,ylab=ylab,sub=sub,probability=TRUE,main=main,border ="gray" )
    lines(x$eval.points,x$est.fn,lwd=2,col=col,ylim=c(min(x$est.fn,na.rm = TRUE),max(x$est.fn,na.rm = TRUE)))
    
    tkgrid(tkrplot(win8,fun=histo),padx = 5,pady = c(5, 10), sticky = "w",columnspan=2,rowspan=2,row=1,column=2)
    #tkgrid(tkrplot(win8,fun=histo),padx = 5,pady = c(5, 10), sticky = "w",columnspan=2,row=2,column=0)
   
    invisible(NULL)
  }
  
  
  bouton_dkeplotfun<-tkbutton(frm1,width = "35", text = "plot.dke.fun",font=police.bouton, command =plot.dk.fun)
  
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
  
  tkgrid(bouton_dkeplotfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=0)
  
  
}

