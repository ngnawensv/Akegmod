##########################################################
#-------------- Chargement des packages-----------------#
#########################################################
require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")


akeg.dke.fun<-function(){
  
  ##########################################################
  #-------------- Fenêtre principale-----------------#
  #########################################################
  win1<-tktoplevel()
  tkwm.title(win1,"dke.fun: Function for density estimation") #definition du titre de la fenetre
  tkwm.resizable(win1,FALSE,FALSE)       # redimentionnement de la fenetre
  tkwm.geometry(win1,"1200x650+80+30")   #Taille et position de la fenetre
  tkwm.minsize(win1,200,100) 
  
  
  ##########################################################
  #-------------- Fonction quitter-----------------#
  #########################################################
  quitter=function(){
    quit=tkmessageBox(title="Info...",message="VOULLEZ-VOUS FERMER CETTE FENETRE ?",
                      icon="question", type="yesno")
    if(tclvalue(quit)=="yes"){
      tkdestroy(win3)
      source("akeg.package.R")
      akeg.package()
    }
    
  }
  
  ##########################################################
  #-------------- Mises en forme-----------------#
  #########################################################
  police.bouton <- tkfont.create(family="arial", size=12,weight="bold")
  police.label <- tkfont.create(family="arial", size=10,weight="bold")
  police.label1 <- tkfont.create(family="arial", size=11)
  police.label2 <- tkfont.create(family="arial", size=12,weight="bold")
  police.label3 <- tkfont.create(family="arial", size=10,weight="bold",underline=T)
  police.menu <- tkfont.create(family="arial", size=11, weight="bold")
  police.titre  <- tkfont.create(family="arial", size=18, weight="bold",slant="italic")
  police.titre1  <- tkfont.create(family="arial", size=14, weight="bold",slant="italic")
  police.sous.titre <- tkfont.create(family="arial", size=12,underline=F)
  
  ##########################################################
  #-------------- Les frames -----------------#
  #########################################################
  frm1 <- tk2frame(win1, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "nsew",rowspan=3,row=0,column=0)
  
  frm0 <- tk2frame(frm1,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "n",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "FUNCTION FOR DENSITY ESTIMATION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "n",columnspan=4,row=0,column=0)
  
  
  ##########################################################
  #--------------Les labels-----------------#
  #########################################################
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
 
  ##########################################################
  #-------------- Generateur du vecteur-----------------#
  #########################################################
  vec_fun <- function(){
    v1<- as.numeric(tclvalue(zone_saisie_vec1))
    v2<- as.numeric(tclvalue(zone_saisie_vec2))
    v3<- as.numeric(tclvalue(zone_saisie_vec3))
    v<-rgamma(v1,v2,v3)
  }
  
  
  ##########################################################
  #-------------------- Fonction dke.fun-----------------#
  #########################################################
  dke.fun <- function(Vec,type_data,ker,h,x,a0,a1,...){
    rm(list=ls())
    x=NULL
    a0=0
    a1=1
    Vec<-vec_fun()
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    h<- as.numeric(tclvalue(zone_saisie_h))
      n <- length(Vec)
      if(is.null(x)) {
        x=seq(min(Vec),max(Vec),length.out=100)
      }
      aux <- matrix(data=Vec,nrow=length(x),ncol=length(Vec),byrow=TRUE)
      #for(i in 1:n){
      #	aux[i,]= kef(x[i],vec_data,bw,ker,a,b)
      # 	   }
      if (type_data == "continuous"){
        source("kef.R")
        aux <- kef(x,aux,h,type_data,ker,a0,a1)
        res<- apply(aux,1,mean)	 # density without normalization
        source("simp_int1.R")
        C<-simp_int(x,res)	 # Normalizant constant 
        result<-res/C 		 # density normalized
        
        
        #The data - same as input Vec(data=Vec)
        tkgrid(tk2label(win1, text = "THE DATA SAMPLE", justify = "center",font=police.label3),
               padx = 5, pady = c(5, 10), sticky = "s",row=1,column=1)
        frm3 <- tk2frame(win1, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=1)
        scrx <- tk2scrollbar(frm3, orient = "horizontal",command = function(...) tkxview(txt, ...))
        scry<- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt, ...))
        txt <- tk2text(frm3, bg = "white",font = "courier", width = 20, height = 22,
                       xscrollcommand = function(...) tkset(scrx, ...),
                       yscrollcommand = function(...) tkset(scry, ...))
        tkgrid(txt,scry, sticky = "nsew")
        tkgrid.rowconfigure(frm3, txt, weight = 1)
        tkgrid.columnconfigure(frm3,txt, weight = 1)
        tkgrid(scrx, sticky = "nsew")
        for (i in Vec)
          tkinsert(txt, "end", paste0(i, "\n"))
        tkconfigure(txt, state = "disabled")
        tkfocus(txt)
        
        #The coordinates of the points where the density is estimated.
        tkgrid(tk2label(win1, text = "THE COORDINATES OF THE POINTS", justify = "center",font=police.label3),
               padx = 5, pady = c(5, 10), sticky = "s",row=1,column=2)
        frm4 <- tk2frame(win1, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm4,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=2)
        scr1 <- tk2scrollbar(frm4, orient = "vertical",command = function(...) tkyview(txt1, ...))
        txt1 <- tk2text(frm4, bg = "white",font = "courier", width = 20, height = 22,yscrollcommand = function(...) tkset(scr1, ...))
        tkgrid(txt1,scr1, sticky = "nsew")
        tkgrid.rowconfigure(frm4, txt1, weight = 1)
        tkgrid.columnconfigure(frm4,txt1, weight = 1)
        for (i in x)
          tkinsert(txt1, "end", paste0(i, "\n"))
        tkconfigure(txt1, state = "disabled")
        tkfocus(txt1)
        
        #The estimated density values(est.fn=result)
        tkgrid(tk2label(win1, text = "THE ESTIMATED DENSITY VALUES", justify = "center",font=police.label3),
               padx = 5, pady = c(5, 10), sticky = "s",row=1,column=3)
        frm5 <- tk2frame(win1, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm5,padx = 5, pady = c(5, 5), sticky = "n",rowspan=4,row=2,column=3)
        scr2 <- tk2scrollbar(frm5, orient = "vertical",command = function(...) tkyview(txt2, ...))
        txt2 <- tk2text(frm5, bg = "white",font = "courier", width = 20, height = 22,yscrollcommand = function(...) tkset(scr2, ...))
        tkgrid(txt2,scr2, sticky = "nsew")
        tkgrid.rowconfigure(frm5, txt2, weight = 1)
        tkgrid.columnconfigure(frm5,txt2, weight = 1)
        for (i in result)
          tkinsert(txt2, "end", paste0(i, "\n"))
        tkconfigure(txt2, state = "disabled",font=police.label2)
        tkfocus(txt2)
        
        
        #The histogram corresponding to the observations (hist=hist(Vec,prob=TRUE))
        histo <- function(){
          his<-hist(Vec,prob=TRUE)
        }
        tkgrid(tkrplot(win1,fun = histo),padx = 5,pady = c(5, 10), sticky = "n",row=3,column=0)
        
        
        # The global normalizing constant (C_n=C)
        frm6 <- tk2frame(win1, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm6,padx = 5, pady = c(5, 5), sticky = "n",columnspan=3,row=0,column=1)
        label_n <- tclVar("")
        label_cn <- tclVar("")
        label_kernel <- tclVar("")
        tclvalue(label_n) <-length(Vec)
        tclvalue(label_cn) <-C
        tclvalue(label_kernel) <-ker
        label_cn_affich <- tk2label(frm6, textvariable = label_cn,font=police.label)
        tkgrid(tk2label(frm6, text = "The sample size(n) : ", justify = "left",font=police.label1),padx = 5,pady = c(5, 10), sticky = "w",row=2,column=0)
        tkgrid(tk2label(frm6, textvariable = label_n, justify = "left",font=police.label),padx = 5,pady = c(5, 10), sticky = "w",row=2,column=1)
        tkgrid(tk2label(frm6, text = "The asssociated kernel used(kernel): ", justify = "left",font=police.label1),padx = 5,pady = c(5, 10), sticky = "w",row=3,column=0)
        tkgrid(tk2label(frm6, textvariable = label_kernel, justify = "left",font=police.label),padx = 5,pady = c(5, 10), sticky = "w",row=3,column=1)
        tkgrid(tk2label(frm6, text = "The global normalizing constant(c_n) : ", justify = "left",font=police.label1),padx = 5,pady = c(5, 10), sticky = "w",row=4,column=0)
        tkgrid(label_cn_affich,padx = 5,pady = c(5, 10), sticky = "w",row=4,column=1)
        
        
        msg<-(structure(list(data=Vec,n=length(Vec),hist=hist(Vec,prob=TRUE),eval.points= x,h=h, kernel=ker,C_n=C,est.fn=result),class="dke.fun"))
        print(msg)
      }else{
        tkmessageBox(title="Info...", message = "VOTRE TYPE DE DONNEES N'EST PAS CONTINUE",font=police.titre1)
      }
    
  }
  
  bouton_dkefun<-tkbutton(frm1,width = "35", text = "dke.fun",font=police.bouton, command =local(function()dke.fun(Vec,type_data,ker,h)))
  
  #Positionnement
  tkgrid(label_vec, padx = 5, pady = c(5, 10),sticky = "w", row=1,column=0)
  tkgrid(saisie_vec1, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=1)
  tkgrid(saisie_vec2, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=2)
  tkgrid(saisie_vec3, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=3)
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=3,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=3,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=4,column=0)
  tkgrid(combo_type_data,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=4,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=5,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=1)
  
  tkgrid(bouton_dkefun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=5, row=6,column=0)
  
  
}