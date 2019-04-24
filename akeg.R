require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
# creation de la fenetre d'acceuil
rm(list=ls())

akeg_dke_fun<-function(){
  
  #-----------Fenetre principale--------------
  win1<-tktoplevel()
  tkwm.title(win1,"dke.fun: Function for density estimation")
  tkwm.resizable(win1,FALSE,FALSE)
  tkwm.geometry(win1,"1300x700+20+5")
  #tkwm.minsize(win1,200,100)
  
  #Creation des
  police.bouton <- tkfont.create(family="arial", size=12)
  police.titre  <- tkfont.create(family="arial", size=18, weight="bold")
  police.sous.titre <- tkfont.create(family="arial", size=14,underline=T)
  
  #-------------Les Frames------------------------------
  frm <- tk2frame(win1, borderwidth = 5,relief = "sunken",width = 20, height = 13,padding = 10)
  tkgrid(frm,padx = 5, pady = c(5, 5), sticky = "w", row=1,column=0)
  frm1 <- tk2frame(win1, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "w", rowspan=2, row=2,column=1)
  

  
  #fonction quitter
  quitter=function(){
    quit=tkmessageBox(title="info...",
                      message="N'oubliez pas de récuperer vos resultats (dans figures et results), \n ils seront ecrasés au prochain lancement de l'application
                      \nêtes-vous sur de vouloir quitter l'application ?",
                      icon="question", type="yesno")
    if(tclvalue(quit)=="yes"){tkdestroy(win1)}
    
  }
  
  #---------------- Debut Menu-------------------------------------------
  
  win1$env$menu <- tk2menu(win1)           # Create a menu
  tkconfigure(win1, menu = win1$env$menu)  # Add it to the 'win1' window
  
  win1$env$menuFichier <- tk2menu(win1$env$menu, tearoff = FALSE) #tearoff=false: evite que le menu soit detachable
  win1$env$menuAide <- tk2menu(win1$env$menu, tearoff = FALSE)
  
  tkadd(win1$env$menuFichier, "command", label = "xxxxxxx",command = function() tkdestroy(win1))
  tkadd(win1$env$menuFichier, "command", label = "Quitter",command =quitter)
  tkadd(win1$env$menuAide, "command", label = "Quitter",command = function() tkdestroy(win1))
  
  tkadd(win1$env$menu, "cascade", label = "Fichier", menu = win1$env$menuFichier)
  tkadd(win1$env$menu, "cascade", label = "Aide", menu = win1$env$menuAide)
  
  #----------------Fin Menu-------------------------------------------


  #Les labels
  label_vec<-tk2label(frm, text = "Vec : ", justify = "left")
  label_type_data<-tk2label(frm, text = "Type de données : ", justify = "left") #justify permet d'aligner le texte
  label_ker<-tk2label(frm, text = "Noyau associé : ", justify = "left")
  label_h<-tk2label(frm, text = "h : ", justify = "left")
  label_borne<-tk2label(frm, text = "Bornes [a0,a1]: ", justify = "left")
  #label_a0<-tk2label(frm, text = "a0 : ", justify = "left")
  #label_a1<-tk2label(frm, text = "a1 : ", justify = "left")
  label_donnees<-tk2label(win1, text = "Données d'entrée: ", justify = "left")
  label_data_vec<-tk2label(win1, text = "Data: ", justify = "left")
  label_x<-tk2label(win1, text = "eval.points: ", justify = "left")
  label_estFun<-tk2label(win1, text = "est.fn: ", justify = "left")
  label_cn_autre<-tk2label(win1, text = "cn_autre: ", justify = "left")

  
  tab_type_data<- c("discrete", "continuous")
  tab_ker<- c("BE", "GA", "LN", "RIG")
  zone_saisie_h <- tclVar("0.052")
  zone_saisie_vec1 <- tclVar("100")
  zone_saisie_vec2 <- tclVar("1.5")
  zone_saisie_vec3 <- tclVar("2.6")
  zone_saisie_a0 <- tclVar("0")
  zone_saisie_a1 <- tclVar("1")
  
  # Default selections for the two combo boxes
  select_type_data <- tclVar("---Choisir---")
  select_ker<- tclVar("---Choisir---")
  
  combo_type_data <- ttkcombobox(frm, values=tab_type_data, textvariable=select_type_data, state="readonly") 
  combo_ker <- ttkcombobox(frm, values=tab_ker, textvariable=select_ker, state="readonly") 
  saisie_h <-tk2entry(frm, width = "23", textvariable =zone_saisie_h) 
  saisie_vec1 <-tk2entry(frm, width = "5", textvariable =zone_saisie_vec1)
  saisie_vec2 <-tk2entry(frm, width = "5", textvariable =zone_saisie_vec2)
  saisie_vec3 <-tk2entry(frm, width = "5", textvariable =zone_saisie_vec3)
  saisie_a0 <-tk2entry(frm, width = "5", textvariable =zone_saisie_a0)
  saisie_a1 <-tk2entry(frm, width = "5", textvariable =zone_saisie_a1)
  
  
  
  #géneration du vecteur vec
  vec_fun <- function(){
    # print("-------------------Vecteur des données---------------------")
    v1<- as.numeric(tclvalue(zone_saisie_vec1))
    v2<- as.numeric(tclvalue(zone_saisie_vec2))
    v3<- as.numeric(tclvalue(zone_saisie_vec3))
    v<-rgamma(v1,v2,v3)
    #print(v)
    
  }
  
  #Fonction dke.fun
  dke_fun <- function(){
    rm(list=ls())
   Vec<-vec_fun()
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    h<- as.numeric(tclvalue(zone_saisie_h))
    x=NULL
    a0=0
    a1=1
    if (type_data == "---Choisir---" || ker == "---Choisir---"){
      tkmessageBox(title="Info...", message = "Selectionner le type de donné et\n le noyau associé!!!")
    }else{
      #print(paste(type_data))
      # print(paste(ker))
      # print(paste(h))
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
        
        
        #data=Vec
        frm3 <- tk2frame(win1, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",rowspan=2,row=1,column=2)
        scr <- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt, ...))
        txt <- tk2text(frm3, bg = "white",font = "courier", width = 20, height = 32,yscrollcommand = function(...) tkset(scr, ...))
        tkgrid(txt,scr, sticky = "nsew")
        tkgrid.rowconfigure(frm3, txt, weight = 1)
        tkgrid.columnconfigure(frm3,txt, weight = 1)
        for (i in Vec)
          tkinsert(txt, "end", paste0(i, "\n"))
        tkconfigure(txt, state = "disabled")
        tkfocus(txt)
        
        #eval.points= x
        frm4 <- tk2frame(win1, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm4,padx = 5, pady = c(5, 10), sticky = "n",rowspan=2,row=1,column=3)
        scr1 <- tk2scrollbar(frm4, orient = "vertical",command = function(...) tkyview(txt1, ...))
        txt1 <- tk2text(frm4, bg = "white",font = "courier", width = 20, height = 32,yscrollcommand = function(...) tkset(scr1, ...))
        tkgrid(txt1,scr1, sticky = "nsew")
        tkgrid.rowconfigure(frm4, txt1, weight = 1)
        tkgrid.columnconfigure(frm4,txt1, weight = 1)
        for (i in x)
          tkinsert(txt1, "end", paste0(i, "\n"))
        tkconfigure(txt1, state = "disabled")
        tkfocus(txt1)
        
        #est.fn=result
        frm5 <- tk2frame(win1, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm5,padx = 5, pady = c(5, 5), sticky = "n",rowspan=2,row=1,column=4)
        scr2 <- tk2scrollbar(frm5, orient = "vertical",command = function(...) tkyview(txt2, ...))
        txt2 <- tk2text(frm5, bg = "white",font = "courier", width = 20, height = 32,yscrollcommand = function(...) tkset(scr2, ...))
        tkgrid(txt2,scr2, sticky = "nsew")
        tkgrid.rowconfigure(frm5, txt2, weight = 1)
        tkgrid.columnconfigure(frm5,txt2, weight = 1)
        for (i in result)
          tkinsert(txt2, "end", paste0(i, "\n"))
        tkconfigure(txt2, state = "disabled")
        #tkfocus(txt2)
        
        #hist=hist(Vec,prob=TRUE)
        histo <- function(){
          his<-hist(Vec,prob=TRUE)
        }
        tkgrid(tkrplot(win1,fun=histo),padx = 5,pady = c(5, 10), sticky = "w",columnspan=2,row=2,column=0)
        #C_n=C
        frm6 <- tk2frame(win1, borderwidth = 5,relief = "sunken",padding = 10,width = 5)
        tkgrid(frm6,padx = 5, pady = c(5, 5), sticky = "w", row=1,column=1)
        label_n <- tclVar("")
        label_cn <- tclVar("")
        label_kernel <- tclVar("")
        tclvalue(label_n) <-length(Vec)
        tclvalue(label_cn) <-C
        tclvalue(label_kernel) <-ker
        label_cn_affich <- tk2label(frm6, textvariable = label_cn)
        tkgrid(tk2label(frm6, text = "n: ", justify = "left"),padx = 5,pady = c(5, 10), sticky = "w",row=0,column=0)
        tkgrid(tk2label(frm6, textvariable = label_n, justify = "left"),padx = 5,pady = c(5, 10), sticky = "w",row=0,column=1)
        tkgrid(tk2label(frm6, text = "Kernel: ", justify = "left"),padx = 5,pady = c(5, 10), sticky = "w",row=1,column=0)
        tkgrid(tk2label(frm6, textvariable = label_kernel, justify = "left"),padx = 5,pady = c(5, 10), sticky = "w",row=1,column=1)
        tkgrid(tk2label(frm6, text = "c_n: ", justify = "left"),padx = 5,pady = c(5, 10), sticky = "w",row=2,column=0)
        tkgrid(label_cn_affich,padx = 5,pady = c(5, 10), sticky = "w",row=2,column=1)
        
        #hist=hist(Vec,prob=TRUE)
        histo <- function(){
          his<-hist(Vec,prob=TRUE)
        }
        tkgrid(tkrplot(win1,fun=histo),padx = 5,pady = c(5, 10), sticky = "w",columnspan=2,row=2,column=0)
        
        
        msg<-(structure(list(data=Vec,n=length(Vec),hist=hist(Vec,prob=TRUE),eval.points= x,h=h, kernel=ker,C_n=C,est.fn=result),class="dke.fun"))
        print(msg)
      }else{
        tkmessageBox(title="Info...", message = "Selectionner le type de données continues")
      }
    }
  }

  
  bouton_dkefun<-tk2button(frm,width = "40", text = "dke.fun", command =dke_fun)
  bouton_vec<-tk2button(frm, text = "Générer vecteur", command =vec_fun)
  
  #Positionnement
  tkgrid(label_donnees,padx = 5,pady = c(5, 10), sticky = "nw",row=0,column=0)
  tkgrid(label_cn_autre,padx = 5,pady = c(5, 10), sticky = "nw",row=0,column=1)
  tkgrid(label_data_vec,padx = 5,pady = c(5, 10), sticky = "nw",row=0,column=2)
  tkgrid(label_x,padx = 5,pady = c(5, 10), sticky = "nw",row=0,column=3)
  tkgrid(label_estFun,padx = 5,pady = c(5, 10), sticky = "nw",row=0,column=4)
  
  tkgrid(label_donnees,padx = 5,pady = c(5, 10), sticky = "w",row=0,column=0)
  tkgrid(label_vec, padx = 5, pady = c(5, 10),sticky = "w", row=0,column=0)
  tkgrid(saisie_vec1, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=1)
  tkgrid(saisie_vec2, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=2)
  tkgrid(saisie_vec3, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=3)
  #tkgrid(bouton_vec, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=4)
  
  tkgrid(label_borne, padx = 5, pady = c(5, 10),sticky = "w", row=1,column=0)
  #tkgrid(label_a0, padx = 5, pady = c(5, 10),sticky = "w", row=1,column=1)
  tkgrid(saisie_a0, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=1)
  #tkgrid(label_a1, padx = 5, pady = c(5, 10),sticky = "w", row=1,column=3)
  tkgrid(saisie_a1, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=3)
  
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=2,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=2,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=3,column=0)
  tkgrid(combo_type_data,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=3,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=4,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=4,column=1)
  
  tkgrid(bouton_dkefun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=0)
  
  
}

