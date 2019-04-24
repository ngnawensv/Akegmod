require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
require(Ake) || stop("Package Ake non disponible.")
rm(list=ls())

akeg.hcvreg.fun<-function(){
  
  #-----------Fenetre principale--------------
  win5<-tktoplevel()
  tkwm.title(win5,"hcvreg.fun: Cross-validation function for bandwidth selection in regresssion")
  tkwm.resizable(win5,FALSE,FALSE)
  tkwm.geometry(win5,"980x530+180+80")
  tkwm.minsize(win5,200,100)
  
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
  police.titre1  <- tkfont.create(family="arial", size=13, weight="bold",slant="italic")
  police.sous.titre <- tkfont.create(family="arial", size=12,underline=F)
  
  #-------------Les Frames------------------------------
  frm1 <- tk2frame(win5, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "n",row=2,column=0)
  
  frm0 <- tk2frame(win5,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "s",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "CROSS-VALIDATION FUNCTION FOR BANDWIDTH SELECTION IN REGRESSION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "s",columnspan=4,row=0,column=0)
  
  frm2 <- tk2frame(win5, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm2,padx = 5, pady = c(5, 5), sticky = "n",row=3,column=0)

  # label_vec<-tk2label(frm1, text = "Vec : ", justify = "left")
  # zone_saisie_vec1 <- tclVar("100")
  # zone_saisie_vec2 <- tclVar("1.5")
  # zone_saisie_vec3 <- tclVar("2.6")
  # saisie_vec1 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec1)
  # saisie_vec2 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec2)
  # saisie_vec3 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec3)
  
  label_type_data<-tk2label(frm1, text = "Data sample type : ", justify = "center",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("discrete", "continuous")
  select_type_data <- tclVar("discrete")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "Smoothing kernel : ", justify = "center",font=police.label)
  tab_ker<- c("bino","triang","dirDU","BE","GA","LN","RIG")
  select_ker<- tclVar("triang")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 
  # 
  # label_h<-tk2label(frm1, text = "h : ", justify = "left")
  # zone_saisie_h <- tclVar("0.052")
  # saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h) 
  # 
  # label_borne<-tk2label(frm1, text = "Bornes [a,c]: ", justify = "left")
  # zone_saisie_a <- tclVar("1")
  # zone_saisie_c <- tclVar("2")
  # saisie_a <-tk2entry(frm1, width = "5", textvariable =zone_saisie_a)
  # saisie_c <-tk2entry(frm1, width = "5", textvariable =zone_saisie_c)
  # 
  # #La premiere colonne de la fentre
  # label_frm1_donnees<-tk2label(win5, text = "Données d'entrée", justify = "center")
  # label_seq_h<-tk2label(win5, text = "seq_h: ", justify = "left")
  # label_cv<-tk2label(win5, text = "CV: ", justify = "left")

  
  #géneration du vecteur vec
  vec_fun <- function(){
    data(milk)
    list(xx=milk$week,yy=milk$yield)
  }
  
  #Fonction dke.fun
  hcvreg.fun<-function(){
    rm(list=ls())
    Vec<-vec_fun()
    data(milk)
    x=milk$week
    y=milk$yield
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    h=NULL
    a0=0
    a1=1
    a=1
    c=2
    if (type_data == "---Choisir---" || ker == "---Choisir---"){
      tkmessageBox(title="Info...", message = "Selectionner le type de donné et\n le noyau associé!!!")
    }else{
    
      if(is.null(h)) 
        
      {
        h=seq(0.001,(max(Vec$xx)-min(Vec$xx))/2, length.out=1000)
      }
      
      x<-Vec$xx
      n <- length(x)
      m=matrix(0,n,length(Vec$xx))
      m2=matrix(0,n,length(Vec$xx))
      A=rep(0,length(h))
      for(k in 1:length(h)){
        for(i in 1:n){
          m[i,]= kef(x[i],Vec$xx,h[k],type_data,ker,a0,a1,a,c)
          m2[i,]= m[i,]*y
        }
        diag(m)<-0
        diag(m2)<-0
        G<-apply(m2,1, sum) 
        E<-apply(m,1, sum)
        A[k]<-(1/length(Vec$xx))*sum((y-(G/E))^2)
      }
      index<-which.min(A)
      hcv<-h[index]
      
      #The optimal bandwidth parameter obtained by cross-validation.
      tkgrid(tk2label(win5, text = "OPTIMAL BANDWIDTH", justify = "center",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=1,column=1)
      frm3 <- tk2frame(win5, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",rowspan=2,row=2,column=1)
      scr1 <- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt1, ...))
      txt1 <- tk2text(frm3, bg = "white",font = "courier", width = 20, height = 20,yscrollcommand = function(...) tkset(scr1, ...))
      tkgrid(txt1,scr1, sticky = "nsew")
      tkgrid.rowconfigure(frm3, txt1, weight = 1)
      tkgrid.columnconfigure(frm3,txt1, weight = 1)
      #tkinsert(txt1, "end", paste0("seq_h \n"))
      for (i in h)
        tkinsert(txt1, "end", paste0(i, "\n"))
      tkconfigure(txt1, state = "disabled")
      tkfocus(txt1)
      
      
      #The values of the cross-validation
      tkgrid(tk2label(win5, text = "VALUES OF CROSS-VALIDATION : ", justify = "center",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=1,column=2)
      frm4 <- tk2frame(win5, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm4,padx = 5, pady = c(5, 10), sticky = "n",rowspan=2,row=2,column=2)
      scr2 <- tk2scrollbar(frm4, orient = "vertical",command = function(...) tkyview(txt2, ...))
      txt2 <- tk2text(frm4, bg = "white",font = "courier", width = 20, height = 20,yscrollcommand = function(...) tkset(scr2, ...))
      tkgrid(txt2,scr2, sticky = "nsew")
      tkgrid.rowconfigure(frm4, txt2, weight = 1)
      tkgrid.columnconfigure(frm4,txt2, weight = 1)
      # tkinsert(txt2, "end", paste0("CV \n"))
      for (i in A)
        tkinsert(txt2, "end", paste0(i, "\n"))
      tkconfigure(txt2, state = "disabled")
      tkfocus(txt2)
      
      #The optimal bandwidth parameter
      label_hcv <- tclVar(" ")
      tclvalue(label_hcv) <-""
      tclvalue(label_hcv) <-hcv
      tkgrid(tk2label(frm2, text = "Optimal bandwidth parameter(hcv)", justify = "left",font=police.label1),
             padx = 5,pady = c(5, 10), sticky = "s",row=0,column=0)
      tkgrid(tk2label(frm2, textvariable = label_hcv, justify = "left",font=police.label3),
             padx = 5,pady = c(5, 10), sticky = "ns",row=1,column=0)
      
      
      
      msg<-structure(list(kernel = ker,hcv=hcv,CV=A,seq_bws=h),class="hcvreg.fun")
      print(msg)
    
    }
  
  }
  

  bouton_hcvregfun<-tkbutton(frm1,width = "25", text = "hcvreg.fun",font=police.bouton, command =hcvreg.fun)
  
  #Positionnement
  # tkgrid(label_frm1_donnees,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4,row=0,column=0)
  # tkgrid(label_vec, padx = 5, pady = c(5, 10),sticky = "w", row=1,column=0)
  # tkgrid(saisie_vec1, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=1)
  # tkgrid(saisie_vec2, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=2)
  # tkgrid(saisie_vec3, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=3)
  
  # tkgrid(label_borne, padx = 5, pady = c(5, 10),sticky = "w", row=2,column=0)
  # tkgrid(saisie_a, padx = 5, pady = c(5, 10), sticky = "w",row=2,column=1)
  # tkgrid(saisie_c, padx = 5, pady = c(5, 10), sticky = "w",row=2,column=3)
  
  # tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=3,column=0)
  # tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=3,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=4,column=0)
  tkgrid(combo_type_data,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=4,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=5,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=1)
  
  tkgrid(bouton_hcvregfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=6,column=0)
  
  
}

