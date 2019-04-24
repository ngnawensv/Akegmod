require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
# creation de la fenetre d'acceuil
rm(list=ls())

akeg.kef<-function(){
  
  #-----------Fenetre principale--------------
  win5<-tktoplevel()
  tkwm.title(win5,"kef: Continuous and discrete associated kernel function")
  tkwm.resizable(win5,FALSE,FALSE)
  tkwm.geometry(win5,"820x450+280+80")
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
  police.titre1  <- tkfont.create(family="arial", size=14, weight="bold",slant="italic")
  police.sous.titre <- tkfont.create(family="arial", size=12,underline=F)
  
  #-------------Les Frames------------------------------
  frm1 <- tk2frame(win5, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "nsew",rowspan=2,row=1,column=0)
  
  frm0 <- tk2frame(win5,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "n",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "CONTINUOUS AND DISCRETE ASSOCIATED KERNEL FUNCTION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "n",columnspan=4,row=0,column=0)

  
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
  
  
  #Fonction kef
  kef.fun <- function(){
    rm(list=ls())
    t<-vec_fun()
    h<- as.numeric(tclvalue(zone_saisie_h))
    x<-as.numeric(tclvalue(zone_saisie_x))
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    a0=0
    a1=1
    a=1
    c=2
      ###########################################################################################################
      # INPUTS:
      #   "x" 	: the target.
      #   "t" 	: the single or the grid value where the function is computed.
      #   "h" 	: the bandwidth parameter.
      #   "ker" 	: the kernel:  "bino" binomial,"triang" discrete triangular,"dirDU" Dirac discrete uniform.
      #			       "BE"  extended beta,"GA" gamma,"LN" lognormal,"RIG" reciprocal inverse Gaussian.     	 
      #   "a0" 	: the left bound of the support of the distribution for extended beta kernel. Default value is 0.
      #   "a1" 	: the right bound of the support of the distribution for extended beta kernel. Default value is 1.
      #   "a" 	: The arm is used only for the discrete triangular distribution.
      #   "c" 	: The number of categories in DiracDU kernel and is used only for DiracDU
      # OUTPUT:
      # Returns the discrete associated kernel value at t.
      ###########################################################################################################
      if (missing(type_data))  stop("argument 'type_data' is omitted")
      if ((type_data=="discrete") & (ker=="GA"||ker=="LN"||ker=="BE" ||ker=="RIG")) 
        stop(" Not appropriate kernel for type_data")
      if ((type_data=="continuous") & (ker=="bino"||ker=="triang"||ker=="dirDU")) 
        stop(" Not appropriate kernel for 'type_data'")
      if ((type_data=="discrete") & missing(ker)) ker<-"bino"
      if ((type_data=="continuous") & missing(ker)) ker<-"GA"
      dtrg<-function(x,t,h,a){
        
        if (a==0)
        {
          result <- t
          Logic1 <- (t==x) 
          Logic0 <- (t!=x) 
          result[Logic1]=1
          result[Logic0]=0					
          
        }
        
        else
          
        {	
          
          u=0:a;
          u=sum(u^h)			 
          D=(2*a+1)*(a+1)^h -2*u                 
          result <- t
          Logic0 <- ((t>=(x-a)) & (t<=(x+a)))  # support Sx={x-a,...,x+a} support of the distribution
          Logic1 <- ((t<(x-a))|(t>(x+a)))  
          tval <- result[Logic0]
          result[Logic1]=0
          result[Logic0]<-  ((a+1)^h - (abs(tval-x))^h)/D # Discrete Triangular 				
          
        }
        return(result)
      }
      diracDU<-function(x,t,h,c){	
        result<-t
        Logic1 <- (t==x) 
        Logic0 <- (t!=x)
        result[Logic1]<-(1-h)
        result[Logic0]<- (h/(c-1)) 
        
        
        
      }
      if(ker=="bino"){	
        result <- t
        Logic0 <- (t <= x+1) # support Sx={0,1,...,x+1}
        Logic1 <- (x+1 < t)
        tval <- result[Logic0]
        result[Logic1]=0
        result[Logic0]<- dbinom(tval,x+1,(x+h)/(x+1))  # The Binomial kernel 
        
        
      }
      else	if(ker=="triang"){
        result <- dtrg(x,t,h,a)  # The discrete Triangular kernel
        
        
      }
      else   if(ker=="dirDU"){	
        result <- diracDU(x,t,h,c)  # The Dirac Discrete Uniform kernel
        
      }
      if(ker=="BE"){	
        
        result <- t
        Logic0 <- ((a0<=t)&(t<= a1)) # support 
        Logic1 <- ((t<a0)|(a1<t))
        tval <- result[Logic0]
        result[Logic1]=0
        
        result[Logic0]<- ((1/((a1-a0)^(1+h^(-1))*beta(((x-a0)/((a1-a0)*h))+1,((a1-x)/((a1-a0)*h))+1))))*((tval-a0)^((x-a0)/((a1-a0)*h)))*((a1-tval)^((a1-x)/((a1-a0)*h))) 
      }
      else if(ker=="GA"){
        result <- t
        Logic0 <- (0<=t) # support 
        Logic1 <- (t<0)
        tval <- result[Logic0]
        result[Logic1]=0		
        result[Logic0]<- dgamma(tval,(x/h)+1,1/h)
        
        
      }
      else if(ker=="LN"){ 
        result <- t
        Logic0 <- (0<=t) # support 
        Logic1 <- (t<0)
        tval <- result[Logic0]
        result[Logic1]=0
        # result[Logic0]<-  (1/(tval*h*sqrt(2*pi)))*exp((-1/2)*((1/h)*log(tval/x)-h)^2) 				
        result[Logic0]<- dlnorm(tval,meanlog=log(x)+h^2,sdlog=h)
      }
      else if(ker=="RIG"){ 
        result <- t
        Logic0 <- (0<t) # support 
        Logic1 <- (t<=0)
        tval <- result[Logic0]
        result[Logic1]<- 0
        eps<-sqrt(x^2+x*h) # see LibenguÃ© (2013)
        result[Logic0]<- (1/sqrt(2*pi*h*tval))*exp((-eps/(2*h))*((tval/eps) -2+(eps/tval)))	
        
        
      }
      
      #the value of the associated kernel function at t according to the target and the bandwidth
      tkgrid(tk2label(win5, text = "VALUE OF THE ASSOCIATED KERNEL FUNCTION", justify = "center",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=1,column=2)
      frm3 <- tk2frame(win5, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",rowspan=3,row=2,column=2)
      scr1 <- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt1, ...))
      txt1 <- tk2text(frm3, bg = "white",font = "courier", width = 20, height = 15,yscrollcommand = function(...) tkset(scr1, ...))
      tkgrid(txt1,scr1, sticky = "nsew")
      tkgrid.rowconfigure(frm3, txt1, weight = 1)
      tkgrid.columnconfigure(frm3,txt1, weight = 1)
      #tkinsert(txt1, "end", paste0("seq_h \n"))
      for (i in result)
        tkinsert(txt1, "end", paste0(i, "\n"))
      tkconfigure(txt1, state = "disabled")
      tkfocus(txt1)
      
      print("Result")
      print(result)
      return(result)
      
    
  }

  
  bouton_kef<-tkbutton(frm1,width = "35", text = "kef",font=police.bouton, command =kef.fun)
  
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
  
  tkgrid(bouton_kef,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=0)
  
  
}

