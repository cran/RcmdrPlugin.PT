.packageName <- "RcmdrPlugin.PT"
 

ProbabilitePT <- function(){
	
	
	require(poistweedie)
	initializeDialog(title=gettextRcmdr(" Plot Probabilites Poisson Tweedie "))
	
	nVar <- tclVar("50")
	nEntry <- tkentry(top, width="6", textvariable=nVar)
	
	pVar <- tclVar("2")
	pEntry <- tkentry(top, width="6", textvariable=pVar)
	
	muVar <- tclVar("10")
	muEntry <- tkentry(top, width="6", textvariable=muVar)
	
	lambdaVar <- tclVar("1")
	lambdaEntry <- tkentry(top, width="6", textvariable=lambdaVar)
	
	theta0Var <- tclVar("0")
	theta0Entry <- tkentry(top, width="6", textvariable=theta0Var)
	
	onOK <- function(){
		##closeDialog()
		n <- round(as.numeric(tclvalue(nVar)))
		if (is.na(n) || n < 1){
			errorCondition(recall=ProbabilitePT, message=getmessageRcmdr("the parametre n must be a integer  "))
			return()
		}
		p <- round(as.numeric(tclvalue(pVar)))
		if (is.na(p) || p < 1){
			errorCondition(recall=ProbabilitePT, message=getmessageRcmdr("the parametre p must be a real number upper to one  "))
			return()
		}
		mu <- round(as.numeric(tclvalue(muVar)))
		if (is.na(mu) || mu <= 0){
			errorCondition(recall=ProbabilitePT, message=getmessageRcmdr("the mean must be a a positive real number"))
			return()
		}
		lambda <- round(as.numeric(tclvalue(lambdaVar)))
		if (is.na(lambda) || lambda <= 0){
			errorCondition(recall=ProbabilitePT, message=getmessageRcmdr("the parametre of dispersion must be a positive real number."))
			return()
		}
		theta0 <- round(as.numeric(tclvalue(theta0Var)))
		if (is.na(theta0) || theta0 > 0){
			errorCondition(recall=ProbabilitePT, message=getmessageRcmdr("theta0 must be a negative real number ."))
			return()
		}
		
		command <- paste("plot(c(0:",n,"),dpoistweedie(c(0:",n,"),", p,",",mu,",",lambda,",",theta0,",log = ",FALSE,"),type = ",'"h"',",lwd=10",")" , sep="")
		doItAndPrint(command)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="dpoistweedie")
	tkgrid(tklabel(top, text=gettextRcmdr("n size")), nEntry, sticky="e")
	tkgrid(tklabel(top, text=gettextRcmdr("p size")), pEntry, sticky="e")
	tkgrid(tklabel(top, text=gettextRcmdr("mean of Poisson-Tweedie mu")), muEntry, sticky="e")
	tkgrid(tklabel(top, text=gettextRcmdr("The parametre of dispersion lambda")), lambdaEntry, sticky="e")
	tkgrid(tklabel(top, text=gettextRcmdr("The parametre Canonique theta0")), theta0Entry, sticky="e")
	
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	tkgrid.configure(pEntry, sticky="w")
	tkgrid.configure(muEntry, sticky="w")
	tkgrid.configure(lambdaEntry, sticky="w")
	tkgrid.configure(theta0Entry, sticky="w")
	dialogSuffix(rows=4, columns=2, focus=pEntry)
}



powerExample <- function(){
	run.pt.examp <- function (hscale = 1.5, vscale = 1.5, wait = FALSE) 
	{
		if (!require(tkrplot)) 
			stop("This function depends on the tkrplot package being available")
		
		n <- tclVar()
		p <- tclVar()
		mu <- tclVar()
		lambda <- tclVar()
		theta0 <- tclVar()
		xmin <- tclVar()
		xmax <- tclVar()
		
		tclvalue(n) <- 200
		tclvalue(p) <- 1
		tclvalue(mu) <- 1
		tclvalue(lambda) <- 1
		tclvalue(theta0) <- 0
		tclvalue(xmin) <- -2
		tclvalue(xmax) <- 4
		
		
		hsc <- tclVar()
		tclvalue(hsc) <- hscale
		
		vsc <- tclVar()
		tclvalue(vsc) <- vscale
		
		out <- numeric(1)
		replot <- function(...) {
			out <<- pt.examp(as.numeric(tclvalue(n)), as.numeric(tclvalue(p)), 
					as.numeric(tclvalue(mu)), as.numeric(tclvalue(lambda)), as.numeric(tclvalue(theta0)), 
					as.numeric(tclvalue(xmin)), as.numeric(tclvalue(xmax)))
			
		}
		
		tt <- tktoplevel()
		tkwm.title(tt, "Simulation des modeles Poisson Tweedie")
		img <- tkrplot(tt, replot, vscale = vscale, hscale = hscale)
		tkpack(img, side = "left")
		tkpack(fr <- tkframe(tt), side = "top", fill = "x")
		
		
		
		tkpack(tklabel(fr, text = "n: "), side = "left")
		tkpack(tkscale(fr, variable = n, orient = "horizontal", 
						command = function(...) tkrreplot(img, hscale = as.numeric(tclvalue(hsc)), 
									vscale = as.numeric(tclvalue(vsc))), from = 1, to = 1000, 
						resolution = 10), side = "right")
		tkpack(fr <- tkframe(tt), side = "top", fill = "x")
		
		
		tkpack(tklabel(fr, text = "Parameter of dispersion p: "), side = "left")
		tkpack(tkscale(fr, variable = p, orient = "horizontal", 
						command = function(...) tkrreplot(img, hscale = as.numeric(tclvalue(hsc)), 
									vscale = as.numeric(tclvalue(vsc))), from = 1, 
						to = 30, resolution = 0.1), side = "right")
		tkpack(fr <- tkframe(tt), side = "top", fill = "x")
		
		
		tkpack(tklabel(fr, text = "The mean PT mu: "), side = "left")
		tkpack(tkscale(fr, variable = mu, orient = "horizontal", 
						command = function(...) tkrreplot(img, hscale = as.numeric(tclvalue(hsc)), 
									vscale = as.numeric(tclvalue(vsc))), from = 0, to = 30, 
						resolution = 0.1), side = "right")
		tkpack(fr <- tkframe(tt), side = "top", fill = "x")
		
		
		tkpack(tklabel(fr, text = "The parameter of dispersion lambda: "), side = "left")
		tkpack(tkscale(fr, variable = lambda, orient = "horizontal", 
						command = function(...) tkrreplot(img, hscale = as.numeric(tclvalue(hsc)), 
									vscale = as.numeric(tclvalue(vsc))), from = 0.1, 
						to = 30, resolution = 0.1), side = "right")
		tkpack(tfr <- tkframe(tt), side = "top", fill = "x")
		
		tkpack(tklabel(fr, text = "The parameter theta0: "), side = "left")
		tkpack(tkscale(fr, variable = theta0, orient = "horizontal", 
						command = function(...) tkrreplot(img, hscale = as.numeric(tclvalue(hsc)), 
									vscale = as.numeric(tclvalue(vsc))), from = -30, 
						to = 0, resolution = 0.1), side = "right")
		tkpack(tfr <- tkframe(tt), side = "top", fill = "x")	
		
		
		tkpack(tklabel(tfr, text = "x min: "), side = "left")
		
		tkpack(tkentry(tfr, textvariable = xmin, width = 6), side = "left")
		tkpack(tklabel(tfr, text = "      x max: "), side = "left")
		
		tkpack(tkentry(tfr, textvariable = xmax, width = 6), side = "left")
		tkpack(tfr <- tkframe(tt), side = "bottom", fill = "x")
		
		tkpack(tklabel(tfr, text = "Hscale: "), side = "left")
		tkpack(tkentry(tfr, textvariable = hsc, width = 6), side = "left")
		
		tkpack(tklabel(tfr, text = "      Vscale: "), side = "left")
		tkpack(tkentry(tfr, textvariable = vsc, width = 6), side = "left")
		tkpack(tfr <- tkframe(tt), side = "bottom", fill = "x")
		
		
		tkpack(tkbutton(tfr, text = "Refresh", command = function() tkrreplot(img, 
									hscale = as.numeric(tclvalue(hsc)), vscale = as.numeric(tclvalue(vsc)))), 
				side = "left", anchor = "s")
		
		tkpack(tkbutton(tfr, text = "Exit", command = function() tkdestroy(tt)), 
				side = "right", anchor = "s")
		
		
		
		if (wait) {
			tkwait.window(tt)
			return(list(n = as.numeric(tclvalue(n)), p = as.numeric(tclvalue(p)), 
							mu = as.numeric(tclvalue(mu)), lambda = as.numeric(tclvalue(lambda)), theta0 = as.numeric(tclvalue(theta0)),
							power = out))
		}
		else {
			return(invisible(NULL))
		}
	}
	
	
	pt.examp <- function (n = 200, p = 1, mu = 1, lambda = 0.05,theta0=0, xmin = -2,xmax = 4) 
	{
		old.par <- par(mfrow = c(1, 1), oma = c(0, 0, 3.1, 0))
		on.exit(par(old.par))
		n <- as.integer(n)
		p <- as.numeric(p)
		mu <- as.numeric(mu)
		lambda <- as.numeric(lambda)
		theta0 <- as.numeric(theta0)
		xmin <- as.numeric(xmin)
		xmax <- as.numeric(xmax)
		se <- p/sqrt(n)
		
		
		x <- c(0:n)
		y<-dpoistweedie(x, p, mu,lambda,theta0,"FALSE")
		xmax<-sum(y)
		x1<-n/2
		x2<-2*n/3
		y1<-xmax
		plot(x, dpoistweedie(x, p, mu,lambda,theta0,"FALSE"), type = "h",
				ylim = c(0, max(dpoistweedie(x, p, mu,lambda,theta0,"FALSE")) * 7/6), main = " Probabilites Poisson Tweedie")
		text(x1,y1,"sum probabilite PT =")
		text(x2,y1,xmax) 
		
		
		
	}
	pt.examp() 		
	run.pt.examp()
}
## -----------------------------
## Calcul de la vraissemblance 
vraispoistweedie <-function (para,y)
{
	p=para[1]
	mu=para[2]
	lambda=para[3]
	theta0=para[4]
	n=length(y)
	logvrais=sum(dpoistweedie(y,p,mu,lambda,theta0,log=TRUE))
	return(-logvrais)
}

## estimation des paramètres poistweedie
parpoistweedie <-function(y)
{ 
	# par[4] vecteur de parametre initiaux
	para <- double(4)
	
	# mu0 moyenne arithmétique pondérée des variables Y
	mu0 <- double(1)
	# var0 variance des variables Y
	var0 <- double(1)		
	
	## determination des modalités et des effectifs
	## histogramme methode1
	op <- par (mfrow = c (1, 2))
	his<-hist(x=y, breaks =0:max(y), col = "blue", probability = TRUE, include.lowest = TRUE, right = TRUE, angle = 45, xlab = " variables aleatoires x ", ylab = " y ", main = "Histogramme de x ")
	x1<-his$breaks
	x2<-his$counts
	
	# calcul de la moyenne arithmétique pondérée des variables Y
	mu0 <-  sum(x1[1:max(y)+1]*x2)/sum(x2)
	##mu0 <-mean(y)
	
	# calcul de la variance des variables Y
	var0 <- sum(x2*(x1[1:max(y)+1]-mu0)^2)/sum(x2)
	## mu0 <-var(y)
	
	# valeur initiale de theta0
	theta0 <- double(1)
	theta0min <- double(1)
	theta0 <- -1
	
	# determination du paramètre p grace a la variance unite
	if(var0-(mu0+mu0^2)<0.0001){
		p=2
	} 
	else {
		
		# valeur minimale de theta0
		theta0min<--100
		# verifions s il existe une valeur de theta0 pour les quelles on peut avoir p=1
		while (theta0>theta0min && (var0-(mu0+mu0*exp(omega(1,mu0,theta0))))>0.0001){
			theta0<-theta0-0.01
		}
		if((var0-(mu0+mu0*exp(omega(1,mu0,theta0))))<0.0001){
			p=1
		}
		else {
			theta0<-runif(1,-10,0)	
			# tirage de p sur l etandue des valeurs plausibles
			p<-runif(1,1,10)
			# si p n'est pas different de 1 et 2 on tire p de nouveau
			while (p==1 || p==2){
				p <-runif(1,1,10)
			}
			
		}
	} 
	
	# determination de la moyenne mu 
	mu <- mu0
	
	
	# determination du paramètre de dispersion lambda	
	lambda<-mu0
	
	
	para[1]<- p
	para[2]<- mu
	para[3]<- lambda
	para[4]<- theta0
	
	res<- try(nlm(vraispoistweedie,para,y), TRUE)
	p1 = res$estimate[1]
	mu1 = res$estimate[2]
	lambda1= res$estimate[3]
	theta01 = res$estimate[4]
	y1 <- dpoistweedie(y = x1[1:max(y)+1], p1 , mu1, lambda1, theta01, log = FALSE)
	hist(x = y, breaks =0:max(y), col = "blue", probability = TRUE, include.lowest = TRUE, right = TRUE, angle = 45, xlab = " variables aleatoires x ", ylab = " y ", main = "Ajustement d'un Poisson-Tweedie ")
	lines(x = x1[1:max(y)+1], y = y1, type = "o", cex = 1.5, col = "red", main = "Ajustement d'un Poisson-Tweedie ")
	testqui2<-chisq.test(x2,y1)
	return(c(para,res,testqui2))
	if (is.numeric(res)){
		p <- 1 # Initialise le compteur
		while( p <=10 ) {
			mu <- 0.1
			while( mu <= 10 ) {
				lambda <- 1
				while( lambda <= 10 ) {
					theta0 <- 0.1
					while( theta0  <= -10 ) {
						res<- try(nlm(vraispoistweedie,para,y), TRUE)
						return(c(para,res))
						if (is.numeric(res)){
							para[1]<- p	
							para[2]<- mu
							para[3]<- lambda
							para[4]<- theta0
						}
						else{
							return(c(para,res))
							break
						}
						theta0 <-theta0 - 0.5
					}
					lambda<-lambda+0.5	
				}
				mu <- mu+0.5	
			}
			p <- p+0.5
			
		}
	}
	else{
		p1 = res$estimate[1]
		mu1 = res$estimate[2]
		lambda1= res$estimate[3]
		theta01 = res$estimate[4]
		y1 <- dpoistweedie(y = x1[1:max(y)+1], p1 , mu1, lambda1, theta01, log = FALSE)
		hist(x = y, breaks =0:max(y), col = "blue", probability = TRUE, include.lowest = TRUE, right = TRUE, angle = 45, xlab = " variables aleatoires x ", ylab = " y ", main = "Ajustement d'un Poisson-Tweedie ")
		lines(x = x1[1:max(y)+1], y = y1, type = "o", cex = 1.5, col = "red", main = "Ajustement d'un Poisson-Tweedie ")
		testqui2<-chisq.test(x2,y1)
		return(c(para,res,testqui2))
		break
	}
}



## ------------------------------------------------------------------------


GuiPT <- function(){
	require(poistweedie)
	require(tcltk)
	require(graphics)
	options(guiToolkit="tcltk")
	
	
	g <- ggroup(cont=gwindow("Modele exponentiel de dispersion: Poisson Tweedie (Distributions discretes )"))
	
	## ligne de commande
	com <- gcommandline(command="GuiPT" ,useGUI=TRUE, useConsole=TRUE,container=g, bg="white", font=getRcmdr("logFont"),wrap="none")
	
	
	lst = list()
	lst$Fichier$Ouvrir$handler = function(h,...) gfile(text = "Ouvrir un fichier", type ="open")
	lst$Fichier$Enregistrer_sous $handler = function(h,...) gfile(text = "Enregistrer sous", type ="save",
				filter = list("All files" = list(patterns = c("*")), "R files" =
								list(patterns = c("*.R","*.Rdata"))))
	lst$Fichier$Changer_Environnement_De_Travail$handler = function(h,...) gfile(text = "Changer l'environnement de Travail", type ="selectdir")
	lst$Fichier$Quitter_Commande_R $handler = function(h,...) quit()       
	
	
	lst$Poisson_Tweedie $Probabilite$handler = function(h,...)  {pt1=gwindow("Probabilite: Poisson-Tweedie"); ggenericwidget(poistweediePRO.list ,cli=com, container=pt1)}
	lst$Poisson_Tweedie $Fonction_de_Repartition$handler = function(h,...) {pt2=gwindow("Fonction de Repartition: Poisson-Tweedie");ggenericwidget(poistweedieFRE.list ,cli=com, container=pt2)}
	lst$Poisson_Tweedie $Fonction_Quantile$handler = function(h,...){pt3=gwindow("Fonction quantile: Poisson-Tweedie"); ggenericwidget(poistweedieFQU.list ,cli=com, container=pt3)}
	lst$Poisson_Tweedie $Fonction_Random$handler = function(h,...) {pt4=gwindow("Fonction Random: Poisson-Tweedie");ggenericwidget(poistweedieFRA.list ,cli=com, container=pt4)}
	lst$Poisson_Tweedie $Variance$handler = function(h,...)  {pt5=gwindow("Fonction Variance:  Poisson-Tweedie");ggenericwidget(poistweedieVAR.list ,cli=com, container=pt5)}
	lst$Poisson_Tweedie $EstimationParametre$handler = function(h,...)  {pt6=gwindow("Estimation des parametres du modele Poisson-Tweedie");ggenericwidget(poistweediePAR.list ,cli=com, container=pt6)}
	
	
	lst$Loi_Binomiale_Negative $Probabilite$handler = function(h,...)  {b1=gwindow("Probabilite: Negative Binomial NB(lambda1,p1) avec mu=-theta0=(1-p1)/p1"); ggenericwidget(NBinomialPRO.list,cli=com, container=b1)}
	lst$Loi_Binomiale_Negative $Fonction_de_Repartition$handler = function(h,...) {b2=gwindow("Fonction de Repartition: Negative Binomial NB(lambda1,p1) avec mu=-theta0=(1-p1)/p1"); ggenericwidget(NBinomialFRE.list,cli=com, container=b2)}
	lst$Loi_Binomiale_Negative $Fonction_Quantile$handler = function(h,...) {b3=gwindow("Fonction quantile: Negative Binomial NB(lambda1,p1) avec mu=-theta0=(1-p1)/p1");ggenericwidget(NBinomialFQU.list,cli=com, container=b3)}
	lst$Loi_Binomiale_Negative $Fonction_Random$handler = function(h,...) {b4=gwindow("Fonction Random: Negative Binomial NB(lambda1,p1) avec mu=-theta0=(1-p1)/p1");ggenericwidget(NBinomialFRA.list,cli=com, container=b4)}
	lst$Loi_Binomiale_Negative $Variance$handler = function(h,...) {b5=gwindow("Fonction Variance:  Negative Binomial NB(lambda1,p1) avec mu=-theta0=(1-p1)/p1");ggenericwidget(NBinomialVAR.list,cli=com, container=b5)}
	
	
	
	
	lst$Loi_Poisson $Probabilite$handler = function(h,...)  {p1=gwindow("Probabilite: Poisson P(lambda1) pour lambda=sqrt(lambda1), lambda=mu=-theta0 "); ggenericwidget(poissonPRO.list,cli=com, container=p1)}
	lst$Loi_Poisson $Fonction_de_Repartition$handler = function(h,...) {p2=gwindow("Fonction de Repartition: Poisson P(lambda1) pour lambda=sqrt(lambda1), lambda=mu=-theta0 ");ggenericwidget(poissonFRE.list,cli=com, container=p2)}
	lst$Loi_Poisson $Fonction_Quantile$handler = function(h,...) {p3=gwindow("Fonction quantile: Poisson P(lambda1) pour lambda=sqrt(lambda1), lambda=mu=-theta0 ");ggenericwidget(poissonFQU.list,cli=com, container=p3)}
	lst$Loi_Poisson $Fonction_Random$handler = function(h,...) {p4=gwindow("Fonction Random: Poisson P(lambda1) pour lambda=sqrt(lambda1), lambda=mu=-theta0 "); ggenericwidget(poissonFRA.list,cli=com, container=p4)}
	lst$Loi_Poisson $Variance$handler = function(h,...)  {p5=gwindow("Fonction Variance: Poisson P(lambda1) pour mu < -theta0"); ggenericwidget(poissonVAR.list,cli=com, container=p5)}
	
	
	
	lst$Graphes $Plot $handler = function(h,...)  {pl=gwindow("Plot"); pl=ggenericwidget(plot.list,cli=com, container=pl)}
	lst$Graphes $Lines $handler = function(h,...)  {li=gwindow("Lines"); li=ggenericwidget(lines.list,cli=com, container=li)}
	lst$Graphes $Histogramme $handler = function(h,...) {hi=gwindow("histogramme ");hi=ggenericwidget(hist.list,cli=com, container=hi)}
	
	
	
	## lst$Aide$ManuelsPDF$Interface$handler = function(h,...)  gfile(text = "Interface", type ="open",initialfilename="poistweedie",
	##                                                       filter = list("All files" = list(patterns = c("*")), "R files" =
	##                                                        list(patterns = c("*.pdf","*.Rdata"))))
	
	## lst$Aide$ManuelsPDF$PoissonTweediePackage$handler = function(h,...)  gfile(text = "Poisson-Tweedie-Package", type ="open",initialfilename="poistweedie",
	##                                                        filter = list("All files" = list(patterns = c("*")), "R files" =
	##                                                        list(patterns = c("*.pdf","*.Rdata"))))
	
	## lst$Aide$ManuelsPDF$MemoireDEAPoissonTweedie $handler = function(h,...) gfile(text = "Mémoire DEA Poisson Tweedie", type ="open",initialfilename="Mémoire de DEA",
	##                                                         filter = list("All files" = list(patterns = c("*")), "R files" =
	##                                                         list(patterns = c("*.pdf","*.Rdata"))))
	
	
	
	##  end menue
	## -------------------------------------------------------------
	
	##  the methodes for the menus
	## -------------------------------------------------------------

	
	
	
	
	
	
	
	TRUE.list = list(
			type = "gradio",
			items = c("TRUE","FALSE")
	)
	
	FALSE.list = list(
			type = "gradio",
			items = c("FALSE","TRUE")
	)
	
	## poistweedie.list la fonction dpoistweedie(y, power, mu,lambda,theta0,log = FALSE)
	
	## a sample list definition
	## Save some typing by defining a list to be used more than once later
	poistweediePAR.list  = list(
			title = " Estimation des Parametres Poisson-Tweedie()",
			help = "nlm",
			action = list(
					beginning = "parpoistweedie(",
					ending = ")"
			),
			
			## type = "text",  either text or graphic
			
			## variableType =  "univariate" , single variable "bivariate" "lmer"
			
			assignto = TRUE,
			arguments = list(
	
							y= list(
									type="gedit",
									coerce.with="as.list",
									text="rnbinom(1000, mu = 4, size = 10)"
							)
					
												
			)
	)
	
	
	
	
	
	
	
	poistweediePRO.list  = list(
			title = " dpoistweedie()",
			help = "dpoistweedie",
			action = list(
					beginning = "dpoistweedie(",
					ending = ")"
			),
			
			## type = "text",  either text or graphic
			
			## variableType =  "univariate" , single variable "bivariate" "lmer"
			
			assignto = TRUE,
			arguments = list(
					y = list(
							type="gedit",
							coerce.with="as.list",
							text="0:100"
					),
					p= list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-1"
					),
					log = FALSE.list
			)
	)
	
	
	
	
	poistweedieFRE.list  = list(
			title = " ppoistweedie()",
			help = "ppoistweedie",
			action = list(
					beginning = "ppoistweedie(",
					ending = ")"
			),
			
			## type = "text", # either text or graphic
			
			## variableType =  "univariate" , # single variable "bivariate" "lmer"
			
			
			assignto = TRUE,
			arguments = list(
					q = list(
							type="gedit",
							coerce.with="as.list",
							text="0:10"
					),
					p= list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					mu = list(
							type="gedit",
							coerce.with="as.numeric",
							text="10"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-1"
					),
					lower.tail= TRUE.list,
					log.p = FALSE.list
			)
	)
	
	
	
	poistweedieFQU.list  = list(
			title = " qpoistweedie()",
			help = "qpoistweedie",
			action = list(
					beginning = "qpoistweedie(",
					ending = ")"
			),
			
			## type = "text", # either text or graphic
			
			## variableType =  "univariate" , # single variable "bivariate" "lmer"
			
			
			assignto = TRUE,
			arguments = list(
					p1= list(
							type="gedit",
							coerce.with="as.list",
							text="runif(10)"
					),
					p= list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-1"
					),
					lower.tail= TRUE.list,
					log.p = FALSE.list
			)
	)
	
	
	
	
	
	poistweedieFRA.list  = list(
			title = " rpoistweedie()",
			help = "rpoistweedie",
			action = list(
					beginning = "rpoistweedie(",
					ending = ")"
			),
			assignto = TRUE,
			arguments = list(
					n= list(
							type="gedit",
							coerce.with="as.list",
							text="100"
					),
					p= list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-1"
					)
			
			)
	)
	
	
	
	
	
	poistweedieVAR.list  = list(
			title = " varpt()",
			help = "varpt",
			action = list(
					beginning = "varpt(",
					ending = ")"
			),
			assignto = TRUE,
			arguments = list(
					mu= list(
							type="gedit",
							coerce.with="as.list",
							text="seq(0.01,10,by=0.1)"
					),
					p= list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-100.1"
					)
			
			)
	)
	
	
	
	## NBinomialPRO.list la fonction dpoistweedie(y, power, mu,lambda,theta0,log = FALSE)
	
	## a sample list definition
	## Save some typing by defining a list to be used more than once later
	
	
	NBinomialPRO.list = list(
			title = " dpoistweedie()",
			help = "dpoistweedie",
			action = list(
					beginning = "dpoistweedie(",
					ending = ")"
			),
			
			## type = "text", # either text or graphic
			
			## variableType =  "univariate" , # single variable "bivariate" "lmer"
			
			
			assignto = TRUE,
			arguments = list(
					y= list(
							type="gedit",
							coerce.with="as.list",
							text="0:100"
					),
					p= list(
							type="gdroplist",
							items=2
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-10"
					),
					log = FALSE.list
			)
	)
	
	
	
	
	NBinomialFRE.list = list(
			title = " ppoistweedie()",
			help = "ppoistweedie",
			action = list(
					beginning = "ppoistweedie(",
					ending = ")"
			),
			
			## type = "text", # either text or graphic
			
			## variableType =  "univariate" , # single variable "bivariate" "lmer"
			
			assignto = TRUE,
			arguments = list(
					q= list(
							type="gedit",
							coerce.with="as.list",
							text="0:10"
					),
					p = list(
							type="gdroplist",
							items=2
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-10"
					),
					lower.tail= TRUE.list,
					log.p = FALSE.list
			)
	)
	
	
	
	
	
	
	
	NBinomialFQU.list = list(
			title = " qpoistweedie()",
			help = "qpoistweedie",
			action = list(
					beginning = "qpoistweedie(",
					ending = ")"
			),
			
			## type = "text", # either text or graphic
			
			## variableType = "univariate" , # single variable "bivariate" "lmer"
			
			assignto = TRUE,
			arguments = list(
					p1= list(
							type="gedit",
							coerce.with="as.list",
							text="runif(10)"
					),
					p= list(
							type="gdroplist",
							items=2
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-10"
					),
					lower.tail= TRUE.list,
					log.p = FALSE.list
			)
	)
	
	
	
	
	NBinomialFRA.list = list(
			title = " rpoistweedie()",
			help = "rpoistweedie",
			action = list(
					beginning = "rpoistweedie(",
					ending = ")"
			),
			
			type = "text", # either text or graphic
			
			variableType =  NULL , # single variable "bivariate" "lmer"
			
			assignto = TRUE,
			arguments = list(
					n = list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					p= list(
							type="gdroplist",
							items=2
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-10"
					)
			)
	)
	
	
	
	
	NBinomialVAR.list  = list(
			title = " varpt()",
			help = "varpt",
			action = list(
					beginning = "varpt(",
					ending = ")"
			),
			
			assignto = TRUE,
			arguments = list(
					mu= list(
							type="gedit",
							coerce.with="as.list",
							text="seq(0.01,10,by=0.1)"
					),
					p= list(
							type="gdroplist",
							items=2
					),
					
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-10"
					)
			)
	)
	
	
	
	
	
	## poissonPRO.list la fonction dpoistweedie(y, power, mu,lambda,theta0,log = FALSE)
	
	## a sample list definition
	## Save some typing by defining a list to be used more than once later
	
	
	poissonPRO.list = list(
			title = " dpoistweedie()",
			help = "dpoistweedie",
			action = list(
					beginning = "dpoistweedie(",
					ending = ")"
			),
			
			## type = "text", # either text or graphic
			
			## variableType =  "univariate" , # single variable "bivariate" "lmer"
			
			
			assignto = TRUE,
			arguments = list(
					y= list(
							type="gedit",
							coerce.with="as.list",
							text="0:100"
					),
					p= list(
							type="gdroplist",
							items=exp(seq(20,700 ,by=2))
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0=list(
							type="gedit",
							coerce.with="as.list",
							text="-1"
					),
					log = FALSE.list
			)
	)
	
	
	
	
	
	poissonFRE.list = list(
			title = " ppoistweedie()",
			help = "ppoistweedie",
			action = list(
					beginning = "ppoistweedie(",
					ending = ")"
			),
			
			## type = "text", # either text or graphic
			
			## variableType =  "univariate" , # single variable "bivariate" "lmer"
			
			
			assignto = TRUE,
			arguments = list(
					q = list(
							type="gedit",
							coerce.with="as.list",
							text="0:10"
					),
					p= list(
							type="gdroplist",
							items=exp(seq(20,700 ,by=2))
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0=list(
							type="gedit",
							coerce.with="as.list",
							text="-1"
					),
					lower.tail= TRUE.list,
					log.p = FALSE.list
			)
	)
	
	
	
	
	poissonFQU.list = list(
			title = " qpoistweedie()",
			help = "qpoistweedie",
			action = list(
					beginning = "qpoistweedie(",
					ending = ")"
			),
			
			## type = "text", # either text or graphic
			
			## variableType =  "univariate" , # single variable "bivariate" "lmer"
			
			
			assignto = TRUE,
			arguments = list(
					p1= list(
							type="gedit",
							coerce.with="as.list",
							text="runif(10)"
					),
					p= list(
							type="gdroplist",
							items=exp(seq(20,700 ,by=2))
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0=list(
							type="gedit",
							coerce.with="as.list",
							text="-1"
					),
					lower.tail= TRUE.list,
					log.p = FALSE.list
			)
	)
	
	
	
	
	poissonFRA.list = list(
			title = " rpoistweedie()",
			help = "rpoistweedie",
			action = list(
					beginning = "rpoistweedie(",
					ending = ")"
			),
			
			assignto = TRUE,
			arguments = list(
					n= list(
							type="gedit",
							coerce.with="as.list",
							text="10"
					),
					p= list(
							type="gdroplist",
							items=exp(seq(20,700 ,by=2))
					),
					mu = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					lambda = list(
							type="gedit",
							coerce.with="as.list",
							text="1"
					),
					theta0=list(
							type="gedit",
							coerce.with="as.list",
							text="-1"
					)
			)
	)
	
	
	
	
	
	poissonVAR.list  = list(
			title = " varpt()",
			help = "varpt",
			action = list(
					beginning = "varpt(",
					ending = ")"
			),
			assignto = TRUE,
			arguments = list(
					mu= list(
							type="gedit",
							coerce.with="as.list",
							text="seq(0.01,10,by=0.1)"
					),
					p= list(
							type="gdroplist",
							items=exp(seq(20,700 ,by=2))
					),
					theta0= list(
							type="gedit",
							coerce.with="as.list",
							text="-1.1"
					)
			)
	)
	
	## define a list for producing a histogram widget
	hist.list = list(
			title = "hist()",
			help = "hist",
			action = list(
					beginning = "hist(",
					ending = ")"
			),
			type = "graphic", # either text or graphic
			variableType = "univariate", # single variable
			arguments = list(
					breaks= list(
							type="gdroplist",
							items=c("","'Sturges'","'Scott'","'freedman-diaconis'","'0:max(y)'")
					),
					
					  						
							col=list(
									type="gdroplist",
									items=c("'blue'","'black'","'red'","'yellow'","'green'","'white'","'orange'")
							),

							
							probability = TRUE.list,
							include.lowest = TRUE.list,
							right = TRUE.list,
							shading = list(
									density = list(
											type="gedit",
											text=NULL
									),
									angle = list(
											type="gedit",
											coerce.with="as.list",
											text="45"),
									xlab= list(
											type="gedit",
											text= " \" variables aleatoires x \" "
									),
									ylab= list(
											type= "gedit",
											text= " \" y \""
									),
									main=list(
											type= "gedit",
											text= " \"Histogramme de x \" "
									
									)
							)   
					)
			)	

	
	
	
			
			## define a list for producing a lines widget
			lines.list = list(
					title = "lines()",
					help = "lines",
					action = list(
							beginning = "lines(",
							ending = ")"
					),
					type = "graphic", # either text or graphic
					variableType = "bivariate", # single variable
					arguments = list(
							type= list(
									type="gdroplist",
									items=c("'h'"," 'p'","'l'"," 'b' ","'c'","'o'","'s'","'S'","'n'")
							),
							
							log= list(
									type="gdroplist",
									items=c("","'x'","'y'","'xy'","'yx'")
							),
							cex=list(
									type="gedit",
									coerce.with="as.list",
									text="1.5"
							),
							col=list(
									type="gdroplist",
									items=c("'black'","'red'","'blue'","'yellow'","'green'","'white'","'orange'")
							),
							main=list(
									type="gedit",
									text= "\"Courbe de la fonction: \""
							)
					)   
			)
			
			
			
			## define a list for producing a plot widget
			plot.list = list(
					title = "plot()",
					help = "plot",
					action = list(
							beginning = "plot(",
							ending = ")"
					),
					type = "graphic", # either text or graphic
					variableType = "bivariate",
					
					arguments = list(
							type= list(
									type="gdroplist",
									items=c("'h'"," 'p'","'l'"," 'b' ","'c'","'o'","'s'","'S'","'n'")
							),
							xlimits = list(
									type="gedit",
									coerce.with="as.list",
									text="\"c(min(y),max(y))\""
							),
							ylimits = list(
									type="gedit",
									coerce.with="as.list",
									text="\"c(0,1)\""
							),
							log= list(
									type="gdroplist",
									items=c("","'x'","'y'","'xy'","'yx'")
							),
							cex=list(
									type="gedit",
									coerce.with="as.list",
									text="1.5"
							),
							col=list(
									type="gdroplist",
									items=c("'black'","'red'","'blue'","'yellow'","'green'","'white'","'orange'")
							),
							main =list(
									type="gedit",
									text= "\"Courbe de la fonction: \""
							)
					)   
			)	
	

	
	gmenu(lst, container=g)
	
		
}






#line 1 "d:/Rcompile/CRANpkg/local/2.10/RcmdrPlugin.HH/R/zzz.R"
.First.lib <- function(libname, pkgname){
	if (!interactive()) return()
	Rcmdr <- options()$Rcmdr
	plugins <- Rcmdr$plugins
	if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
		Rcmdr$plugins <- c(plugins, pkgname)
		options(Rcmdr=Rcmdr)
		closeCommander(ask=FALSE, ask.save=TRUE)
		Commander()
	}
}



