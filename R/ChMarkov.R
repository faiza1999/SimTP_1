#' Simulation des chaines de Markov (TP N02)
#' @export
#'
#'@description
#' Simulation d'une chaine de Markov
#'
#'
#'
#'
#' @param etats   Le vecteur des etats de la chaine
#' @param mu      La disttibution intiale
#' @param nb_rep  Le nom de repetition des trasitions
#' @param p       La matrice de trasition

ChMarkov<-function(etats,mu,p,nb_rep)
{

  cat("Le nombre des etats est egal à ",length(etats),"\n");
  cat("Les etats sont ",etats,"\n");
  cat("Le nombre des repetitions est egal a ",nb_rep,"\n")


 # p<-matrix(c(0.6,0.3,0.1,0.3,0.3,0.4,0.4,0.1,0.5),ncol=length(etats),byrow=T);# matrice de transition

  #n=80; # nombre d'\'{e}tapes


  x<-c(rep(0,nb_rep)); #matrix de 80 element avec Valeur = 0

  cat("La matrice x est intialé à 0 pour tout ses elements \n x",x,"\n");

  t<-c(seq(0:nb_rep));# indices des temps

  cat("Lindice de temps varie entre ",t[1]," et ",t[nb_rep],"\n");

  x[1]<-rdist(etats,mu);# g\'{e}n\`{e}re la premi\`{e}re valeur



  i<-1;
  for(i in 1:nb_rep)
    x[i+1]<-rdist(etats,p[x[i],])

  cat("La matrice x = est ",x,"\n");

  plot(t,x,pch=8,xlim=c(0,nb_rep),ylim=c(0,length(mu)+1),col=length(etats));

}
