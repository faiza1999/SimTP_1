#' Simulation des chaines de Markov (TP N02)
#' @export
#'
#'@description
#' Simulation d'une chaine de Markov
#'
#'
#'
#' @param etats   Le vecteur des etats de la chaine
#' @param mu      La disttibution intiale
#' @param nb_rep  Le nom de repetition des trasitions
#' @param p       La matrice de trasition

ChMarkov<-function(etats,mu,p,nb_rep)
{

  x<-c(rep(0,nb_rep));

  t<-c(seq(0:nb_rep));# indices des temps

  x[1]<-rdist(etats,mu);# g\'{e}n\`{e}re la premi\`{e}re valeur



  i<-1;
  for(i in 1:nb_rep)
    x[i+1]<-rdist(etats,p[x[i],])


  plot(t,x,pch=8,xlim=c(0,nb_rep),ylim=c(0,length(mu)+1),col=length(etats));
  return(x)

}
