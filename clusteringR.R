#lendos os dados
dados = read.table("insirasuabase", header = TRUE, as.is = T)

#transposta, distancia e escala
dados <- t(dados)
dados <- dist(dados)
dados <- scale(dist_dados)

#vendo infos dos dados 
colnames(dados)
rownames(dados)
dim(dados)

#bibliotecas utilizadas
install.packages("cluster")
library(cluster)

install.packages("dendextend")
library(dendextend)

install.packages("pvclust")
library(pvclust)


#vetores iguais 
stand_cluster <- c(TRUE,FALSE)
metric_cluster <- c("euclidean","manhattan")

set.seed(123)


#fazendo pvclust
pvclust_dist <-c("correlation", "uncentered", "euclidean", "manhattan")
pvclust_hclust <-c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid")

for(i in pvclust_dist){
  for(j in pvclust_hclust){
    pvclust_clust <- pvclust(dados, method.dist = i, method.hclust = j,use.cor="pairwise.complete.obs", nboot=10, parallel=TRUE, r=1)
    pvcut <- cutree(pvclust_clust$hclust,k=4)
    
    
    #imprime os clusters de cada amostra
    print(i)
    print(j)
    print(pvcut)
    
    #montando o dendograma separado por cores
    dend1 <- as.dendrogram(pvclust_clust)
    dend2 <- color_branches(dend1,k=4)
    
    #exportanto dendograma
    png(paste("pvclust",i,j,".png",sep = ""), width = 1200, height = 500)
    plot(dend2,main=paste("Diana ",i,j,sep=" "))
    
    dev.off()
  }
}


#fazendo agnes
agnes_method <- c("average","single","complete","weighted","ward")
for (i in metric_cluster) {
  for(j in agnes_method){
    for(n in stand_cluster){
      agnes_clust <- agnes(dados, metric = i,method = j,stand = n)
      agnescut <- cutree(agnes_clust,k=4)
      
      #imprime os clusters de cada amostra
      print(i)
      print(j)
      print(n)
      print(agnescut)
      
      #montando o dendograma separado por cores
      dend1 <- as.dendrogram(agnes_clust)
      dend2 <- color_branches(dend1,k=4)
      
      #exportanto dendograma
      png(paste("agnes",i,j,n,".png",sep = ""), width = 1200, height = 500)
      plot(dend2,main=paste("Agnes ",i,j,n,sep=" "))
      
      dev.off()
      
    }
  }
  
}


#fazendo diana
for(i in metric_cluster){
  for(j in stand_cluster){
    diana_clust <- diana(dados, metric=i, stand=j)
    diancut <- cutree(diana_clust, k=4)
    #imprime os clusters de cada amostra
    print(i)
    print(j)
    print(diancut)
    
    #montando o dendograma separado por cores
    dend1 <- as.dendrogram(diana_clust)
    dend2 <- color_branches(dend1,k=4)
    
    #exportanto dendograma
    png(paste("diana",i,j,".png",sep = ""), width = 1200, height = 500)
    plot(dend2, main=paste("Diana ",i,j,sep=" "))
    
    dev.off()
    
  }
}


#fazendo pam
for(i in metric_cluster){
  pam_clust <- pam(dados, metric = i, k=4)
  clusterspam <- pam_clust$cluster
  print(i)
  print(clusterspam)
}



#fazendo Kmeans
kmean_alg <- c("Hartigan-Wong","Forgy", "MacQueen")
for(i in kmean_alg){
  model <- kmeans(dados,centers = 4, algorithm = i) 
  clusterskm <- model$cluster
  print(i)
  print(clusterskm)
}















