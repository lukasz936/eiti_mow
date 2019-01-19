#d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
#install.packages("ggplot2")


# k-means algorithm

# preparing parameters
data_for_clustering <- d2[,c("Dalc", "Walc")]
algorithms = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
# nstarts if centers is a number, how many random sets should be chosen
nstarts = c(1, 3, 5, 10)
# the maximum number of iterations allowed
max_iters = c(1, 10, 100, 500, 1000, 5000)
# choosen centers
centers = data.frame(matrix(c(1,5,5,5), nrow=2, ncol=2, byrow = TRUE))
print(centers)

#plot walc and dalc only with density
ggplot2::ggplot(data = data_for_clustering, mapping = ggplot2::aes(x = data_for_clustering$Dalc, y = data_for_clustering$Walc, color = "Red")) + ggplot2::guides(color=FALSE) + 
  ggplot2::geom_count() +
  ggplot2::labs( 
    y = "Walc - weekendowe spożycie alkoholu", 
    x = "Dalc - spożycie alkoholu w dni pracujące", 
    title = "Zależność atrybutów Dalc od Walc")
 ggplot2::ggsave("Dalc_Walc_plot.png")

k_means_fun <- function(iters, centers, algorithm, nstart){
  k_means_results <<- kmeans(x = data_for_clustering, 
                           centers, 
                           iters, 
                           nstart, 
                           algorithm, 
                           trace=FALSE)
  k_means_centers = data.frame(matrix(unlist(k_means_results[2]), nrow=2, ncol = 2, byrow=F, dimnames = list(c(1, 2), c("Dalc", "Walc"))))
  
  #plot results
  ggplot2::ggplot(data = data_for_clustering, mapping = ggplot2::aes(x = data_for_clustering$Dalc, y = data_for_clustering$Walc, color = factor(k_means_results$cluster))) + 
    ggplot2::scale_color_manual( values = c("green", "red")) + ggplot2::labs(color="grupa") +
    ggplot2::geom_count() +
    ggplot2::labs( 
      y = "Walc - weekendowe spożycie alkoholu", 
      x = "Dalc - spożycie alkoholu w dni pracujące", 
      title = "Zależność atrybutów Dalc od Walc")
  #ggplot2::ggsave(paste("kmeans", "centers", centers, "iters", iters, "nstarts", nstart, algorithm, ".png", sep="_"))
}

k_means_fun(1000,2,c("Forgy"),5)


k_means_results = kmeans(x = data_for_clustering, 
                         centres, 
                         iter.max = 10, 
                         nstart = 100, 
                         algorithm = c("MacQueen"), 
                         trace=FALSE)
k_means_centers = data.frame(matrix(unlist(k_means_results[2]), nrow=2, ncol = 2, byrow=F, dimnames = list(c(1, 2), c("Dalc", "Walc"))))
print(k_means_results)




ggplot2::ggplot(mapping=ggplot2::aes(data_for_clustering$Dalc, data_for_clustering$Walc)) + 
  ggplot2::geom_count() + 
  ggplot2::geom_point(mapping=ggplot2::aes(k_means_centers$Dalc, k_means_centers$Walc, color=c(1,2)), shape=18, size=7) +
  ggplot2::geom_point(mapping=ggplot2::aes(origins$X1, origins$X2, color=-1)) +
  ggplot2::labs(title = "Alcohol consumption clustering", 
                                 "algorithm used:", as.character("Hartigan"),  "\t",
                                 "starting clusters:", as.character(100), sep = " "),
                y="Weekend consumption",
                x="Daily consumption") +
  ggplot2::coord_fixed()
ggplot2::ggsave(paste("kmeans", "ours", "centers", ".png", sep="_"))


#dbscan

install.packages("dbscan")
db_clusters1 = dbscan::dbscan(data_for_clustering, eps=1, minPts =5)

plot(
  data_for_clustering[, 1],
  data_for_clustering[, 2],
  pch = 20,
  col = db_clusters1$cluster,
  main = "Grupowanie DBSCAN\neps=1, minPts= 5",
  xlab = "Dalc",
  ylab = "Walc"
)

db_clusters2 = dbscan::dbscan(data_for_clustering, eps=0.1, minPts = 5)
plot(
  data_for_clustering[, 1],
  data_for_clustering[, 2],
  pch = 20,
  col = db_clusters2$cluster,
  main = "Grupowanie DBSCAN\neps=0.1, minPts= 5",
  xlab = "Dalc",
  ylab = "Walc"
)

db_clusters3 = dbscan::dbscan(data_for_clustering, eps=0.1, minPts = 1)
plot(
  data_for_clustering[, 1],
  data_for_clustering[, 2],
  pch = 20,
  col = db_clusters3$cluster,
  main = "Grupowanie DBSCAN\neps=0.1, minPts= 1",
  xlab = "Dalc",
  ylab = "Walc"
)

install.packages("factoextra")
install.packages("cluster")
library(cluster)
grupy <- agnes(data_for_clustering, method = "average")

data=cutree(grupy, k = 2)
library(factoextra)
fviz_dend(grupy, k = 2, rect = TRUE, main = "Metoda Ward")



ggplot2::ggplot(data = data_for_clustering, mapping = ggplot2::aes(x = data_for_clustering$Dalc, y = data_for_clustering$Walc, color = factor(data))) + 
  ggplot2::scale_color_manual( values = c("green", "red")) + ggplot2::labs(color="grupa") +
  ggplot2::geom_count() +
  ggplot2::labs( 
    y = "Walc - weekendowe spożycie alkoholu", 
    x = "Dalc - spożycie alkoholu w dni pracujące", 
    title = "Grupowanie hierarchiczne - agnes")
ggplot2::ggsave(paste("agnes", ".png", sep="_"))
