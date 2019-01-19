#d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
#install.packages("ggplot2")

# preparing parameters
data_for_clustering <- d2[,c("Dalc", "Walc")]

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
