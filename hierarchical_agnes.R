#d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
#install.packages("ggplot2")

# preparing parameters
data_for_clustering <- d2[,c("Dalc", "Walc")]
install.packages("cluster")
library(cluster)


agnes_algorithm_results1 <- agnes(data_for_clustering, method = "ward")
agnes_groups1=cutree(agnes_algorithm_results1, k = 2)

ggplot2::ggplot(data = data_for_clustering, mapping = ggplot2::aes(x = data_for_clustering$Dalc, y = data_for_clustering$Walc, color = factor(agnes_groups1))) + 
  ggplot2::scale_color_manual( values = c("green", "red")) + ggplot2::labs(color="grupa") +
  ggplot2::geom_count() +
  ggplot2::labs( 
    y = "Walc - weekendowe spożycie alkoholu", 
    x = "Dalc - spożycie alkoholu w dni pracujące", 
    title = "Grupowanie hierarchiczne\nAgnes - metoda Ward")
ggplot2::ggsave(paste("agnes_ward", ".png", sep="_"))


agnes_algorithm_results2 <- agnes(data_for_clustering, method = "complete")
agnes_groups2=cutree(agnes_algorithm_results2, k = 2)

ggplot2::ggplot(data = data_for_clustering, mapping = ggplot2::aes(x = data_for_clustering$Dalc, y = data_for_clustering$Walc, color = factor(agnes_groups2))) + 
  ggplot2::scale_color_manual( values = c("green", "red")) + ggplot2::labs(color="grupa") +
  ggplot2::geom_count() +
  ggplot2::labs( 
    y = "Walc - weekendowe spożycie alkoholu", 
    x = "Dalc - spożycie alkoholu w dni pracujące", 
    title = "Grupowanie hierarchiczne\nAgnes - metoda complete")
ggplot2::ggsave(paste("agnes_complete", ".png", sep="_"))


agnes_algorithm_results3 <- agnes(data_for_clustering, method = "single")
agnes_groups3=cutree(agnes_algorithm_results3, k = 2)

ggplot2::ggplot(data = data_for_clustering, mapping = ggplot2::aes(x = data_for_clustering$Dalc, y = data_for_clustering$Walc, color = factor(agnes_groups3))) + 
  ggplot2::scale_color_manual( values = c("green", "red")) + ggplot2::labs(color="grupa") +
  ggplot2::geom_count() +
  ggplot2::labs( 
    y = "Walc - weekendowe spożycie alkoholu", 
    x = "Dalc - spożycie alkoholu w dni pracujące", 
    title = "Grupowanie hierarchiczne\nAgnes - metoda single")
ggplot2::ggsave(paste("agnes_single", ".png", sep="_"))



agnes_algorithm_results4 <- agnes(data_for_clustering, method = "average")
agnes_groups4=cutree(agnes_algorithm_results4, k = 2)

ggplot2::ggplot(data = data_for_clustering, mapping = ggplot2::aes(x = data_for_clustering$Dalc, y = data_for_clustering$Walc, color = factor(agnes_groups4))) + 
  ggplot2::scale_color_manual( values = c("green", "red")) + ggplot2::labs(color="grupa") +
  ggplot2::geom_count() +
  ggplot2::labs( 
    y = "Walc - weekendowe spożycie alkoholu", 
    x = "Dalc - spożycie alkoholu w dni pracujące", 
    title = "Grupowanie hierarchiczne\nAgnes - metoda average")
ggplot2::ggsave(paste("agnes_average", ".png", sep="_"))
