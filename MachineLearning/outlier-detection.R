Get_Outliers <- function(vec){
    vec[vec %in% boxplot.stats(vec)$out & vec >= 10]
}