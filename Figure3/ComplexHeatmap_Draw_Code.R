setwd("E:/Work2020/Yu-H6-5-THYA/")


library(ComplexHeatmap)
library(RColorBrewer)

Cli <- data.frame(Group=read.table("temp-0.15rr_cli.csv",header = T,row.names = 1,sep=',')) 
ha = HeatmapAnnotation(Group=Cli$Group, col = list
                       (Group = c("Mild PTC"="yellow", "Aggressive PTC"="purple"), annotation_legend_param = list
                       (Group = list(direction = "horizontal"))))
s1 = which(Cli$Group=='Mild PTC')
s2 = which(Cli$Group=='Aggressive PTC')

data1<-read.table("temp-0.15rr.csv",header = T,row.names = 1,sep=',') 

col = c("missense variant" = "#1F78B4", "splicing variant" = "#33A02C", "nonsense variant" = "#E31A1C")
alter_fun = function(x, y, w, h, v) {
  n=sum(v)
  h=h*0.9
  grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#CCCCCC", col = NA))
  if(v["missense variant"])  grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  if(v["splicing variant"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
  if(v["nonsense variant"])    grid.rect(x, y - h*0.5 + 0.95:n/n*h, w*1, 1/n*h,  gp = gpar(fill = col[names(which(v))], col = NA), just = "top")
}
oncoPrint(data1, get_type = function(x) strsplit(x, ";")[[1]],
          bottom_annotation = ha, column_order = c(s1,s2),
          alter_fun = alter_fun, col = col,  row_names_gp = gpar(fontsize = 8),       
          heatmap_legend_param = list(title = "Alternations", at = c("missense variant", "splicing variant", "nonsense variant")))

