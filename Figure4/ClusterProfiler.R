library(openxlsx)
library(clusterProfiler)
library(ggplot2)
library(enrichplot)
library(GOplot)
library(DOSE)
library(stringr)
library(org.Hs.eg.db)
#pathway和pvalue 画气泡图
setwd("C:\\Users\\1\\Desktop\\李艺老师202101")
library(ggplot2)
pathway = read.csv("enrich.csv",header=TRUE,row.names=1,check.names = FALSE)  


p = ggplot(pathway,aes(Pvalue,Pathway))
p=p + geom_point()  
# 修稿点的大小
p=p + geom_point(aes(size=Ratio))
# 展示三维数据
pbubble = p+ geom_point(aes(size=Ratio,color=-1*log10(Pvalue)))
# 设置渐变色
pr = pbubble+scale_color_gradient(low="green",high = "red")
# 绘制p气泡图
pr = pr+labs(color=expression(-log[10](Pvalue)),size="Ratio",  
             x="Pvalue",y="Pathway name",title="Pathway enrichment")
pr + theme_bw()
## 保存图片
ggsave("out11.pdf")# 保存为pdf格式
ggsave("out12.png",width=4,height=4)


