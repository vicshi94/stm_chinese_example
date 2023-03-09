library(stm)
library(textir)

# 显示中文
# library(showtext)
# font_files()
#showtext_auto(enable = TRUE)
# font_add('Songti', 'Songti.ttc')

# 读取数据 ----------------------------------------------------

#load(stm.Rdata)
data<-read.csv(file.choose(),sep = ',', quote = '',
               header = TRUE, fileEncoding = 'utf-8')

# 提取数据 ----------------------------------------------------
#
# 利用textProcessor算法提取数据,将data文本列和data作为参数
# textProcessor暂不支持中文，需要自行预处理
# wordLength=c(3,Inf)表示移除短于3，长于inf的词语：中文调为1以免单字被删
processed <- textProcessor(data$fulltext,metadata=data,wordLengths=c(1,Inf))

# 数据预处理 ---------------------------------------------------
#
# 查看不同阈值删除doc,words,token情况
# pdf("output/stm-plot-removed.pdf")
plotRemoved(processed$documents, lower.thresh=seq(from = 1, to = 100, by =1))
# dev.off()
# 根据结果确定lower.thresh取值 preDocuments默认值为1

# 建立完整文档-协变量数据
out <- prepDocuments(
  documents = processed$documents, #包含索引词的计数及文档列表
  vocab = processed$vocab, # 索引词的关联单词
  meta = processed$meta, #包含文档协变量
  lower.thresh = 5)

# 模型拟合 ----------------------------------------------------
#
poliblogPrevFit <- stm(documents = out$documents, 
                       vocab = out$vocab,
                       K = 15, #主题数
                       # 主题偏好模型指定的协变量
                       prevalence = ~stage+platform,
                       # 词语偏好模型指定的协变量
                       content = ~stage,
                       max.em.its = 10, #最大迭代数
                       data = out$meta,
                       init.type = 'Spectral' # OR LDA
                      )
# 处理day这个连续变量: 用s(day), 引入spline解决自由度损失问题
# 其他标准转换函数: log(), ns(), bs()

# 模型评估
#
# 1. 指定主题数创建初始化模型
poliblogPrevFit <- stm(
  out$documents, out$vocab,K = 15, prevalence = ~stage+platform, 
  max.em.its = 75, data = out$meta,init.type = 'Spectral' #优先Spectral
  )

# 2. 指定主题数模型选择
poliblogSelect <- selectModel(
  out$documents, out$vocab, K = 15, prevalence = ~stage+platform,
  max.em.its = 50, data = out$meta, 
  runs = 20, #20 models
  seed = 20221030
  )

# 通过语义coherence和exclusivity选择模型（越大越好）
# 可视化结果
plotModels(poliblogSelect, pch = c(1,2,3,4), legend.position = 'bottomright')

#选择模型{k}
selectmodel <- poliblogSelect$runout[[4]]

# 3. 确定主题数
storage <- searchK(
  out$documents, out$vocab, K = c(5:10),
  prevalence = ~stage+platform, data = out$meta
  )

# 可视化结果

# 自定义 plot.searchK(), 显示Exclusivity
plt_searchK<-function(x, ...){
  oldpar <- par(no.readonly=TRUE)
  g <- x$results
  par(mfrow=c(3,2),mar=c(4,4,2,2),oma=c(1,1,1,1))
  
  plot(g$K,g$heldout,type="p", main="Held-Out Likelihood", xlab="", ylab="")
  lines(g$K,g$heldout,lty=1,col=1)
  
  plot(g$K,g$residual,type="p", main="Residuals", xlab="", ylab="")
  lines(g$K,g$residual,lty=1,col=1 )
  
  if(!is.null(g$semcoh)){
    plot(g$K,g$semcoh,type="p", main="Semantic Coherence", xlab="", ylab="")
    lines(g$K,g$semcoh,lty=1,col=1 ) 
  }
  
  plot(g$K,g$exclus,type="p", main="Exclusivity", xlab="", ylab="")
  lines(g$K,g$exclus,lty=1,col=1 )  
  
  plot(g$K,g$bound,type="p", main="Bound", xlab="Number of Topics (K)", ylab="")
  lines(g$K,g$bound,lty=1,col=1 ) 
  
  plot(g$K,g$lbound,type="p", main="Lower Bound", xlab="Number of Topics (K)", ylab="")
  lines(g$K,g$lbound,lty=1,col=1 ) 
  
  title("Diagnostic Values by Number of Topics", outer=TRUE)  
  par(oldpar)
}

# pdf("output/stm-plot-ntopics.pdf")
# plot(storage)
plt_searchK(storage)
# dev.off

t <- storage$results[[2]] # exclusivity
t <- storage$results[[3]] # coherence

# 结果展示 ----------------------------------------------------
#
# 结果保存为csv
stm_res_df <- poliblogPrevFit$theta
write.csv(stm_res_df, "stm_res_df.csv")

# 高频词
labelTopicsSel <- labelTopics(poliblogPrevFit, c(1:15)) #主题数
# sink("output/labelTopics-selected.txt",append = FALSE, split = TRUE)
print(labelTopicsSel)
# sink()

# sink("output/stm-list-sagelabel.txt",append = FALSE, split = TRUE)
print(sageLabels(poliblogPrevFit)) #另一种输出方式，更详细
# sink()

# 列出主题典型文档
shortdoc <- substr(out$meta$fulltext,1,200) # 前200个字符
thoughts1 <- findThoughts(
  poliblogPrevFit, texts = shortdoc, n=3, topics=2 #展示Topic{k}的n篇文档
  )$docs[[1]]
# pdf("output/findThoughts-Topic1.pdf")
plotQuote(thoughts1,width = 30, main = 'Topic 1')
# dev.off()

# 多个结果
thoughts10 <- findThoughts(
  poliblogPrevFit, texts = shortdoc, n=3, topics=10 #展示Topic{k}的n篇文档
)$docs[[1]]
# pdf("output/findThoughts-Topics.pdf")
par(mfrow=c(2,1),mar=c(.5,.5,1,.5)) # 2x1的表格
plotQuote(thoughts1,width = 30, main = 'Topic 1')
plotQuote(thoughts10,width = 30, main = 'Topic 10')
# dev.off()

# estimateEffect
out$meta$stage <- as.factor(out$meta$stage)
prep <- estimateEffect(
  # 对所有主题的影响
  1:15 ~ platform+stage, poliblogPrevFit, meta=out$meta,
  uncertainty = 'Global' # OR "Local", "None"
  # 考虑全局不确定性，选择None会加速计算时间，获得更窄的置信区间
)
summary(prep, topics=1)
summary(prep, topics=2)

# 可视化结果 ---------------------------------------------------
#
# 主题占比条形图
# pdf("top-topic.pdf")
plot(poliblogPrevFit,type = 'summary', xlim = c(0,.3))
# dev.off()

# 主题关系对比图
# pdf("stm-plot-topical-prevalence-contrast.pdf")
plot(
  prep, covariate = 'platform', topics = c(2,7,8),
  model = poliblogPrevFit, method = 'difference',
  cov.value1 = 'hkdiscuss', cov.value2 = 'lihkg',
  xlab = 'x-label',
  main = 'Effect of hkdicuss v.s. lihkg',
  xlim = c(-.1,.1), labeltype = 'custom',
  custom.labels = c("Topic 2", 'Topic 7', "Topic 8")
)
#dev.off()

# 主题时序变化图
# pdf("stm-plot-prevalence-trends.pdf")
# plot(
#   prep, "days with numeric type", method = "continuous", topics = 7,
#   model = z, printlegend = FALSE, xaxt = "n", xlab = "Stages"
# )
# monthseq <- seq(
#   from = as.Date("2020-06-30"), to = as.Date("2022-06-30"), by = "month"
#   )
# monthnames <- months(monthseq)
# axis(
#   1, 
#   at = as.numeric(monthseq)-min(as.numeric(monthseq)),
#   labels = monthnames
# )
# dev.off()

# 词汇与协变量关联度
poliblogContent <- stm(out$documents, out$vocab, K=15,
                       prevalence = ~ platform + stage, 
                       content = ~ platform,
                       max.em.its = 10,
                       data = out$meta,
                       init.type = "Spectral"
                       )
# pdf("stm-plot-content-perspectives.pdf")
plot(poliblogContent, type = 'perspectives', topics = 10)
# dev.off()

# 主题间词汇差异
# pdf("stm-plot-content-perspectives-2vs7.pdf")
plot(poliblogPrevFit,type = 'perspectives', topics = c(2,7))
# dev.off()

# 协变量之间的影响
# poliblogInteraction <- stm(
#   out$documents, out$vocab, K = 15,
#   prevalence = ~rating*day, max.em.its = 75,
#   data = out$meta, init.type = "Spectral")
# prep <- estimateEffect(
#   c(k)~rating*day, poliblogInteraction,metadata = out$meta, uncertainty = "None"
# ) # Topic_k
# pdf("stm-plot-two-topic-contrast.pdf")
# plot(
#   prep,covariate = "day", model = poliblogInteraction, method = 'continuous',
#   xlab = "Days", moderator = "rating", moderator.value = "Liberal",
#   linecol = "blue", ylim = c(0,.12), printlegend = FALSE
# )
# plot(
#   prep,covariate = "day", model = poliblogInteraction, method = 'continuous',
#   xlab = "Days", moderator = "rating", moderator.value = "onservative",
#   linecol = "red", add = TRUE, printlegend = FALSE
# )
# legend(0, 0.06, c("Liberal", "Conservative"), lwd = 2, col = c("blue", "red"))
# dev.off()

# 补充可视化结果 -------------------------------------------------
#
# 词云图
# pdf("stm-plot-wordcloud.pdf")
cloud(poliblogPrevFit, topic = 2, scale = c(2, 0.25))
# dev.off()

# 主题网络
# require igraph
mod.out.corr <- topicCorr(poliblogPrevFit)
# pdf("stm-plot-topic-correlations.pdf")
plot(mod.out.corr)
# dev.off()

# stmCorrViz
# 主题分层聚类展示
library(stmCorrViz)
# corrViz <- stmCorrViz(...)
stmCorrViz(
  poliblogPrevFit, "stm-interactive-correlation.html",
  documents_raw = data$fulltext, documents_matrix = out$documents
  )

# 话题质量
topicQuality(model=poliblogPrevFit, documents=out$documents)

# 更多插件：
# https://www.structuraltopicmodel.com/
