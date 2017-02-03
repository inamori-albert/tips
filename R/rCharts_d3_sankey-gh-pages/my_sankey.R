# sankey-graphをデータフレームからD3.jsで描画
setwd('~\rCharts_d3_sankey-gh-pages')


seats <- data.frame(
  source=c("A","A","a","a","a","a","a","a","a","a","a","b"),
  target=c("a","b","1","2","3","4","5","6","7","8","9"," b"),
  value=c(121,121,65,17,11,8,8,8,1,1,2,121)
)

sankeyPlot <- rCharts$new()
sankeyPlot$setLib('libraries/widgets/d3_sankey')
sankeyPlot$setTemplate(script = "libraries/widgets/d3_sankey/layouts/chart.html")

sankeyPlot$set(
  data = seats,
  nodeWidth = 15,
  nodePadding = 5,
  layout = 32,
  width = 750,
  height = 500,
  labelFormat = ""
)

sankeyPlot$show()