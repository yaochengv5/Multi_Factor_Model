## 载入各种包
require(tidyverse)
require(lubridate)
require(WindR)
w.start()

## 获取股票收益的截面数据
MFM_reture <- function(stock_code, from, to, frequency="M", show_chinese = FALSE){
  # stock_code:【向量】股票名称构成的向量
  # from: 【字符串】 时间起点
  # to：【字符串】时间终点
  # frequency: 【单个字符串】“M”月（默认）“D”日“W”周“Q”季度"Y"年度
  # show_chinese: 【逻辑字符】，默认显示股票代码；可以修改为显示股票中文名称
        w_wsd_data<-w.wsd(stock_code,"pct_chg",from,to,str_c("Period=",frequency,";PriceAdj=B"))$Data 
        
        if (show_chinese == TRUE)
        names(w_wsd_data)[-1] <- sto_name(stock_code)
}


# sto_name 用于将股票代码转换为公司名称，例如将“600030”或“600030.sh”转换为"中信证券" #######
        sto_name <- function(code){
        ## code: 【字符串】，表示股票代码，只有字符串的前六位数字会被截取
                
                
                
        # 从data文件夹中获取数据文件
                sto_name <- suppressMessages(read_csv("data/stock_names.csv"))
                
         #适用于1×1变量的函数
                sto_name_toapply <- function(code){
                         # 去掉代码中的“ST”
                                ifelse(str_detect(code, "^[Ss][Tt]"),
                                        code <- str_sub(code,3),
                                        NA)
                                
                        # 截取code的前6位
                                code6 <- str_sub(code,1,6)
                                
                        # code前六位是否是数字？
                                ifelse(!str_detect(code6, "\\d{6}"),
                                        invisible(stop("请确保输入代码的前6位是数字")),
                                        NA)
                        # 哪些行匹配？ 
                                good <- str_detect(sto_name[[3]], str_c("^",code6,"$"))
                        
                        # 获取符合条件的股票代码
                                sto_name[[1]][good]
                }
                
        # 通过apply函数向量化
                lapply(code, sto_name_toapply) %>% as.matrix()
                
}


# sto_code 用于将公司名称转换为代码######
sto_code <- function(stockname, code_style = "wind", exact = TRUE){
        ## stockname：【字符串】表示股票的中文名称；
        ## code_style 【字符串】表示生成的代码类型，共有三种：
        ##      1. pure： 六位数字
        ##      2. wind： wind代码
        ##      3. quant： quantmod代码
        ## exact:【逻辑变量】：表示是否完全匹配中文名称，默认是完全匹配。要想获得唯一的输出结果，该项必须为真。

        # 从data文件夹中获取数据文件
        sto_name <- suppressMessages(read_csv("data/stock_names.csv"))
        
        
        # 适用于1×1变量的函数
        sto_code_toapply <- function(stockname, code_style = "wind", exact = TRUE){
                
        
                 # 哪些行匹配？        
                if(exact==TRUE)
                         good <- str_detect(sto_name[[1]], str_c("^",stockname,"$")) 
                else
                         good <- str_detect(sto_name[[1]], stockname) 
        
                # 获取符合条件的股票名称
                 sto_name[[str_c(code_style,"_name")]][good]
        }
        
        # 通过apply函数向量化
        lapply(stockname, sto_code_toapply) %>% as.matrix()
}
              