# *************************************************************************************************
# File name:      report_rtf.sas
# 
# Study:          XXXXXXXX
# 
# SAS version:    9.4
# 
# Purpose:        report rtf file
# 
# Macros called:
#   
#   Notes:
#   
#   Parameters:
#   
#   Sample:
#   
# Date started:    13OCT2025
# Date completed:  
#   
#   Mod     Date            Name            Description
# ---     -----------     ------------    -----------------------------------------------
# 1.0     13OCT2025       Xinwei.Zhong    Created

############################################## Prepared by hengrui ##############################################

cat('\f')
rm(list=ls(envir = .GlobalEnv, all.names = TRUE))
setwd("D:/zxw/R-study/R-output-rtf/R_report_rtf/package")
##安装Report2rtf
#install.packages("Report2rtf_0.1.0.tar.gz")

library(Report2rtf)
library(ggplot2)

load("adae.rda")
mpg<-mpg
mpg1<-mpg[1:15,]
##################### 输出简易表 ##########################
rtf_start(outpath="./") 
rtf_addtextline(text="表1：RTF文件输出表测试",style="title")
rtf_addtable(mpg1,varlist=c("manufacturer/制造商/10/header%/test01"
                             ,"model/模式/10/header%/test01/\\c"
                             ,"displ/排量/10/header%\\test02$换行/header zz/\\c"
                             ,"year/年份/10/header%\\test02$换行/header zz/\\r\\order"
                             ,"cyl/参数01/10/header%\\test03/header zz/\\r\\order"
                             ,"trans/参数02/10/header%\\test03/header zz/\\r"
                             ,"drv/参数03/10/header%\\test03/header zz/\\r")
)


############
rtf_pagebreak()
rtf_addreporttitle()
rtf_addtextline(text="表2：RTF文件输出表测试(空表)",style="title")
adae1<-adae[0,]
######### 注意：填写对齐方式前需要添加"/" 即拆分符切换用"/\\"开始 ######
rtf_addtable(adae1,varlist=c("SUBJID/label 01/10/\\l\\order"
                              ,"TRT01P/label 02/10"
                              ,"AETERM/label 03/10/header%\\test02/header zz/\\c"
                              ,"AESOC/label 04/10/header%\\test02/header zz/\\r"))
rtf_addtextline(text="footnote: 脚注测试#R/RTF{\\par} a.每次访视至少1\\3完成一类问题的受试者占当前访视在治人数比例；
#R/RTF{\\par\\b} b. 每次访视至少完成一类问题的受试者占有基线PRO的受试者比例；")
rtf_end() 

##################### 输出图形 ##########################
p <- ggplot(data = mtcars, aes(x = wt, y = mpg)) +geom_point() +
  ggtitle("ggplot示例")

mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()

rtf_start(outpath="./",reportname="report_figure.rtf",marglrtb=c(1,1,0.5,0.5),bodytitle=F) 
rtf_addtextline(text="图1：RTF文件输出图测试",style="title")
rtf_plot(mt,imagefmt="emf",width = 9, height = 5.4)
rtf_addtextline(text="footnote: 脚注测试#R/RTF{\\par\\b} a. \\\\ 每次访视至少完成一类问题的受试者占当前访视在治人数比例；
#R/RTF{\\par\\b} b. 每次访视至少完成一类问题的受试者占有基线PRO的受试者比例；",style="footnote")
rtf_end()  


################################ 
rtf_start(reportname = "null_rtf0.rtf",reporttitle = c("XXXX","Report for IDMC","HengRui",""),font="Times New Roman")
rtf_end()

rtf_start(reportname = "null_rtf.rtf",reporttitle = c("XXXX","Report for IDMC","HengRui",""))
rtf_addtextline(text="表1：RTF文件输出表测试",style="normal")
rtf_pagebreak()
rtf_plot(mt,width = 10, height = 5.4)
rtf_end()


rtf_start(outpath="./",reportname = "report_rtf_2.rtf",bodytitle=F) 
rtf_addtextline(text="表1：RTF文件输出表测试",style="title")
rtf_addtable(mpg,varlist=c("manufacturer/制造商/10/header%/test01"
                            ,"model/模式/10/header%/test01/\\c"
                            ,"displ/排量/10/header%\\test02$换行/header zz/\\c"
                            ,"year/年份/10/header%\\test02$换行/header zz/\\r\\order"
                            ,"cyl/参数01/10/header%\\test03/header zz/\\r\\order"
                            ,"trans/参数02/10/header%\\test03/header zz/\\r"
                            ,"drv/参数03/10/header%\\test03/header zz/\\r")
              ,perpage_obs="model")

rtf_addtextline(text="footnote: 脚注测试#R/RTF{\\par\\b} a.每次访视至少1\\3完成一类问题的受试者占当前访视在治人数比例；
#R/RTF{\\par\\b} b. 每次访视至少完成一类问题的受试者占有基线PRO的受试者比例；",style="footnote")
rtf_end() 
 