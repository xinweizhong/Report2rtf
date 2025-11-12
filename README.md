# **Report2rtf**
**Assemble Multiple RTF Table Listing and Figure Into One RTF Document**
**用于R中输出临床项目常见RTF文档，可以在一个RTF文档中输出一个或多个图/表及插入必其他想要的文本，也可以在同一个RTF文档中输出多个表格和/或图形。**

\
\
**1. Function函数**
   - **语法:**
>**输出RTF的开始程序（function）：**
```
rtf_start (outpath=".",reportname="report_rtfrtf"  ,reporttitle=c("Prject name","Report for","Sponsor","") , papersize ="A4" , fontsize=10.5,font="SimSun/Times New Roman" , protectspecialchars=FALSE , marglrtb=c(1,1,1,1), orientation="landscape", bodytitle=TRUE, head_footery=c(0.5,0.5))
```
>**输出RTF的结束程序（function）：**
```
rtf_end()
```
>**在RTF文档中添加图形function：**
```
rtf_plot(x,imagefmt="emf", width = 7, height = 7,bg = "transparent", fg = "black", pointsize = 12, family = "Helvetica", coordDPI = 1000,  custom.lty=TRUE, emfPlus=TRUE, emfPlusFont = FALSE, emfPlusRaster = FALSE,emfPlusFontToPath = FALSE,res=300)
```
>**在RTF文档中文本行function：**
```
rtf_addtextline(text=NULL,fontsize=10.5,style="normal",align = "left", bold=FALSE, protectspecialchars=FALSE,inheader=NULL)
```
>**在RTF文档中换页function：**
```
rtf_pagebreak()
```
>**在RTF文档中添加与rtf_start中相同的reporttitle function：**
```
rtf_addreporttitle()
```
>**在RTF文档中添加表格 function：**
```
rtf_addtable(indat,varlist=NULL,norecord ="No Observed Value Was Reported." , protectspecialchars=FALSE,perpage_obs=NA)
```


\
**2. 示例**
   - **示例1：生成空白RTF文档**
>**程序：**
```
rtf_start(reportname = "null_rtfrtf",reporttitle = NULL)
rtf_end()
```
**结果：**\
![null_rtf](/image/null_rtf.png)

 
\
   - **示例2：在RTF文档中插入header项目信息（reporttitle参数）**
>**程序：**
```
rtf_start(reportname = "null_rtfrtf",reporttitle = c("XXXX","Report for IDMC","HengRui",""))
rtf_end()
```
**结果：**\
>**默认bodytitle=TRUE时reporttitle将插入正文:**\
![bodytitle_TRUE](/image/bodytitle_TRUE.png)
>**当bodytitle=FALSE时reporttitle将插入页眉**\
![bodytitle_FALSE](/image/bodytitle_FALSE.png)
 \
 \
   - **示例3：在RTF文档中插入文本行**
>**程序：**
```
rtf_start(reportname = "null_rtfrtf",reporttitle = c("XXXX","Report for IDMC","HengRui",""))
rtf_addtextline(text="表1：RTF文件输出表测试",style="title")
rtf_end()
```
>**结果：**\
>**当设置 style="title" 且当rtf_start的参数bodytitle = TRUE时文本在正文且有书签：**\
 ![bodytitle_true_title](/image/bodytitle_true_title.png)
>**当设置 style="title" 且当rtf_start的参数bodytitle = FALSE时，文本将插入页眉：**\
 ![bodytitle_FALSE_title](/image/bodytitle_FALSE_title.png)
>**当设置 style="normal"时文本在正文：**\
 ![addtextline_normal](/image/addtextline_normal.png)
>**当设置 style="footnote"时文本在页脚：**\
![addtextline_footnote](/image/addtextline_footnote.png)

  \
  \
   - **示例4：在RTF文档中插入图**
>**程序：**
```
mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
rtf_start(outpath="./",reportname="report_figurertf",marglrtb=c(1,1,0.5,0.5),bodytitle=F) 
rtf_addtextline(text="图1：RTF文件输出图测试",style="title")
rtf_plot(mt,width = 10, height = 5.4)
rtf_end()
```
>**结果：**\
![inset_image](/image/inset_image.png)

 
 \
 \
   - **示例5：在RTF文档中换页**
>**程序：**
```
rtf_start() 
……
rtf_pagebreak()
……
rtf_end()
```

 \
 \
   - **示例6：在RTF文档中插入表格**
>**程序：**
```
rtf_start()
rtf_addtextline(text="表2：RTF文件输出表测试(空表)",style="title")
adae1<-adae[0,]
rtf_addtable(adae1,varlist=c("SUBJID/label 01/10/\\l\\order"
                             ,"TRT01P/label 02/10"
                             ,"AETERM/label 03/10/header%\\test02/header zz/\\c"
                             ,"AESOC/label 04/10/header%\\test02/header zz/\\r"))
rtf_end()
```
>**结果：**\
 ![inset_table](/image/inset_table.png)\
 

>**当设置perpage_obs参数时将按指定的分页行数进行分页（如下图设置perpage_obs=18）：**\
>**使用perpage_obs参数时最好使用bodytitle=FALSE，并将footnote放进页脚，这样每页都有title及footnote。**\
![inset_table_perpage_obs](/image/inset_table_perpage_obs.png)



 \
   - **示例7：多function同时使用**
```
library(ggplot2)
mpg<-mpg
mpg1<-mpg[1:25,]
##################### 输出简易表 ##########################
rtf_start(outpath="./") 
rtf_addtextline(text="表1：RTF文件输出表测试",style="title")
rtf_addtable(mpg1,varlist=c("manufacturer/制造商/10/header%/test01/\\l\\order"
                             ,"model/模式/10/header%/test01/\\c\\order"
                             ,"displ/排量/10/header%\\test02/header zz/\\c"
                             ,"year/年份/10/header%\\test02/header zz/\\r"
                             ,"cyl/参数01/10/header%\\test03/header zz/\\r"
                             ,"trans/参数02/10/header%\\test03/header zz/\\r"
                             ,"drv/参数03/10/header%\\test03/header zz/\\r"))

############
rtf_pagebreak()
rtf_addreporttitle()
rtf_addtextline(text="表2：RTF文件输出表测试(空表)",style="title")
adae1<-adae[0,]
######### ######
rtf_addtable(adae1,varlist=c("SUBJID/label 01/10/\\l\\order"
                             ,"TRT01P/label 02/10/\\c"
                             ,"AETERM/label 03/10/header%\\test02/header zz/\\c"
                             ,"AESOC/label 04/10/header%\\test02/header zz/\\r"))
rtf_addtextline(text="footnote: 脚注测试\\par a.每次访视至少完成一类问题的受试者占当前访视在治人数比例；
\\par b. 每次访视至少完成一类问题的受试者占有基线PRO的受试者比例；")
rtf_end() 

##################### 输出图形 ##########################
mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
rtf_start(outpath="./",reportname="report_figurertf",marglrtb=c(1,1,0.5,0.5),bodytitle=F) 
rtf_addtextline(text="图1：RTF文件输出图测试",style="title")
rtf_plot(mt,width = 10, height = 5.4)
rtf_addtextline(text="footnote: 脚注测试\\par a.每次访视至少完成一类问题的受试者占当前访视在治人数比例；
\\par b. 每次访视至少完成一类问题的受试者占有基线PRO的受试者比例；")
rtf_end()
```

