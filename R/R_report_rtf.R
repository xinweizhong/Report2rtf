.to_rtffontcode <- function(str) {
  str1<-unlist(strsplit(str, ""))
  for(i in 1:length(str1)){
    if (grepl("[^\u0001-\u007F]",str1[i], perl = TRUE)){
      s<-paste0("\\u", utf8ToInt(str1[i]),";")
      str1[i]<-s
    }
  }
  unicode_str<-paste0(str1, collapse = "")
  return(unicode_str)
}

.count_char <- function(string, char) {
  pattern <- paste0("\\", char)
  matches <- gregexpr(pattern, string)[[1]]
  if (matches[1] == -1) return(0)
  length(matches)
}

.split_var <- function(indat,var=null,split="/",prefix="split") {
  inds<-indat
  inds[,var]<-gsub("%/", "$#@",gsub("%\\\\", "$##@",inds[,var]))
  obs<-ncol(inds)
  max_count<- max(sapply(inds[,var], .count_char, char = split))+1
  inds <-cbind(inds,as.data.frame(do.call(rbind, lapply(strsplit(inds[,var], split=paste0("\\", split)), function(x) {
    length(x) <- max_count 
    x}))))
  ord<-c((obs+1):(obs+max_count))
  colnames(inds)[ord]<-paste0(prefix, 1:(ncol(inds)-obs))
  return(inds)
}
#' 输出RTF文档的开始位置function
#'
#' 输出RTF文档的开始位置function，可以设置一些常见文档页面参数，如纸张大小、页面方向、页边距等（具体见下方参数）。
#'
#' @param outpath RTF文件输出路径
#' @param reportname 指定输出的RTF名称（必须带上扩展名“.rtf”） 
#' @param reporttitle 指定页眉信息（包含项目信息）
#' @param papersize 指定纸张大小，只支持A4或LETTER.
#' @param fontsize 指定字体大小.
#' @param font 指定输出字体，支持双字体（字体间用“/”隔开）
#' @param protectspecialchars 指定是否对特殊文本进行保护。设置为TRUE时，文本中的RTF码将视为普通文本.
#' @param marglrtb 指定页边距大小，单位为in，页边距设置顺序依次为左右上下.
#' @param orientation 指定纸张方向，PORTRAIT=纵向，landscape=横向. 
#' @param head_footery 指定页眉文本与页边距离
#' @param bodytitle 指定页眉文本放在正文还是header中
#'
#' @examples
#' library(ggplot2)
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' rtf_start(reportname = "null_rtf.rtf",reporttitle = c("XXXX","Report for IDMC","Sponsor",""))
#' rtf_addtextline(text="Table 1：RTF file output table test",style="title")
#' rtf_pagebreak()
#' rtf_plot(mt,width = 10, height = 5.4)
#' rtf_end()
#'
#' @export

############# start rtf #############
rtf_start <- function(outpath=".",reportname="report_rtf.rtf"
                       ,reporttitle=c("Prject name","Report for","Sponsor","")
                       ,papersize ="A4" ,fontsize=10.5,font="SimSun/Times New Roman"
                       ,protectspecialchars=FALSE,marglrtb=c(1,1,1,1),orientation="landscape"
                       ,head_footery=c(0.5,0.5),bodytitle=TRUE){
  
  ############# start rtf code #############
  rtf_start_code<-"{\\rtf1\\ansi\\ansicpg936\\uc1\\deff0\\deflang2052\\deflangfe2052"
  rtf_start_font<-"{\\fonttbl "
  font_ls<-""
  for(i in 1:(.count_char(font,"/")+1)){
    font_1<-unlist(strsplit(font,"/"))[i]
    if(nchar(font_1)>0){
      font_1<-.to_rtffontcode(font_1)
      font_ls<-paste0(font_ls,paste0("\\f",i))
      rtf_start_font<-paste(rtf_start_font,paste0("{\\f",i,"\\froman\\fprq2\\fcharset134\\cpg936 ", font_1,";}"),sep="\n")
    }
  }
  rtf_start_font<-paste(rtf_start_font,"}")
  rtf_color<-"{\\colortbl;\n\\red0\\green0\\blue0;\n\\red0\\green0\\blue255;\n\\red0\\green255\\blue255;\n\\red0\\green255\\blue0;\n\\red255\\green0\\blue255;\n\\red255\\green0\\blue0;\n\\red255\\green255\\blue0;\n\\red255\\green255\\blue255;}"
  
  if(toupper(papersize)=="A4"){##### A4 width=11.69236 height=8.267361 [297 x 210 mm, 11.7 x 8.3 in]####
    paperwh<-c(16837,11905)
  }else{##### letter width=11 height=8.5 ####
    paperwh<-c(15840,12240)
  }
  if(tolower(orientation)=="landscape"){
    paperw<-paperwh[1]
    paperh<-paperwh[2]
    landscape<-"\\lndscpsxn"
  }else{
    paperw<-paperwh[2]
    paperh<-paperwh[1]
    landscape<-""
  }
  
  if(is.null(marglrtb)){marglrtb=c(1,1,1,1)}
  for(i in 1:4){
    if(is.na(marglrtb[i])){ marglrtb[i]<-1 }
  }
  if(is.null(head_footery)){head_footery=c(0.5,0.5)}
  for(i in 1:2){
    if(is.na(head_footery[i])){ head_footery[i]<-0.5 }
  }
  rtf_ori_code<-"\\widowctrl\\ftnbj\\aenddoc\\formshade\\viewkind1\\viewscale100\\pgbrdrhead\\pgbrdrfoot\\fet0\\titlepg"
  rtf_ori_code<-paste0(rtf_ori_code,paste0("\\paperw",paperwh[2]),paste0("\\paperh",paperwh[1]))
  rtf_ori_code<-paste0(rtf_ori_code,paste0("\\margl",marglrtb[1]*1440),paste0("\\margr",marglrtb[2]*1440),paste0("\\margt",marglrtb[3]*1440),paste0("\\margb",marglrtb[4]*1440))
  rtf_ori_code<-paste(rtf_ori_code,paste0("\\sectd\\linex0\\endnhere\\headery0\\footery0",landscape,paste0("\\pgwsxn",paperw),paste0("\\pghsxn",paperh)),sep="\n")
  
  max_colw<-(paperw-marglrtb[1]*1440-marglrtb[2]*1440)-2
  header_code<-"{\\header\\pard\\plain\\qc{"
  footer_code<-"{\\footer\\pard\\plain\\ql{"
  
  tb_cellw_temp<-"\\cltxlrtb\\clvertalt\\clcbpat20\\cellx"
  tab_txt_pre_temp<-paste0("\\pard\\plain\\intbl\\sb0\\sa0",font_ls,"\\cf1")
  
  pagen_txt<-""
  if(!is.null(reporttitle) &!any(is.na(reporttitle))){
    if(protectspecialchars==TRUE){
      reporttitle<-gsub("\\\\", "\\\\\\\\\\1", reporttitle,perl = TRUE)
    }else{
      reporttitle<-gsub("\\\\([^a-zA-Z])", "\\\\\\\\\\1", gsub("\\\\(\\\\)", "\\\\\\\\\\\\", reporttitle,perl = TRUE),perl = TRUE)
    }
    for(i in 1:4){
      if(is.na(reporttitle[i])){ reporttitle[i]<-"" }
    }
    if(nchar(reporttitle[4])<1){
      pagen_txt<-"\\u31532; {\\field{\\*\\fldinst { PAGE }}} \\u39029; \\u20849; {\\field{\\*\\fldinst { NUMPAGES }}} \\u39029;"
    }else{ pagen_txt<-reporttitle[4] }
    header_rtf_txtcw<-paste0("\\trowd\\trkeep\\trqc\n",tb_cellw_temp,as.integer(max_colw/2),"\n",paste0(tb_cellw_temp,max_colw))
    hlabel_txt<-reporttitle[1]
    if(nchar(hlabel_txt)>0){
      #hlabel_txt<-gsub("\\\\([a-zA-Z])", "\\\\\\\\\\1", hlabel_txt,perl = TRUE)
      hlabel_txt<-.to_rtffontcode(hlabel_txt)
    }
    header_rtf_txtc1<-paste0(tab_txt_pre_temp,"\\ql\\fs",fontsize*2,"{",hlabel_txt,"\\cell}")
    header_rtf_txtc2<-paste0(tab_txt_pre_temp,"\\qr\\fs",fontsize*2,"{",pagen_txt,"\\cell}")
    header_rtf_txt<-paste(header_rtf_txtcw,header_rtf_txtc1,header_rtf_txtc2,"{\\row}",sep="\n")
    hlabel_txt<-reporttitle[2]
    if(nchar(hlabel_txt)>0){
      #hlabel_txt<-gsub("\\\\([a-zA-Z])", "\\\\\\\\\\1", hlabel_txt,perl = TRUE)
      hlabel_txt<-.to_rtffontcode(hlabel_txt)
    }
    header_rtf_txtc1<-paste0(tab_txt_pre_temp,"\\ql\\fs",fontsize*2,"{",hlabel_txt,"\\cell}")
    hlabel_txt<-reporttitle[3]
    if(nchar(hlabel_txt)>0){
      #hlabel_txt<-gsub("\\\\([a-zA-Z])", "\\\\\\\\\\1", hlabel_txt,perl = TRUE)
      hlabel_txt<-.to_rtffontcode(hlabel_txt)
    }
    header_rtf_txtc2<-paste0(tab_txt_pre_temp,"\\qr\\fs",fontsize*2,"{",hlabel_txt,"\\cell}")
    header_rtf_txt<-paste(header_rtf_txt,header_rtf_txtcw,header_rtf_txtc1,header_rtf_txtc2,"{\\row}",sep="\n")
    
    if (bodytitle==FALSE){ 
      header_rtf_txt<-paste(header_code,header_rtf_txt,"@*#HEADERTITLE#*@","}}",sep="\n") 
      rtf_ori_code<-gsub("\\\\headery\\d+\\\\footery\\d+",paste0("\\\\headery",head_footery[1]*1440,paste0("\\\\footery",head_footery[2]*1440)), rtf_ori_code)
    }
  }else{ header_rtf_txt="" }
  
  ############# keep options ###################
  .trf_options<-list(outpath=outpath,reportname=reportname,reporttitle=reporttitle,papersize =papersize 
                     ,fontsize=fontsize,font=font,marglrtb=marglrtb,orientation=orientation,bodytitle=bodytitle
                     ,paperwh=paperwh,max_colw=max_colw,header_code=header_code,footer_code=footer_code
                     ,pagen_txt=pagen_txt,tb_cellw_temp=tb_cellw_temp,tab_txt_pre_temp=tab_txt_pre_temp
                     ,lstobj_type=NULL,protectspecialchars=protectspecialchars,header_rtf_txt=header_rtf_txt)  
  
  .trf_options$rtf_code<-paste(rtf_start_code,rtf_start_font,rtf_color,rtf_ori_code,header_rtf_txt,sep="\n")
  .trf_options<<-.trf_options
}

#' 输出RTF文档的结束位置function
#'
#' 输出RTF文档的结束位置function，注意调用时需要最后面加上“()”，如：rtf_end()。
#'
#'
#' @examples
#' library(ggplot2)
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' rtf_start(reportname = "null_rtf.rtf",reporttitle = c("XXXX","Report for IDMC","Sponsor",""))
#' rtf_addtextline(text="Table 1：RTF file output table test",style="title")
#' rtf_pagebreak()
#' rtf_plot(mt,width = 10, height = 5.4)
#' rtf_end()
#'
#' @export
############# end rtf #############
rtf_end <- function(){
  .trf_options$rtf_code<<-gsub("@*#HEADERTITLE#*@", "",.trf_options$rtf_code, fixed = TRUE)
  .trf_options$rtf_code<<-paste(.trf_options$rtf_code,"}",sep="\n")
  write(.trf_options$rtf_code,file=paste(.trf_options$outpath,.trf_options$reportname,sep="/"))
  rm(list=c(".trf_options"),envir = .GlobalEnv)
}

#' 在RTF文档中插入图片的function
#'
#' 在RTF文档中插入图片的function，参数除了x是绘图对象object，其他都是devEMF包中emf function或者grDevices包（png/bmp）function参数，所以具体参数参考devEMF包(emf)或grDevices包（png/bmp）说明。
#'
#' @param x 一个绘图对象
#' @param imagefmt 指定输出图片的格式，可选emf/png/bmp 
#'
#' @examples
#' library(ggplot2)
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' rtf_start(reportname = "null_rtf.rtf",reporttitle = c("XXXX","Report for IDMC","Sponsor",""))
#' rtf_addtextline(text="Table 1：RTF file output table test",style="title")
#' rtf_pagebreak()
#' rtf_plot(mt,width = 10, height = 5.4)
#' rtf_end()
#'
#' @export
############# plot/figure rtf #############
rtf_plot <- function(x,imagefmt="emf", width = 7, height = 7,bg = "transparent", fg = "black"
                      ,pointsize = 12,family = "Arial", coordDPI = 1000, custom.lty=TRUE,emfPlus=TRUE,
                      emfPlusFont = FALSE, emfPlusRaster = FALSE,emfPlusFontToPath = FALSE
                      ,res=300,symbolfamily="default"){
  imagefmt<-tolower(imagefmt)
  if(!(imagefmt %in% c("emf","bmp","png"))){
    stop("[ERROR]: [imagefmt]value must be emf/png/bmp!")
  }
  if(imagefmt=="emf"){
    if(!require('devEMF')) {
      install.packages('devEMF')
      library('devEMF')
    }
  }
  if(imagefmt %in% c("bmp","png")){
    if(!require('grDevices')) {
      install.packages('grDevices')
      library('grDevices')
    }
    family<-""
    height<-height*res
    width<-width*res
  }
  imagefname<-paste0("R_temp_plot.",tolower(imagefmt))
  
  if (file.exists(imagefname)){
    file.remove(imagefname)
  }
  if(imagefmt=="emf"){
    emf(file = imagefname, width = width, height = height, units ="in",bg = bg, fg = fg
        ,pointsize = pointsize,family = family, coordDPI = coordDPI, custom.lty=custom.lty,emfPlus=emfPlus,
        emfPlusFont = emfPlusFont, emfPlusRaster = emfPlusRaster,emfPlusFontToPath = emfPlusFontToPath)
  }
  if(imagefmt=="bmp"){
    bmp(filename =imagefname,width = width, height=height, units="px", pointsize=pointsize,
        bg = bg,family = family,res=res,symbolfamily=symbolfamily)
  }
  if(imagefmt=="png"){
    png(filename =imagefname,width = width, height=height, units="px", pointsize=pointsize,
        bg = bg,family = family,res=res,symbolfamily=symbolfamily)
  }
  
  print(x)
  dev.off()
  Sys.sleep(1)
  ########### read emf #########
  max.bytes<-50000000  # maximum file size in bytes (~50MB)
  emf_data<-readBin(imagefname, what="raw", size=1, signed=TRUE, endian="little",n=max.bytes)
  rtf_emf<-paste0("\\pard\\plain\\qc{{\\*\\shppict{\\pict\\",paste0(imagefmt,"blip","\\picwgoal",width*1440),paste0("\\pichgoal",height*1440))
  for(i in 1:ceiling(length(emf_data)/128)){
    rtf_emf<-paste(rtf_emf,paste0(emf_data[(128*(i-1)+1):(128*i)],collapse=""),sep="\n")
  }
  rtf_emf<-paste(rtf_emf,"}}}\\par",sep="\n")
  .trf_options$rtf_code<<-paste(.trf_options$rtf_code,rtf_emf,sep="\n")
  file.remove(imagefname)
  #print(dev.list())
}

#' 在RTF文档中插入文本行的function
#'
#' 在RTF文档中插入文本行的function。
#'
#' @param text 指定添加进RTF的文本。
#' @param fontsize 指定字体大小.
#' @param style 指定输出的样式，title样式会生成导航栏书签，footnote时将文本插入页脚。
#' @param align 指定文本对齐方式.
#' @param bold 指定是否加粗文本.
#' @param protectspecialchars 指定是否对特殊文本进行保护。设置为TRUE时，文本中的RTF码将视为普通文本.
#' @param inheader 指定是否将文本插入header(页眉)中，仅对style=title时有效.
#'
#' @examples
#' library(ggplot2)
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' rtf_start(reportname = "null_rtf.rtf",reporttitle = c("XXXX","Report for IDMC","Sponsor",""))
#' rtf_addtextline(text="Table 1：RTF file output table test",style="title")
#' rtf_pagebreak()
#' rtf_plot(mt,width = 10, height = 5.4)
#' rtf_end()
#'
#' @export
############# add textline rtf #############
rtf_addtextline <- function(text=NULL,fontsize=10.5,style="normal",align = "left",bold=FALSE
                             ,protectspecialchars=FALSE,inheader=NULL){
  if (!is.null(text) &!is.na(text)){
    align_txt<-"\\ql"
    if (toupper(align) %in% c("CENTER","C")){ align_txt<-"\\qc" }
    if (toupper(align) %in% c("RIGHT","R")){ align_txt<-"\\qr" }
    bold_txt<-"\\b0"
    if (bold==TRUE){ bold_txt<-"\\b" }
    if(is.null(inheader)){
      if(.trf_options$bodytitle==FALSE){inheader<-TRUE} else{inheader<-FALSE}
    }
    
    if(protectspecialchars==TRUE){
      text<-gsub("\\\\", "\\\\\\\\\\1", text,perl = TRUE)
    }else{
      text<-gsub("\\\\([^a-zA-Z])", "\\\\\\\\\\1", gsub("\\\\(\\\\)", "\\\\\\\\\\\\", text,perl = TRUE),perl = TRUE)
    }
    text<-.to_rtffontcode(text)
    tb_cellw_temp<-paste0("\\trowd\\trkeep\\trqc\n",.trf_options$tb_cellw_temp,.trf_options$max_colw,"\n")
    rtf_textline<-paste0(tb_cellw_temp,.trf_options$tab_txt_pre_temp,"\\fs",fontsize*2,align_txt,"{",text,"\\cell}{\\row}")
    rtf_textline_bk<-rtf_textline
    if (tolower(style)=="title"){
      bold_txt<-"\\b"
      tab_indx_precode<-"\\pard{\\par}{\\upr{\\*\\bkmkstart IDX}{\\*\\ud{\\*\\bkmkstart IDX}}}{\\*\\bkmkend IDX}"
      rtf_textline<-paste0(tab_indx_precode,"\\b\\sb0\\sa0\\ql",.trf_options$font_ls,"\\cf1\\outlinelevel1\\fs",fontsize*2,bold_txt,align_txt,"{",text,"}\\par")
    }
    if (tolower(style)=="footnote"){
      rtf_textline<-paste("{\\footer\\pard\\plain\\qc{",rtf_textline,"}}",sep="\n")
    }
    if(inheader==TRUE&tolower(style)=="title"){
      .trf_options$rtf_code<<-gsub("@*#HEADERTITLE#*@", rtf_textline_bk,.trf_options$rtf_code, fixed = TRUE)
      rtf_textline<-gsub("\\\\fs\\d+\\\\", "\\\\fs0\\\\", gsub("\\\\cf\\d+\\\\", "\\\\cf8\\\\", rtf_textline,perl = TRUE),perl = TRUE)
      .trf_options$rtf_code<<-paste(.trf_options$rtf_code,rtf_textline,sep="\n")
    }
    else{.trf_options$rtf_code<<-paste(.trf_options$rtf_code,rtf_textline,sep="\n")}
  }
}

#' 在RTF文档中添加换页function
#'
#' 在RTF文档中添加换页function，注意调用时需要最后面加上“()”，如：rtf_pagebreak () 。
#'
#' @examples
#' library(ggplot2)
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' rtf_start(reportname = "null_rtf.rtf",reporttitle = c("XXXX","Report for IDMC","Sponsor",""))
#' rtf_addtextline(text="Table 1：RTF file output table test",style="title")
#' rtf_pagebreak()
#' rtf_plot(mt,width = 10, height = 5.4)
#' rtf_end()
#'
#' @export
############# pagebreak rtf #############
rtf_pagebreak <- function(){
  pagebreak_code<-"\\pard\\b0\\i0\\chcbpat0{\\page\\par}"
  .trf_options$rtf_code<<-paste0(.trf_options$rtf_code,pagebreak_code)
}

#' 在RTF文档中插入与“rtf_start”function中相同的reporttitle文本的function
#'
#' 在RTF文档中插入与“rtf_start”function中相同的reporttitle文本的function，注意调用时需要最后面加上“()”，如：rtf_addreporttitle () 。
#'
#' @examples
#' library(ggplot2)
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' rtf_start(reportname = "null_rtf.rtf",reporttitle = c("XXXX","Report for IDMC","Sponsor",""))
#' rtf_addtextline(text="Table 1：RTF file output table test",style="title")
#' rtf_pagebreak()
#' rtf_addreporttitle ()
#' rtf_plot(mt,width = 10, height = 5.4)
#' rtf_end()
#'
#' @export
############# add reporttitle rtf #############
rtf_addreporttitle <- function(){
  .trf_options$rtf_code<<-paste(.trf_options$rtf_code,.trf_options$header_rtf_txt,sep="\n")
}

################ add table rtf ##################
.find_mergehd<-function(inds,groupvar=NULL){
  inds$retain_value <- NA
  inds$retain_max <- NA
  for(i in 1:nrow(inds)) {
    if(i == 1) {
      inds$retain_value[i] <- 0
    } else if(inds[i,groupvar] == inds[i-1,groupvar]) {
      inds$retain_value[i] <- inds$retain_value[i-1]
    }else if(inds[i,groupvar] != inds[i-1,groupvar]) {
      inds$retain_value[i] <- inds$retain_value[i-1]+1
      inds$retain_max[i-1] <- 9
    }
    if(i == nrow(inds)) {
      inds$retain_max[i] <- 9
    }
  }
  return(inds)
}

.count_delimiters <- function(vector, delimiter) {
  sapply(vector, function(x) {
    if(is.na(x)) return(0)  # 处理NA值
    splits <- strsplit(x, delimiter, fixed = TRUE)[[1]]
    return(length(splits) - 1)
  })
}

#' 在RTF文档中插入表格的function
#'
#' 在RTF文档中插入表格的function，具体见使用说明文档。
#'
#' @param indat 指定在RTF文档中添加的表格，必须是data.frame
#' @param varlist 包含信息：var/header/width/merge header/…/\\align\\order，具体见使用说明文档。
#' 需列出的变量及其显示名称，但是需要注意的是，如果需要指定“\\align\\order”部分必须在其前面加上“/”,例如“/\\align\\order”.
#' @param norecord 指定当导入的数据集观测数为0的时候输出的文本.
#' @param protectspecialchars 指定是否对特殊文本进行保护。设置为TRUE时，文本中的RTF码将视为普通文本.
#' @param perpage_obs 指定每页输出行数，当设置为NA时表示直接把数据集观测数作为每页行数，当指定具体行数num时按指定的具体行数num分页，当指定分页变量（pagebrak varibale）时则按分页变量值分页（注意变量名需要加上引号）.
#'
#' @examples
#' library(ggplot2)
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' rtf_start(reportname = "null_rtf.rtf",reporttitle = c("XXXX","Report for IDMC","Sponsor",""))
#' rtf_addtextline(text="Table 1：RTF file output table test",style="title")
#' rtf_addtable(adae1,varlist=c("SUBJID/label 01/10/\\l\\order"
#'                             ,"TRT01P/label 02/10"
#'                             ,"AETERM/label 03/10/header%\\test02/header zz/\\c"
#'                             ,"AESOC/label 04/10/header%\\test02/header zz/\\r"))
#' rtf_end()
#'
#' @export
rtf_addtable <- function(indat,varlist=NULL,norecord ="No Observed Value Was Reported."
                          ,protectspecialchars=FALSE,perpage_obs=NA){
  ######### parameter control ###
  if(!missing(indat)){
    if(!exists("indat")){
      stop("[ERROR]: Dataset[indat] not exist!")
    }
    if(!is.data.frame(indat)){stop("[ERROR]: Dataset[indat] must be dataframe!")}
  }else{
    stop("[ERROR]: Function Value[indat] is uninitialized!")
  }
  indat<-as.data.frame(indat)
  lstrvar_seq<-4
  if (!is.null(varlist) ){
    if(protectspecialchars==TRUE){
      varlist<-gsub("%\\\\", "%\\\\\\\\\\1", varlist,perl = TRUE)
    }else{
      varlist<-gsub("%\\\\([^a-zA-Z])", "%\\\\\\\\\\1", varlist,perl = TRUE)
    }
    varlist<-gsub("%/", "$#@",gsub("%\\\\", "$##@",varlist))
    var_attrib<-data.frame(varlist=varlist)
    count<-.count_delimiters(varlist,"/")
    var_attrib<-.split_var(var_attrib,var="varlist",split="/",prefix="rsplit")
    max_cz<-max(count)+1
    for(r in 1:nrow(var_attrib)){
      max_c<-count[r]+1
      lstvar1<-var_attrib[r,paste0("rsplit",max_c)]
      for(c in max_cz:max_c){
        if(grepl("\\\\",lstvar1)){
            if(c==max_cz){ var_attrib[r,paste0("rsplit",c)]<-lstvar1 }
            else { var_attrib[r,paste0("rsplit",c)]<-"" }
        }else { 
          if(is.na(var_attrib[r,paste0("rsplit",c)])){ var_attrib[r,paste0("rsplit",c)]<-"" } 
          var_attrib[r,paste0("rsplit",max_cz)]<-"\\L" 
        }
      }
    }
    lstrvar_seq<-ncol(var_attrib)-1
    lstrvar<-paste("rsplit",lstrvar_seq,sep="")
    all(substr(var_attrib[,lstrvar],1,1)=="\\")
    for(i in 1:nrow(var_attrib)){
      if(substr(var_attrib[i,lstrvar],1,1)=="\\"){ var_attrib[i,lstrvar]<-substring(var_attrib[i,lstrvar],2) } 
    }
    var_attrib<-.split_var(var_attrib,var=lstrvar,split="\\",prefix="lsplit")
    varls<-names(var_attrib)
    if(!("lsplit1" %in% varls)){ var_attrib$lsplit1<-"L" }
    if(!("lsplit2" %in% varls)){ var_attrib$lsplit2<-NA }
    
    varls<-names(var_attrib)
    varls1<-varls[substr(varls,1,6)=='rsplit']
    for(i in 1:ncol(var_attrib)){
      var_attrib[,i]<-gsub("\\$#@","/", gsub("\\$##@","\\\\", var_attrib[,i]))
    }
    # for(i in 1:nrow(var_attrib)){
    #   if(var_attrib$lsplit1[i]==""|is.na(var_attrib$lsplit1[i])){ var_attrib$lsplit1[i]<-"L" }
    # }
    #length(varls1)
    if(any(!c("rsplit1","rsplit2","rsplit3") %in% varls)==TRUE){
      stop("[ERROR]: The parameter[varlist] must contain at least variable, label and width!")
    }
  }else{
    if(ncol(indat)>20){indat<-indat[,1:20]}
    varls<-names(indat)
    vlabel<-varls
    if(protectspecialchars==TRUE){
      vlabel<-gsub("\\\\", "\\\\\\\\\\1", vlabel,perl = TRUE)
    }else{
      vlabel<-gsub("\\\\([^a-zA-Z])", "\\\\\\\\\\1", gsub("\\\\(\\\\)", "\\\\\\\\\\\\", vlabel,perl = TRUE),perl = TRUE)
    }
    for(i in 1:length(varls)){
      if(!is.null(attr(indat[,i],"label"))){vlabel[i]<-attr(indat[,i],"label")}
    }
    var_attrib<-data.frame(rsplit1=varls,rsplit2=vlabel,rsplit3=1/length(varls)*100
                           ,lsplit1="L",lsplit2=NA)
  }
  #indat11<<-indat
  if(is.na(perpage_obs)|grepl('^\\d+$', as.character(perpage_obs), perl = TRUE)){
    if(nrow(indat)>0){ 
      indat$per_pageobs<-1
    }else{ indat$per_pageobs<-numeric(0) }
  }else{
    indat$per_pageobs<-indat[,perpage_obs]
  }
  #indat12<<-indat
  ordervar<-c("per_pageobs",var_attrib$rsplit1[toupper(var_attrib$lsplit2) %in% c("ORDER","GROUP")])
  if(nrow(indat)>0 & length(ordervar)>0){
    sort_list <- lapply(ordervar, function(var) indat[[var]])
    names(sort_list) <- ordervar
    indat <- indat[do.call(order, sort_list), ]
    #print("order var")
  }
  rownames(indat)<-NULL
  if(nrow(indat)>0 & grepl('^\\d+$', as.character(perpage_obs), perl = TRUE)){
    indat$x_nrown<-c(1:nrow(indat))
    indat$per_pageobs<-ceiling(indat$x_nrown/perpage_obs)
  }
  if(nrow(indat)>0){indat<-.find_mergehd(indat,groupvar="per_pageobs")}
  indat13<<-indat
  ########## add table ########
  var_attrib<-transform(var_attrib,width=ifelse(grepl("^(\\d|\\.)+$",rsplit3),as.numeric(rsplit3),5))
  
  sumwidth<-sum(var_attrib$width)
  if(sumwidth<100){
    var_attrib<-transform(var_attrib,width=width/sumwidth*100)
  }
  var_attrib<-transform(var_attrib,cellw=cumsum(as.integer(width/100*.trf_options$max_colw)))
  if(var_attrib$cellw[nrow(var_attrib)]!=.trf_options$max_colw){
    var_attrib$cellw[nrow(var_attrib)]=.trf_options$max_colw
  }
  tb_cellw_temp_t<-"\\cltxlrtb\\clbrdrt\\brdrs\\clvertalt\\clcbpat20\\cellx"
  tb_cellw_temp_b<-"\\cltxlrtb\\clbrdrb\\brdrs\\clvertalt\\clcbpat20\\cellx"
  tb_cellw_temp_tb<-"\\cltxlrtb\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clvertalt\\clcbpat20\\cellx"
  
  tab_txt_pre_temp<-paste0(.trf_options$tab_txt_pre_temp,"\\fs",.trf_options$fontsize*2)
  tab_pre_code<-"\\trowd\\trkeep\\trql\n"
  nulltabtab_pre_code_tb<-paste0(tab_pre_code,paste0(tb_cellw_temp_tb,.trf_options$max_colw,"\n"))
  tab_header<-""
  
  #################### table header ##################
  ############# merged header ##########
  merge_header_z<-""
  merge_headeryn<-FALSE
  if((lstrvar_seq-1)>4){
    merge_headeryn<-TRUE
    for(i in (lstrvar_seq-1):4){
      mg_var<-paste0("rsplit",i)
      mergehd<-.find_mergehd(var_attrib,groupvar=mg_var)
      mergehd<-mergehd[!is.na(mergehd$retain_max),]
      merge_header<-""
      merge_pre_code<-tab_pre_code
      for(j in 1:nrow(mergehd)){
        mgheader_txt<-mergehd[j,mg_var]
        # mgheader_txt<-gsub("\\\\", "\\\\\\\\\\1", mgheader_txt,perl = TRUE)
        if(nchar(mgheader_txt)>0){ mgheader_txt<-.to_rtffontcode(mgheader_txt) }
        if(i==(lstrvar_seq-1)){
          tb_cellw_temp_1<-ifelse(is.na(mgheader_txt)|mgheader_txt=="",tb_cellw_temp_t,tb_cellw_temp_tb)
        }else{
          tb_cellw_temp_1<-ifelse(is.na(mgheader_txt)|mgheader_txt=="",.trf_options$tb_cellw_temp,tb_cellw_temp_tb)
        }
        if(nchar(mgheader_txt)>0){
          mgheader_txt<-gsub("\\$", "\\\\\\par ",mgheader_txt)
          merge_header<-paste0(merge_header,tab_txt_pre_temp,"\\qc{",mgheader_txt,"\\cell}\n")
        }else {
          merge_header<-paste0(merge_header,"\\pard\\plain\\intbl{\\cell}\n")
        }
        merge_pre_code<-paste0(merge_pre_code,paste0(tb_cellw_temp_1,mergehd$cellw[j],"\n"))
      }
      merge_header_z<-paste0(merge_header_z,merge_pre_code,merge_header,"{\\row}\n")
    }
  }
  #################general header ##############
  for(c in 1:nrow(var_attrib)){
    tb_align_txt<-"\\ql"
    if (toupper(var_attrib$lsplit1[c]) %in% c("CENTER","C")){ tb_align_txt<-"\\qc" }
    if (toupper(var_attrib$lsplit1[c]) %in% c("RIGHT","R")){ tb_align_txt<-"\\qr" }
    label_txt<-var_attrib$rsplit2[c]
    if(nchar(label_txt)>0){
      #label_txt<-gsub("\\\\([a-zA-Z])", "\\\\\\\\\\1", label_txt,perl = TRUE)
      label_txt<-.to_rtffontcode(label_txt)
      label_txt<-gsub("\\$", "\\\\\\par ",label_txt)
      tab_header<-paste0(tab_header,tab_txt_pre_temp,tb_align_txt,"{",label_txt,"\\cell}\n")
    }else {
      tab_header<-paste0(tab_header,"\\pard\\plain\\intbl{\\cell}\n")
    }
    tab_pre_code<-paste0(tab_pre_code,paste0(.trf_options$tb_cellw_temp,var_attrib$cellw[c],"\n"))
  }
  tab_pre_code_b<-gsub("\\\\cltxlrtb","\\\\cltxlrtb\\\\clbrdrb\\\\brdrs", tab_pre_code)
  tab_pre_code_tb<-gsub("\\\\cltxlrtb","\\\\cltxlrtb\\\\clbrdrb\\\\brdrs\\\\clbrdrt\\\\brdrs", tab_pre_code)
  tab_pre_code_1<-tab_pre_code_tb
  if(merge_headeryn==TRUE){
    tab_pre_code_1<-tab_pre_code_b
  }
  
  rtf_table<-paste0(merge_header_z,tab_pre_code_1,tab_header,"{\\row}\n")
  rtf_table<-gsub("\\\\trkeep","\\\\trkeep\\\\trhdr", rtf_table)
  outdat<-indat[,var_attrib$rsplit1]
  #print("output data")
  pagebreak_code<-paste0("\\pard\\b0\\i0\\chcbpat0{\\page\\par}\n",rtf_table)
  outdat1<<-outdat
  if(nrow(outdat)>0){
    for(r in 1:nrow(outdat)){
      tab_txt_cd<-""
      for(c in 1:ncol(outdat)){
        tb_align_txt<-"\\ql"
        if (toupper(var_attrib$lsplit1[c]) %in% c("CENTER","C")){ tb_align_txt<-"\\qc" }
        if (toupper(var_attrib$lsplit1[c]) %in% c("RIGHT","R")){ tb_align_txt<-"\\qr" }
        tab_txt_rtf<-as.character(outdat[r,c])
        if (toupper(var_attrib$lsplit2[c]) %in% c("ORDER","GROUP")& r>1){ 
          if(outdat[r-1,c]==outdat[r,c]){ tab_txt_rtf<-"" } 
        }
        if (nchar(tab_txt_rtf)>0){
          if(protectspecialchars==TRUE){
            tab_txt_rtf<-gsub("\\\\", "\\\\\\\\\\1", tab_txt_rtf,perl = TRUE)
          }else{
            tab_txt_rtf<-gsub("\\\\([^a-zA-Z])", "\\\\\\\\\\1", gsub("\\\\(\\\\)", "\\\\\\\\\\\\", tab_txt_rtf,perl = TRUE),perl = TRUE)
          }
          tab_txt_rtf<-.to_rtffontcode(tab_txt_rtf)
        }else { tab_txt_rtf<-"" }
        if(nchar(tab_txt_rtf)>0){
          tab_txt_cd<-paste0(tab_txt_cd,tab_txt_pre_temp,tb_align_txt,"{",tab_txt_rtf,"\\cell}\n")
        }else {
          tab_txt_cd<-paste0(tab_txt_cd,"\\pard\\plain\\intbl{\\cell}\n")
        }
      }
      if(r<nrow(outdat)& is.na(indat[r,"retain_max"])){ rtf_table<-paste0(rtf_table,tab_pre_code,tab_txt_cd,"{\\row}\n") }
      else { rtf_table<-paste0(rtf_table,tab_pre_code_b,tab_txt_cd,"{\\row}\n") }
      ##page break
      #print(paste0("rown:",r))
      if(!is.na(indat[r,"retain_max"])& indat[r,"retain_max"]==9 &r<nrow(outdat)){ #print(paste0("rown:",r))
        rtf_table<-paste0(rtf_table,pagebreak_code)
      }
    }
  }else{
    rtf_table<-paste0(rtf_table,nulltabtab_pre_code_tb,paste0(tab_txt_pre_temp,"\\qc{\\par\\par\\par",.to_rtffontcode(norecord),"\\par\\par\\cell}\n"),"{\\row}")
  }
  .trf_options$rtf_code<<-paste(.trf_options$rtf_code,rtf_table,sep="\n")
}



