install.packages("XML")
install.packages("RCurl")
install.packages("stringr")
install.packages("plotly")
install.packages("ggplot2")
install.packages("lubridate")


graphics.off()
genie_chart<-function(){
  
  library(XML)
  library(RCurl)
  library(stringr)  # str_replace_all 함수 사용
  library(ggplot2)
  library(plotly)
  library(lubridate)
  
  graphics.off()
  
  slc<-c('실시간 top 100 가수별 점유율', '실시간 top 100 장르별 점유율','top100에서 가장 오래된 노래','1-4위 차트변동 추이','계절별 인기 장르','기획사별 점유율','3년간 장르 점유율 변동 추이','인기가수 및 노래')
  x1<-menu(slc,title = 'genie 차트 보기',graphics=T)
  
  x2<-0
  if (x1 ==  4)
    p<-readline(prompt = '최근 몇개월의 변동을 보시겠습니까?')
  else if (x1 == 5)
    x2<-menu(c("봄","여름","가을","겨울","작년전체"),title = '계절 선택',graphics=T)
  else if (x1==6)
    x2<-5
  else if (x1==8)
    x2<-menu(c('인기가수','인기노래'),title= '가수/노래',graphics=T)
  print(x2)
  node_onclick = function(node) xmlAttrs(node)["onclick"]   # 앨범 번호를 가지고온다. (장르, 기획사)
  
  
  get_inf<-function(i,j,PARSED){
    album_n<- xpathSApply(PARSED, paste("//*[@id='body-content']/div[6]/div/table/tbody/tr[",i,"]/td[3]/a"), node_onclick)
    album_n<-str_replace_all(album_n,"[^\\d]","" )   # 숫자가 아닌 문자를 지운다.
    
    inf_url<-gsub(' ','',paste("http://www.genie.co.kr/detail/albumInfo?axnm=",album_n))
    SOURCE2 = getURL(inf_url)   # url 정보를 가져옴
    PARSED_a = htmlParse(SOURCE2, encoding = "UTF-8")  
    
    genre <- xpathSApply(PARSED_a,paste("//*[@id='body-content']/div[2]/div[2]/ul/li[2]/span[2]"),xmlValue) #장르
    agency <- xpathSApply(PARSED_a,paste("//*[@id='body-content']/div[2]/div[2]/ul/li[4]/span[2]"),xmlValue) #기획사
    c_day<- xpathSApply(PARSED_a,paste("//*[@id='body-content']/div[2]/div[2]/ul/li[5]/span[2]"),xmlValue)
    c_day<- gsub('\\.','-',c_day)
    c_day<- gsub('\n','',c_day)
    
    
    if (j==2){
      rank<-i+50
      # PARSED<-PARSED2
    }
    else
      rank<-i
    s_name <-xpathSApply(PARSED,paste("//*[@id='body-content']/div[6]/div/table/tbody/tr[",i,"]/td[5]/a[1]/text()"),xmlValue)
    s_name<-gsub("\n",'',s_name)
    s_name<-gsub(' ','',s_name)
    artist <-xpathSApply(PARSED,paste("//*[@id='body-content']/div[6]/div/table/tbody/tr[",i,"]/td[5]/a[2]"),xmlValue)
    
    return(c(rank,s_name,artist,genre,agency,c_day))
  }
  
  top_100 <- function(n){
    
    chart<-data.frame()
    for (j in 1:2){  #1,2 페이지
      url = paste("http://www.genie.co.kr/chart/top200?ditc=D&ymd=20180614&hh=17&rtm=Y&pg=",j)
      url<-gsub(' ','',url)
      SOURCE = getURL(url)   # url 정보를 가져오고
      PARSED = htmlParse(SOURCE, encoding = "UTF-8")  
      print(paste('url:',url))
      for(i in 1:50){
        if(i==1&j==1)
          chart<-get_inf(i,j,PARSED)
        else
          chart<-rbind(chart,get_inf(i,j,PARSED))
      }
    }
    
    colnames(chart)<-c("순위","제목","아티스트","장르","기획사",'발매일')
    print(chart)
    
    c<-as.data.frame(chart[,n])
    a<-aggregate(chart[,n],c,length)
    
    if (n==3){
      aa<-a[order(a$x,decreasing = T),]
      aa<-aa[1:10,] 
      aa<-data.frame(aa)
      
      colnames(aa)<-c('아티스트','건수')
      ggplot(aa, aes(아티스트, 건수, fill=아티스트))+geom_bar(stat="identity") +coord_flip()+  #가로출력
        geom_text(aes(y = 건수, label = paste(건수,'%'), size = 6, vjust = 0.2,hjust=1.2),color='white')  
    }
    else if (n==4){
      aa<-a[1:100,]
      aa<-na.omit(aa)
      colnames(aa)<-c('장르','건수')
      ggplot(aa, aes(장르, 건수, fill=장르))+geom_bar(stat="identity")+ 
        geom_text(aes(y = 건수, label = paste(건수,'%'), size = 3 , vjust = 1.2 ,hjust=0.4),color='white')  
    }
  }
  
  old_song<-function(){
    
    chart<-data.frame()
    for (j in 1:2){
      url = paste("http://www.genie.co.kr/chart/top200?ditc=D&ymd=20180614&hh=17&rtm=Y&pg=",j)
      url<-gsub(' ','',url)
      SOURCE = getURL(url)   # url 정보를 가져오고
      PARSED = htmlParse(SOURCE, encoding = "UTF-8")  
      print(url)
      for(i in 1:50){
        if(i==1&j==1){
          chart<-get_inf(i,j,PARSED)
        }
        else
          chart<-rbind(chart,get_inf(i,j,PARSED))
      }
    }
    
    chart<-cbind(chart,'n')
    colnames(chart)<-c("순위","제목","아티스트","장르","기획사",'발매일','오늘-발매일')
    
    x<-Sys.Date()-as.Date(chart[,6])
    x<-str_replace_all(x,"[^\\d]","" )
    
    for (i in 1:100)
      chart[i,7]<-x[i]
    
    z<-data.frame(name=chart[,2],num=as.numeric(chart[,7]))
    z<-z[order(z$num,decreasing=T),]
    
    z<-z[1:10,]
    print(z)
    colnames(z)<-c('제목','일수')
    ggplot(z, aes(제목, 일수, fill=제목))+geom_bar(stat="identity") + 
      geom_text(aes(y = 일수, label = 일수, size = 3 , vjust = 1.2 ,hjust=0.4),color='white')  
  }
  
  get_inf2<-function(i,j,PARSED,ym_s){
    rank<-i
    s_name <-xpathSApply(PARSED,paste("//*[@id='body-content']/div[4]/div/table/tbody/tr[",i,"]/td[5]/a[1]"),xmlValue)
    s_name<-gsub("\n",'',s_name)
    s_name<-gsub(' ','',s_name)
    artist <-xpathSApply(PARSED,paste("//*[@id='body-content']/div[4]/div/table/tbody/tr[",i,"]/td[5]/a[2]"),xmlValue)
    
    return(c(rank,s_name,ym_s,artist))
  }
  
  ex_m<-function(n){
    
    add.months= function(date,a) seq(date, by = paste (a, "months"), length = 2)[2]
    
    chart<-data.frame()
    for (j in 1:n){
      ym<-add.months(Sys.Date(),-j)
      ym<-str_replace_all(ym,"[^\\d]","" )   # 숫자가 아닌 문자를 지운다.
      url = paste("http://www.genie.co.kr/chart/top200?ditc=M&rtm=N&ymd=",ym)
      url<-gsub(' ','',url)
      SOURCE = getURL(url)   # url 정보를 가져오고
      PARSED = htmlParse(SOURCE, encoding = "UTF-8")  
      print(url)
      
      for(i in 1:4){
        if(i==1&j==1){
          chart<-get_inf2(i,j,PARSED,substr(ym,1,6))
        }
        else
          chart<-rbind(chart,get_inf2(i,j,PARSED,substr(ym,1,6)))
        #%>% add_trace(y = ~뿜뿜, name = '뿜뿜', line = list(color = 'rgb(22, 16, 29)', width = 4))
      }
    }
    
    
    t<-tapply(as.numeric(chart[,1]), list(chart[,3],chart[,2]),sum)
    r<-rownames(t)
    t[is.na(t)]<-5
    
    t2<-t
    colnames(t2) <- str_replace_all(colnames(t),"[:punct:]","" )
    x<-colnames(t)
    x<-str_replace_all(x,"[:punct:]","" )
    
    p<-plot_ly(data.frame(t2), x = r, y = as.formula(paste('~', x[1])), name = colnames(t)[1], type = 'scatter', mode = 'lines',
               line = list( width = 4)) # %>%
    
    for (i in 2:length(colnames(t)))
      p<- p %>% add_trace(y = as.formula(paste('~', x[i])), name = colnames(t)[i], line = list(width = 4)) #%>%
    
    p %>% layout(title = "지니 차트 TOP4",
                 xaxis = list(title = "Months"),
                 yaxis = list (title = "Rank",range = c(4.5,0.4)))
  }
  
  get_inf3<-function(i,PARSED,ym_s){
    
    album_n<- xpathSApply(PARSED, paste("//*[@id='body-content']/div[4]/div/table/tbody/tr[",i,"]/td[3]/a/span"), node_onclick)
    album_n<-str_replace_all(album_n,"[^\\d]","" )   # 숫자가 아닌 문자를 지운다.
    print(album_n)
    
    inf_url<-gsub(' ','',paste("http://www.genie.co.kr/detail/albumInfo?axnm=",album_n))
    SOURCE2 = getURL(inf_url)   # url 정보를 가져오고
    PARSED_a = htmlParse(SOURCE2, encoding = "UTF-8")    # html 형태로 바꿔
    artist <-xpathSApply(PARSED,paste("//*[@id='body-content']/div[4]/div/table/tbody/tr[",i,"]/td[5]/a[2]"),xmlValue)
    s_name <-xpathSApply(PARSED,paste("//*[@id='body-content']/div[4]/div/table/tbody/tr[",i,"]/td[5]/a[1]"),xmlValue)
    s_name<-gsub("\n",'',s_name)
    s_name<-gsub(' ','',s_name)
    
    
    genre <- xpathSApply(PARSED_a,paste("//*[@id='body-content']/div[2]/div[2]/ul/li[2]/span[2]"),xmlValue) #장르
    agency <- xpathSApply(PARSED_a,paste("//*[@id='body-content']/div[2]/div[2]/ul/li[4]/span[2]"),xmlValue) #기획사
    
    return(c(i,genre,ym_s,agency,artist,s_name))
  }
  
  sw_p<-function(x2){
    
    add.years= function(date,n) seq(date, by = paste (n, "years"), length = 2)[2]
    chart<-data.frame()
    nn<-0
    
    g<-0
    if (x1==6)
      g<-2
    
    for (j in 1:(3-g)){
      ym<-add.years(Sys.Date(),-j)
      ym<-str_replace_all(ym,"[^\\d]","" )   # 숫자가 아닌 문자를 지운다.
      
      ss<-0
      if (x2==1)
        ss<- c(3:5)
      else if(x2==2)
        ss<- c(6:8)
      else if (x2==3)
        ss<- c(9:11)
      else if(x2==4)
        ss<- c(12,1,2)
      else if (x2 == 5)
        ss<-c(1:12)
      else 
        return(print('취소'))
      
      for (m in ss){
        if (m<10)
          ym<-paste(substr(ym,1,4),'0',m)
        else 
          ym<-paste(substr(ym,1,4),m)
        
        ym<-str_replace_all(ym," ","")   # 공백제거
        url = paste("http://www.genie.co.kr/chart/top200?ditc=M&rtm=N&ymd=",ym,"01")
        url<-gsub(' ','',url)
        SOURCE = getURL(url)   # url 정보를 가져오고
        PARSED = htmlParse(SOURCE, encoding = "UTF-8")  
        print(url)
        
        for(i in 1:50){        #1위부터 20위까지
          if(i==1&j==1&nn==0){
            chart<-get_inf3(i,PARSED,substr(ym,1,6))
            nn<-1
          }
          else
            chart<-rbind(chart,get_inf3(i,PARSED,substr(ym,1,6)))
        }
      }
    }
    chart<-data.frame(chart)
    colnames(chart)<-c("숫자","장르","월",'기획사','아티스트','제목')
    
    if (x1==5){
      t<- aggregate(as.numeric(chart$숫자)~(chart$장르+chart$월),chart,sum)
      tt<- aggregate(as.numeric(chart$숫자)~(chart$장르),chart,sum)
      tt<-cbind(tt,round((tt[,2]/nrow(chart)*100),1))
      
      colnames(tt)<-c("장르","건수","비율")
      tt<-tt[order(tt$건수,decreasing = T),]
      print(tt)
      
      colnames(t)<-c("장르","월","건수")
      
      bar<-ggplot(t, aes(x=월, y=건수, fill=장르))
      bar+geom_bar(stat="identity", position="dodge")
      
    }else if (x1==6){ ### 최근 1년 기획사 점유율
      t<- aggregate(as.numeric(chart$숫자)~(chart$기획사+chart$월),chart,sum)
      tt<- aggregate(as.numeric(chart$숫자)~(chart$기획사),chart,sum)
      tt<-cbind(tt,round((tt[,2]/nrow(chart)*100),1))
      
      tt<-tt[order(tt[,2],decreasing = T),]
      colnames(tt)<-c("기획사","월","건수")
      
      print(tt)
      
      ## 기획사, 월별 건수 중 상위 4개씩만 가져오
      colnames(t)<-c("기획사","월","건수")
      ttt<-data.frame(t[order(t$월,t$건수,decreasing = T),])
      mm<-unique(t[,2])
      tttt<-NULL
      for(i in 1:length(mm)){
        for (j in 1:4){
          tttt<-rbind(tttt,ttt[ttt$월==mm[i],][j,])
        }
      }
      
      colnames(tt)<-c("기획사","건수","비율")
      tt<-tt[order(tt$건수,decreasing = T),]
      
      bar<-ggplot(tttt, aes(x=월, y=건수, fill=기획사))
      bar+geom_bar(stat="identity", position="dodge")
    }
  }
  
  ex_m2<-function(n){
    
    add.months= function(date,a) seq(date, by = paste (a, "months"), length = 2)[2]
    
    chart<-data.frame()
    for (j in 1:n){
      ym<-add.months(Sys.Date(),-j)
      ym<-str_replace_all(ym,"[^\\d]","" )   # 숫자가 아닌 문자를 지운다.
      url = paste("http://www.genie.co.kr/chart/top200?ditc=M&rtm=N&ymd=",ym)
      url<-gsub(' ','',url)
      SOURCE = getURL(url)   # url 정보를 가져오고
      PARSED = htmlParse(SOURCE, encoding = "UTF-8")  
      print(url)
      
      for(i in 1:50){
        if(i==1&j==1){
          chart<-get_inf3(i,PARSED,substr(ym,1,6))
        }
        else
          chart<-rbind(chart,get_inf3(i,PARSED,substr(ym,1,6)))
        #%>% add_trace(y = ~뿜뿜, name = '뿜뿜', line = list(color = 'rgb(22, 16, 29)', width = 4))
      }
    }
    
    print(chart)
  
    
    chart[,3]<-substr(chart[,3],5,6)
    colnames(chart)<-c('숫자','장르','월','소속사','아티스트','제목')
    print(chart)
    
    chart<-data.frame(chart)
    
    t<- aggregate(as.numeric(chart$숫자)~(chart$장르+chart$월),chart,length)
    print("@@##@#@#")
    colnames(t)<-c('장르','월','건수')
    print(t)
    
    ttt<-data.frame(t[order(t$월,t$건수,decreasing = T),])
    mm<-unique(t[,2])
    tttt<-NULL
    for(i in 1:length(mm)){
      for (j in 1:6){
        tttt<-rbind(tttt,ttt[ttt$월==mm[i],][j,])
      }
    }
    
    
    tt<- aggregate(as.numeric(chart$숫자)~(chart$장르),chart,sum)
    tt<-cbind(tt,round((tt[,2]/nrow(chart)*100),1))
    
    colnames(tt)<-c("장르","건수","비율")
    tt<-tt[order(tt$건수,decreasing = T),]
    print(tt)
    
    colnames(t)<-c("장르","월","건수")
    
    #bar<-ggplot(tttt, aes(x=월, y=건수, fill=장르))
    bar<-ggplot(tttt, aes(x=월, y=건수, group = 장르, colour = 장르,size=0.7))
    bar+ geom_line()  +
      facet_wrap(~ 장르) +
      theme_bw()+ 
      geom_vline(xintercept=2, colour="grey", lty="dashed", size=1) +
      geom_vline(xintercept=5, colour="grey", lty="dashed", size=1) +
      geom_vline(xintercept=8, colour="grey", lty="dashed", size=1) + 
      geom_vline(xintercept=11, colour="grey", lty="dashed", size=1) 
    
   
  }
  
  ex_m3<-function(){
    
    months_between<-function(y){  ## 특정 날짜는 오늘로 부터 몇달 전인가?
      day = as.Date(y)
      
      (year(Sys.Date())-year(day))*12 + (month(Sys.Date())-month(day))
    }
    
    dd <- months_between('2012-02-01')
    
    add.months= function(date,a) seq(date, by = paste (a, "months"), length = 2)[2]
    
    chart<-data.frame()
    
    for (j in 1:dd){   ### dd로 수정할것
      ym<-add.months(Sys.Date(),-j)
      ym<-str_replace_all(ym,"[^\\d]","" )   # 숫자가 아닌 문자를 지운다.
      url = paste("http://www.genie.co.kr/chart/top200?ditc=M&rtm=N&ymd=",ym)
      url<-gsub(' ','',url)
      SOURCE = getURL(url)   # url 정보를 가져오고
      PARSED = htmlParse(SOURCE, encoding = "UTF-8")  
      print(url)
      
      for(i in 1:50){   # 1위 부터 4ㅇ
        if(i==1&j==1){
          chart<-get_inf3(i,PARSED,substr(ym,1,6))
        }
        else
          chart<-rbind(chart,get_inf3(i,PARSED,substr(ym,1,6)))
      }
    }
      
    print(chart)
    chart
    chart<-data.frame(chart)
    chart<-cbind(chart,(101-as.numeric(chart[,'X1'])))
    colnames(chart)<-c('순위','장르','월','기획사','아티스트','제목','점수')
    print(head(chart))
    print("bbbbbbbbbbb")
    
    if (x2==1){
    rnk<-aggregate(점수~아티스트,chart,sum)
    rnk<-rnk[order(rnk$점수,decreasing=T),]
    rnk<-data.frame(rnk[1:10,])
    print(class(rnk))
    print(rnk)
    
    ggplot(rnk, aes(아티스트, 점수, fill=아티스트))+geom_bar(stat="identity") + 
      geom_text(aes(y = 점수, label = 점수, size = 3 , vjust = 1.2 ,hjust=0.4),color='white')
    }
    else if (x2==2){
      rnk2<-aggregate(점수~제목,chart,sum)
      rnk2<-rnk2[order(rnk2$점수,decreasing=T),]
      rnk2<-data.frame(rnk2[1:10,])
      print(rnk2)
      
      ggplot(rnk2, aes(제목, 점수, fill=제목))+geom_bar(stat="identity") + 
        geom_text(aes(y = 점수, label = 점수, size = 3 , vjust = 1.2 ,hjust=0.4),color='white')
      
    }
    
    #rnk2<-aggregate(점수~제목,chart,sum)
    #print(rnk2)
    #head(rnk2[order(rnk2$점수,decreasing=T),],10)
  }
  
  switch(as.numeric(x1),top_100(3),top_100(4),old_song(),ex_m(p),sw_p(x2),sw_p(5),ex_m2(36),ex_m3())
}

genie_chart()
