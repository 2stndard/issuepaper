library(readxl)
library(dplyr)
library(tidyverse)
library(showtext)
showtext_auto()
library("RColorBrewer")
library(plotly)
library(withr)
library(processx)
library(networkD3)

getwd()

df.2018 <-  read_excel("./No1/복사본 신입생 출신 지역(2018-2020).xlsx", sheet = '2018', skip = 2, na = '-', col_types
                  = c('numeric', rep('text', 3), rep('numeric', 19)), col_names = F)

df.2019 <-  read_excel("./No1/복사본 신입생 출신 지역(2018-2020).xlsx", sheet = '2019', skip = 2, na = '-', col_types
                       = c('numeric', rep('text', 3), rep('numeric', 19)), col_names = F)

df.2020 <-  read_excel("./No1/복사본 신입생 출신 지역(2018-2020).xlsx", sheet = '2020', skip = 2, na = '-', col_types
                       = c('numeric', rep('text', 3), rep('numeric', 19)), col_names = F)

df <- rbind(df.2018, df.2019, df.2020)

colnames(df) <- c('연도', '구분', '대학시도', '졸업연도', '입학합계', '입학서울', '입학부산', '입학대구', '입학인천', '입학광주', '입학대전', '입학울산', '입학세종', '입학경기', '입학강원', '입학충북', '입학충남', '입학전북', '입학전남', '입학경북', '입학경남', '입학제주', '입학기타')

colnames(df) <- c('연도', '구분', '대학시도', '졸업연도', '합계', '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '기타')


df$대학시도 <- fct_relevel(df$대학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df <- df %>% mutate(grad.year = 
  case_when(
    .$졸업연도 == '00년전' ~ .$연도,
    .$졸업연도 == '01년전' ~ .$연도 - 1,
    .$졸업연도 == '02년전' ~ .$연도 - 2, 
    .$졸업연도 == '03년전' ~ .$연도 - 3, 
    .$졸업연도 == '04년전' ~ .$연도 - 4, 
    .$졸업연도 == '05년전' ~ .$연도 - 5, 
    .$졸업연도 == '06년전' ~ .$연도 - 6, 
    .$졸업연도 == '07년전' ~ .$연도 - 7, 
    .$졸업연도 == '08년전' ~ .$연도 - 8, 
    .$졸업연도 == '09년전' ~ .$연도 - 9, 
    .$졸업연도 == '10년전 이상' ~ .$연도 - 10, 
    .$졸업연도 == '계' ~ 0,
    .$졸업연도 == '확인불가능' ~ 99
  )
) %>%
  relocate(grad.year, .after = 졸업연도)

df <- df |>
  mutate(구분 = replace(구분, 구분 == '01 대학', '대학')) |>
  mutate(구분 = replace(구분, 구분 == '08 전문대학', '전문대학'))
  

# df |>
#   gather(입학시도, 입학자수, starts_with('입학')) |>
#   filter(연도 == 2018, 입학시도 != '입학합계') |>
#   mutate(입학시도1 = fct_recode(입학시도, '서울' = '입학서울', '부산' = '입학부산', '대구' = '입학대구', '인천' = '입학인천', '광주' = '입학광주','대전' = '입학대전','울산' = '입학울산','세종' = '입학세종', '경기' = '입학경기','강원' = '입학강원', '충북' = '입학충북', '충남' = '입학충남', '전북' = '입학전북', '전남' = '입학전남', '경북' = '입학경북', '경남' = '입학경남', '제주' = '입학제주')) |> View()


##  전체 시도별 입학생의 출신 고교 분포
df |>
  gather(입학시도, 입학자수, 7:24) |>
  filter(연도 == 2020, 입학시도 != '입학합계', 졸업연도 == '계') |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')) |>
  group_by(구분, 대학시도, 입학시도) |>
  mutate(pct = 입학자수/합계) |>
  ggplot(aes(x = 대학시도, y = pct, fill = 입학시도)) + 
  geom_bar(position="fill", stat="identity") + 
  #  geom_text(aes(label = scales::percent(pct)),
  #              position="stack",hjust=0.5,col="firebrick",size=3) +
  facet_wrap(~구분, nrow = 2) + 
  ##  coord_flip() +
  labs(title = '시도별 (전문)대학 입학생 출신 고교 지역 분포', 
       x = '대학 소재지', fill = '출신 고교 분포') + 
##  scale_fill_brewer(palette = "Set1") + 
  theme_minimal()


## Heat Map
## 학생수

year <- 2019

df |>
  gather(입학시도, 입학자수, 7:24) |>
  filter(연도 == year, 입학시도 != '입학합계', 졸업연도 == '계', 대학시도 != '전국') |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '기타')) |>
  group_by(구분, 대학시도, 입학시도) |>
  mutate(pct = 입학자수/합계) |>
  ggplot(aes(x = 대학시도, y = 입학시도, fill = pct)) + 
  geom_tile() +
  geom_text(aes(label = scales::number(입학자수, big.mark = ',', accuracy = 1))) +
  facet_wrap(~구분, nrow = 2, scales = 'free') + 
  scale_fill_gradient(low="white", high="darkblue") + 
  labs(title = paste0(year, '년도 ', '지역별 (전문)대학 신입생 고교 출신지역 분포'),
       y = '출신고교 소재지역', x = '대학소재지') 



## 대학시도 100% 기준
df |>
  gather(입학시도, 입학자수, 7:24) |>
  filter(연도 == year, 입학시도 != '입학합계', 졸업연도 == '계', 대학시도 != '전국') |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '기타')) |>
  group_by(구분, 대학시도, 입학시도) |>
  mutate(pct = 입학자수/합계) |>
  ggplot(aes(x = 대학시도, y = 입학시도, fill = pct)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(round(pct, 3), accuracy=0.1))) +
  facet_wrap(~구분, nrow = 2) + 
  scale_fill_gradient(low="white", high="darkblue") + 
  labs(title = paste0(year, '년도 ', '지역별 (전문)대학 신입생 고교 출신지역 분포 비율'),
       y = '출신고교 소재지역', x = '대학소재지') 


  
## 입학시도 100% 기준
df |>
  gather(입학시도, 입학자수, 6:24) |> 
  filter(연도 == 2020, 입학시도 != '합계', 졸업연도 == '계', 대학시도 != '전국') |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '기타')) |>
  group_by(연도, 구분, 입학시도) |>
  mutate(입학생합계 = sum(입학자수)) |> 
  mutate(pct = 입학자수/입학생합계) |>
  ggplot(aes(x = 입학시도, y = 대학시도, fill = pct)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(round(pct, 4), accuracy=0.01))) +
  facet_wrap(~구분, nrow = 2) + 
  scale_fill_gradient(low="white", high="darkblue") + 
  labs(title = paste0(year, '년도 ', '출신 고교 지역별 (전문)대학 입학지역 분포 비율'),
       x = '출신고교 소재지역', y = '대학소재지')



## Bar chart
## 소재 (전문)대학 신입생 고교 출신지역 분포

대학소재지 <- '대구'

df |>
  gather(입학시도, 입학자수, 7:24) |>
  filter(연도 == year, 입학시도 != '입학합계', 졸업연도 != '계', 대학시도 != '전국', 대학시도 == 대학소재지) |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '기타')) |>
  group_by(구분, 입학시도) |>
  summarise(입학자수 = sum(입학자수)) |>
  ggplot(aes(x = 입학시도, y = 입학자수)) + 
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) + 
  geom_text(aes(x = 입학시도, y = 입학자수, label = format(입학자수, big.mark = ',')), vjust = -0.5) + 
  facet_wrap(~구분, nrow = 2, scales = "free") +
  labs(title = paste0(year, '년도 ', 대학소재지, '소재 (전문)대학 신입생 고교 출신지역 분포'), 
       x = '출신교고 소재지역', 
       y = '입학자수') +
  theme_minimal()

########################################################
## sankey 학생수

기준시도 = '서울'
학교급 = '전문대학'
기준년도 = 2020

df |>
  gather(입학시도, 입학자수, 6:24) |> 
  filter(연도 == 기준년도, 입학시도 != '합계', 졸업연도 == '계', 대학시도 != '전국') |> 
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '기타')) |> 
  filter(대학시도 == 기준시도, 구분 == 학교급) |> 
  mutate(pct = (입학자수/sum(입학자수))*100) |>
  arrange(구분) -> from


df |>
  gather(입학시도, 입학자수, 6:24) |> 
  filter(연도 == 기준년도, 입학시도 != '합계', 졸업연도 == '계', 대학시도 != '전국') |> 
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '기타')) |> 
  filter(입학시도 == 기준시도, 구분 == 학교급) |> 
  mutate(pct = (입학자수/sum(입학자수))*100) |>
  arrange(구분) -> to

# paste0(from$입학시도, ' - ', round(from$pct, 1), '%')
# paste0(to$대학시도, ' - ', round(to$pct, 1), '%')


plot_ly(type = 'sankey', 
        name = 'test',
        orientation = 'h',
        domain = list(
          x =  c(0,100),
          y =  c(0,100)
        ),
        node = list(
#          label = c('서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '기타', 기준시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주'), 
          label = c(paste0(from$입학시도, ' - ', round(from$pct, 1), '%'),
                    기준시도,
                    paste0(to$대학시도, ' - ', round(to$pct, 1), '%')),
          color = c(rep('red', 18), 'black', rep('blue', 17)),
          pad = 5, 
          thickness = 30, 
          valueformat = ".0f",
          valuesuffix = "%",
          line = list(color = 'black', width = 0.5)
        ), 
        link = list(
          source = c(0:17, rep(18, 17)),
          target = c(rep(18, 18), 19:35),
##          label = c(rep('a', 35)),
          value = c(from$pct, to$pct)
        ), 
        textfont = list(size = 12)
) %>%
  layout(autosize = T, margin=list(b = 50, t = 50,  pad = 4)) %>% 
  layout(
  title = paste0(기준년도, '년 ' , 기준시도, "지역 ", 학교급, ' 신입생의 전입 및 전출 현황(%)'), 
  font = list(
    size = 15
  ),
  xaxis = list(showgrid = T, zeroline = T, showticklabels = T,
               showgrid = T),
  yaxis = list(showgrid = T, zeroline = T, showticklabels = T,
               showgrid = T)
) %>% add_annotations(
  x= 0.25,
  y= -0.05,
  xref = "paper",
  yref = "paper",
  text = paste0('각 지역 졸업생의 ', 기준시도, "지역 ", 학교급,' 전입'),
  xanchor = 'center',
  showarrow = F
) %>% add_annotations(
  x= 0.75,
  y= -0.05,
  xref = "paper",
  yref = "paper",
  text = paste0(기준시도, " 고교졸업생 중 ", 학교급, '지역별 현황'),
  xanchor = 'center',
  showarrow = F
)



library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p
?sankeyNetwork


plot_ly() %>%
  config(
    toImageButtonOptions = list(
      format = "eps",
      filename = "myplot",
      width = 600,
      height = 700
    )
  )
orca()

install.packages('rsvg')
library(rsvg)
rsvg_pdf('myplot.svg', 'myplot.pdf')
getwd()

Sys.setenv(PATH = paste0('C:/Users/estnd/AppData/Local/Programs/orca/;', Sys.getenv("PATH")))


links <- data.frame(
  source = c(0:17, rep(18, 17)),
  target = c(rep(18, 18), 19:35),
  value = c(from$pct, to$pct)
)

nodes <- data.frame(
  name = c(paste0(from$입학시도, ' - ', round(from$pct, 1), '%'),
           기준시도,
           paste0(to$대학시도, ' - ', round(to$pct, 1), '%'))
)

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, fontSize = 12, nodeWidth = 20)
