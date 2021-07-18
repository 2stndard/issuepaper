library(readxl)
library(dplyr)
library(tidyverse)

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



# df |>
#   gather(입학시도, 입학자수, starts_with('입학')) |>
#   filter(연도 == 2018, 입학시도 != '입학합계') |>
#   mutate(입학시도1 = fct_recode(입학시도, '서울' = '입학서울', '부산' = '입학부산', '대구' = '입학대구', '인천' = '입학인천', '광주' = '입학광주','대전' = '입학대전','울산' = '입학울산','세종' = '입학세종', '경기' = '입학경기','강원' = '입학강원', '충북' = '입학충북', '충남' = '입학충남', '전북' = '입학전북', '전남' = '입학전남', '경북' = '입학경북', '경남' = '입학경남', '제주' = '입학제주')) |> View()

df |>
  gather(입학시도, 입학자수, 7:24) |>
  filter(연도 == 2020, 입학시도 != '입학합계', 졸업연도 != '계', 대학시도 != '전국', 대학시도 == '서울') |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')) |>
  group_by(구분, 입학시도) |>
  summarise(입학자수 = sum(입학자수)) |>
  ggplot(aes(x = 입학시도, y = 입학자수)) + 
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) + 
  geom_text(aes(x = 입학시도, y = 입학자수, label = 입학자수), vjust = -0.5) + 
  facet_wrap(~구분, nrow = 2) +
  theme_minimal()

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
  theme_minimal()


## 학생수
df |>
  gather(입학시도, 입학자수, 7:24) |>
  filter(연도 == 2020, 입학시도 != '입학합계', 졸업연도 == '계', 대학시도 != '전국') |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')) |>
  group_by(구분, 대학시도, 입학시도) |>
  mutate(pct = 입학자수/합계) |>
  ggplot(aes(x = 대학시도, y = 입학시도, fill = pct)) + 
  geom_tile() +
  geom_text(aes(label = scales::number(입학자수, big.mark = ',', accuracy = 1))) +
  facet_wrap(~구분, nrow = 2) + 
  scale_fill_gradient(low="white", high="darkblue") + 
  labs(y = '고교졸업생시도', x = '대학소재지시도')



## 대학시도 100% 기준
df |>
  gather(입학시도, 입학자수, 7:24) |>
  filter(연도 == 2020, 입학시도 != '입학합계', 졸업연도 == '계', 대학시도 != '전국') |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')) |>
  group_by(구분, 대학시도, 입학시도) |>
  mutate(pct = 입학자수/합계) |>
  ggplot(aes(x = 대학시도, y = 입학시도, fill = pct)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(round(pct, 3), accuracy=0.1))) +
  facet_wrap(~구분, nrow = 2) + 
  scale_fill_gradient(low="white", high="darkblue") + 
  labs(y = '고교졸업생시도', x = '대학소재지시도')


  
## 입학시도 100% 기준
df |>
  gather(입학시도, 입학자수, 6:24) |> 
  filter(연도 == 2020, 입학시도 != '합계', 졸업연도 == '계', 대학시도 != '전국') |>
  mutate(입학시도 = fct_relevel(입학시도, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')) |>
  group_by(연도, 구분, 입학시도) |>
  mutate(입학생합계 = sum(입학자수)) |> 
  mutate(pct = 입학자수/입학생합계) |>
  ggplot(aes(x = 입학시도, y = 대학시도, fill = pct)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(round(pct, 4), accuracy=0.01))) +
  facet_wrap(~구분, nrow = 2) + 
  scale_fill_gradient(low="white", high="darkblue") + 
  labs(x = '고교졸업생시도', y = '대학소재지시도')

