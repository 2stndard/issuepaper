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

df |>
  gather(입학시도, 입학자수, starts_with('입학')) |>
  filter(연도 == 2018, 입학시도 != '입학합계') |>
  mutate(입학시도1 = fct_relevel(입학시도, '입학서울', '입학부산', '입학대구', '입학인천', '입학광주', '입학대전', '입학울산', '입학세종', '입학경기', '입학강원', '입학충북', '입학충남', '입학전북', '입학전남', '입학경북', '입학경남', '입학제주')) |>
  ggplot(aes(x = substr(입학시도1, 3, 10), y = 입학자수)) + 
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) + 
  theme_minimal()


+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0)


?fct_relabel  

glimpse(df)

View(count(df, 졸업연도))

df[df$졸업연도 == '00년전', 4] <- as.character(df[df$졸업연도 == '00년전', 1])

