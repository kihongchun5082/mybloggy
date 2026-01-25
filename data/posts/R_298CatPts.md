# R을 사용한 건강보험통계연보 분석(1)

## 298분류별 주요 질병별 환자 수 추이

* 2024년 2월 15일에 게재한 포스트 "만성질환이 국민건강에 왜 중요한가요?" 에 참조된 '우리나라 주요 만성질환자 수 추이(2010-2022년)' 그래프의 자료는 ⎡건강보험통계연보 제6편 질병통계⎦에 있는 <표 𝚅𝙸-2>(190페이지 분량)에 의해 산출하였습니다. 

![우리나라주요만성질환자수추이](</images/posts/우리나라주요만성질환자수추이.png>)

* 국민건강보험공단이나 심사평가원 홈페이지에서 건강보험통계연보를 다운받으면 “06-2(건강공단) 2022년_제6편 질병통계.xlsx”가 있습니다. 이 엑셀파일을 'tsv파일로 내보내기'하면서 '여러 표를 하나의 파일에 통합'을 선택합니다.

* 최종 산출한 테이블은 다음과 같습니다. R을 사용해 다운로드한 연보 엑셀 자료를 읽어와서 작업한 결과입니다.

![우리나라건강보험통계연보근거한298분류에따른주요만성질환의연도별환자수](</images/posts/우리나라298분류에따른연도별환자수.png>)

* 연보 연도에 따라 표 제목과 형태가 약간씩 차이가 있을 수 있을 것입니다. 따라서 각 연도별로 자료를 식별하였습니다. 가장 최근인 2022년도 연보를 읽고 자료를 산출한 R 프로그램 코드는 다음과 같습니다. 필요한 설명을 추가하였습니다.

```R
install.packages("dplyr")
library(dplyr)
install.packages("data.table")
install.packages("curl")
library(data.table)
library(curl)
install.packages("writexl")
library(writexl)
getwd()
# 엑셀시트전체를 모두 합쳐 tsv 파일로 변환한 파일을 읽어 R table로 만듦 
D22_NHIYB_T06_2 <- fread(file="NHI289_06_2_2022t.tsv")
# 읽은 파일의 구조와 변수명을 확인함
str(D22_NHIYB_T06_2)
# data.table에 고유 행번호를 부여하기 위해 일련번호를 만들어 붙임
serNum<-data.frame(x1=1:8119)
D221<-cbind(serNum, D22_NHIYB_T06_2)
# 각 행의 설명정보인 x1(일련번호), V1(질환그룹 고유번호), V2(계・입・퇴원 구분) 변수를 선택함 
DF_sel<-D221[,c("x1","V1","V2")]
D22_sel<-as.data.table(DF_sel)
# 연보에서는 질환그룹별 계,입원,외래가 있는데 질환그룹은 V1변수에 맨 위 셀에만 있기 때문에 모든 행에 이 정보를 가지고 가기 위해 shift를 사용해 lag된 변수 생성함    
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D22_sel1<-D22_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D22_sel1$V1, D22_sel1$lag_ref1, D22_sel1$lag_ref2)
# lag된 변수 3개 중 최대값 즉 맨 위 셀과 같은 내용으로 변수 만듦
V1_shift1<-apply(V1_shift, 1, max, na.rm = TRUE)
# 일련번호를 붙이고 원래 자료와 머지함
V1_shift2 <- cbind(serNum, V1_shift1)
D22_V1<-as.data.table(merge(V1_shift2, D221, by="x1", all=TRUE))
# 연보의 여러 페이지를 한 파일로 붙였으므로 페이지 처음에 있는 변수명과 행번호를 확인함
show(D22_V1[1,"V3"])
show(D22_V1[97,"V3"])
show(D22_V1[192,"V3"])
show(D22_V1[287,"V3"])
show(D22_V1[382,"V3"])
show(D22_V1[477,"V3"])
# 연보에서는 3개 연령군을 한페이지에 병렬로 열을 구성하였으므로 이걸 행으로 전환해야함 
# 3개 연령군이 표시된 변수를 복제하고 새 변수명 부여함
D22_V1[,`:=` (V3_1 = V3)]
D22_V1[, `:=` (V8_1=V8)]
D22_V1[, `:=` (V13_1=V13)]
# 한글로 되어있는 셀을 영문으로 바꾸어 변수명으로 사용할 준비를 함
D22_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D22_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D22_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D22_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D22_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D22_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D22_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D22_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D22_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D22_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D22_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D22_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D22_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D22_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D22_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D22_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D22_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D22_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D22_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D22_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
# 한글로 되어있는 셀을 숫자와 영문으로 바꾸고, 행・열 피봇후 행을 식별할 수 있도록 해당변수 빈셀에 같은 내용으로 채움
D22_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D22_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D22_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D22_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D22_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D22_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]

D22_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D22_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D22_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D22_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D22_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D22_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]

D22_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D22_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D22_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D22_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D22_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D22_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]

D22_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D22_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
# 파일전체를 대상으로 7페이지마다 나오는 같은 내용의 맨위셀이 반복되어 나오는 지를 확인함
D22_V1[V3=="총                계\nG.T",]
D22_V1[V3=="5    ~    9세",]
D22_V1[V3=="20    ~    24세",]
D22_V1[V3=="35    ~    39세",]
D22_V1[V3=="50    ~    54세",]
D22_V1[V3=="65    ~    69세",]
D22_V1[V13=="75세이상",]
# 질환분류1-20에 해당하는 첫 7페이지에 페이지번호를 붙임
D22_V1[x1>=1 & x1<97,`:=` (pageX=1)]
D22_V1[x1>=97 & x1<192,`:=` (pageX=2)]
D22_V1[x1>=192 & x1<287,`:=` (pageX=3)]
D22_V1[x1>=287 & x1<382,`:=` (pageX=4)]
D22_V1[x1>=382 & x1<477,`:=` (pageX=5)]
D22_V1[x1>=477 & x1<572,`:=` (pageX=6)]
# 파일 전체를 대상으로 페이지 번호를 반복루틴을 통해 붙임
xx<-572
i<-7
repeat {
  D22_V1[x1>=xx & x1<xx+90,`:=` (pageX=i)]
  xx<-xx+90
  i<-i+1
  if (xx >= 8119) { break }
}

install.packages("tidyverse")
# 불필요한 변수를 삭제함
D22_V1[, V1 := NULL]
D22_V1[, V2 := NULL]
D22_V1[, V3 := NULL]
D22_V1[, V8 := NULL]
D22_V1[, V13 := NULL]
head(D22_V1)
# 연보에 줄바꿈한 행은 빈셀이므로 삭제함
D22_V1_0 <- D22_V1[D22_V1$V3_1 != ""]
# 변수명으로 사용할 행이 중간에 있으므로 이 행을 삭제하고 셀 내용을 변수명으로 가지고 옴
D22_V1_01 <- D22_V1_0[D22_V1_0$V4 != "visits"]
# 한 페이지에 있는 세개의 연령그룹을 분리한 후 각 연령그룹을 페이지로 분리하고  페이지별로 연령그룹을 새로운 변수를 만들어 자료 생성함
# 페이지당 첫번째 연령그룹
D22_V1_1 <- D22_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
# 페이지별로 나눔(data list 형성됨)
D22_V1_1_split <- split(D22_V1_1, D22_V1_1$pageX)
# 90개의 페이지로 나눈 후 각 페이지는 동일 연령그룹이므로 변수를 만들어 연령그룹을 입력함  
eleNum = c(1:90)
for (i in eleNum) {
  D22_V1_1_split[[i]][, `:=` (ageCtg = as.character(D22_V1_1_split[[i]][1,4]))]
}
# 나눈 페이지를 행으로 붙임(append)
D22_V1_1_A = do.call(rbind, D22_V1_1_split)
# 연령그룹이 있는 행을 삭제함
D22_V1_1_A_1 <- D22_V1_1_A[D22_V1_1_A$disGrp != "C"]
# 페이지당 두번째 연령그룸
D22_V1_2 <- D22_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D22_V1_2_split <- split(D22_V1_2, D22_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D22_V1_2_split[[i]][, `:=` (ageCtg = as.character(D22_V1_2_split[[i]][1,4]))]
}
D22_V1_2_A = do.call(rbind, D22_V1_2_split)
D22_V1_2_A_1 <- D22_V1_2_A[D22_V1_2_A$disGrp != "C"]
# 페이지당 세번째 연령그룹
D22_V1_3 <- D22_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D22_V1_3_split <- split(D22_V1_3, D22_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D22_V1_3_split[[i]][, `:=` (ageCtg = as.character(D22_V1_3_split[[i]][1,4]))]
}
D22_V1_3_A = do.call(rbind, D22_V1_3_split)
D22_V1_3_A_1 <- D22_V1_3_A[D22_V1_3_A$disGrp != "C"]
# 페이지당 연령그룹을 분리한 3개의 Table을 모두 붙임(append)
D22_V1_ALL <- rbind(D22_V1_1_A_1, D22_V1_2_A_1, D22_V1_3_A_1)
# 자료에서 숫자의 자리수 컴머를 없애서 숫자형으로 자료를 읽고자 함
D22_V1_ALL_1 <- as.data.frame(apply(D22_V1_ALL, 2, function(x) gsub(",", "", x)))
# 결과로 산출할 자료가 각 질병그룹별 환자 수이므로 계 만 가지고 옴
D22_V1_ALL_GT_ST <- D22_V1_ALL_1[D22_V1_ALL_1$ToInOu == "ST" & D22_V1_ALL_1$ageCtg == "G.T",]
# 필요한 변수를 숫자형으로 바꿈
D22_V1_ALL_GT_ST1<-mutate(D22_V1_ALL_GT_ST, YR2022_nofPts=as.numeric(D22_V1_ALL_GT_ST$patients),  YR2022_exp=as.numeric(D22_V1_ALL_GT_ST$expense))
# 필요한 변수를 선택함
D22_V1_ALL_GT_ST_pts<-select(D22_V1_ALL_GT_ST1, disGrp, YR2022_nofPts, YR2022_exp)
# 필요한 질병그룹을 선택함
ALL_pts_seleDis_2022 <- D22_V1_ALL_GT_ST_pts[D22_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2022)
str(ALL_pts_seleDis_2022)
```

* 2010년부터 2021년까지 각 연도의 연보를 읽고 2010년-2022년의 298분류에 따른 특정 질환군의 환자 수를 산출한 R 프로그램입니다.

```R
# 2021

D21_NHIYB_T06_21 <- fread(input="NHI289_06_2_2021.tsv")
D21_NHIYB_T06_2 <- select(D21_NHIYB_T06_21, V1:V17)
str(D21_NHIYB_T06_2)
serNum<-data.frame(x1=1:8119)
D211<-cbind(serNum, D21_NHIYB_T06_2)
DF_sel<-D211[,c("x1","V1","V2")]
D21_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D21_sel1<-D21_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D21_sel1$V1, D21_sel1$lag_ref1, D21_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D21_V1<-as.data.table(merge(V1_shift2, D211, by="x1", all=TRUE))
D21_V1[V1=="구    분
Division",c(x1)]
D21_V1[,`:=` (V3_1 = V3)]
D21_V1[, `:=` (V8_1=V8)]
D21_V1[, `:=` (V13_1=V13)]
D21_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D21_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D21_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D21_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D21_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D21_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D21_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D21_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D21_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D21_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D21_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D21_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D21_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D21_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D21_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D21_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D21_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D21_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D21_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D21_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D21_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D21_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D21_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D21_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D21_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D21_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D21_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D21_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D21_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D21_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D21_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D21_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D21_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D21_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D21_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D21_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D21_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D21_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D21_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D21_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D21_V1[V3=="총                계\nG.T",]
D21_V1[V3=="5    ~    9세",]
D21_V1[V3=="20    ~    24세",]
D21_V1[V3=="35    ~    39세",]
D21_V1[V3=="50    ~    54세",]
D21_V1[V3=="65    ~    69세",]
D21_V1[V13=="75세이상",]
D21_V1[x1>=1 & x1<96,`:=` (pageX=1)]
D21_V1[x1>=96 & x1<190,`:=` (pageX=2)]
D21_V1[x1>=190 & x1<284,`:=` (pageX=3)]
D21_V1[x1>=284 & x1<378,`:=` (pageX=4)]
D21_V1[x1>=378 & x1<472,`:=` (pageX=5)]
D21_V1[x1>=472 & x1<566,`:=` (pageX=6)]
xx<-566
i<-7
repeat {
  D21_V1[x1>=xx & x1<xx+90,`:=` (pageX=i)]
  xx<-xx+90
  i<-i+1
  if (xx >= 8119) { break }
}
D21_V1[, V1 := NULL]
D21_V1[, V2 := NULL]
D21_V1[, V3 := NULL]
D21_V1[, V8 := NULL]
D21_V1[, V13 := NULL]
head(D21_V1)
D21_V1_0 <- D21_V1[D21_V1$V3 != ""]
D21_V1_01 <- D21_V1_0[D21_V1_0$V4 != "visits"]
D21_V1_1 <- D21_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D21_V1_1_split <- split(D21_V1_1, D21_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D21_V1_1_split[[i]][, `:=` (ageCtg = as.character(D21_V1_1_split[[i]][1,4]))]
}
D21_V1_1_A = do.call(rbind, D21_V1_1_split)
D21_V1_1_A_1 <- D21_V1_1_A[D21_V1_1_A$disGrp != "C"]
D21_V1_2 <- D21_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D21_V1_2_split <- split(D21_V1_2, D21_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D21_V1_2_split[[i]][, `:=` (ageCtg = as.character(D21_V1_2_split[[i]][1,4]))]
}
D21_V1_2_A = do.call(rbind, D21_V1_2_split)
D21_V1_2_A_1 <- D21_V1_2_A[D21_V1_2_A$disGrp != "C"]
D21_V1_3 <- D21_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D21_V1_3_split <- split(D21_V1_3, D21_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D21_V1_3_split[[i]][, `:=` (ageCtg = as.character(D21_V1_3_split[[i]][1,4]))]
}
D21_V1_3_A = do.call(rbind, D21_V1_3_split)
D21_V1_3_A_1 <- D21_V1_3_A[D21_V1_3_A$disGrp != "C"]
D21_V1_ALL <- rbind(D21_V1_1_A_1, D21_V1_2_A_1, D21_V1_3_A_1)
D21_V1_ALL_1 <- as.data.frame(apply(D21_V1_ALL, 2, function(x) gsub(",", "", x)))
D21_V1_ALL_GT_ST <- D21_V1_ALL_1[D21_V1_ALL_1$ToInOu == "ST" & D21_V1_ALL_1$ageCtg == "G.T",]
D21_V1_ALL_GT_ST1<-mutate(D21_V1_ALL_GT_ST, YR2021_nofPts=as.numeric(D21_V1_ALL_GT_ST$patients),  YR2021_exp=as.numeric(D21_V1_ALL_GT_ST$expense))
D21_V1_ALL_GT_ST_pts<-select(D21_V1_ALL_GT_ST1, disGrp, YR2021_nofPts, YR2021_exp)
ALL_pts_seleDis_2021 <- D21_V1_ALL_GT_ST_pts[D21_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2021)
str(ALL_pts_seleDis_2021)


# 2020

D20_NHIYB_T06_21 <- fread(input="NHI289_06_2_2020.tsv")
D20_NHIYB_T06_2 <- select(D20_NHIYB_T06_21, V1:V17)
str(D20_NHIYB_T06_2)
serNum<-data.frame(x1=1:8119)
D201<-cbind(serNum, D20_NHIYB_T06_2)
DF_sel<-D201[,c("x1","V1","V2")]
D20_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D20_sel1<-D20_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D20_sel1$V1, D20_sel1$lag_ref1, D20_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D20_V1<-as.data.table(merge(V1_shift2, D201, by="x1", all=TRUE))
D20_V1[V1=="구    분
Division",c(x1)]
D20_V1[,`:=` (V3_1 = V3)]
D20_V1[, `:=` (V8_1=V8)]
D20_V1[, `:=` (V13_1=V13)]
D20_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D20_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D20_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D20_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D20_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D20_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D20_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D20_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D20_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D20_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D20_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D20_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D20_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D20_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D20_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D20_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D20_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D20_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D20_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D20_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D20_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D20_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D20_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D20_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D20_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D20_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D20_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D20_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D20_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D20_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D20_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D20_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D20_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D20_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D20_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D20_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D20_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D20_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D20_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D20_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D20_V1[V3=="총                계\nG.T",]
D20_V1[V3=="5    ~    9세",]
D20_V1[V3=="20    ~    24세",]
D20_V1[V3=="35    ~    39세",]
D20_V1[V3=="50    ~    54세",]
D20_V1[V3=="65    ~    69세",]
D20_V1[V13=="75세이상",]
D20_V1[x1>=1 & x1<96,`:=` (pageX=1)]
D20_V1[x1>=96 & x1<190,`:=` (pageX=2)]
D20_V1[x1>=190 & x1<284,`:=` (pageX=3)]
D20_V1[x1>=284 & x1<378,`:=` (pageX=4)]
D20_V1[x1>=378 & x1<472,`:=` (pageX=5)]
D20_V1[x1>=472 & x1<566,`:=` (pageX=6)]
xx<-566
i<-7
repeat {
  D20_V1[x1>=xx & x1<xx+90,`:=` (pageX=i)]
  xx<-xx+90
  i<-i+1
  if (xx >= 8119) { break }
}
D20_V1[, V1 := NULL]
D20_V1[, V2 := NULL]
D20_V1[, V3 := NULL]
D20_V1[, V8 := NULL]
D20_V1[, V13 := NULL]
head(D20_V1)
D20_V1_0 <- D20_V1[D20_V1$V3 != ""]
D20_V1_01 <- D20_V1_0[D20_V1_0$V4 != "visits"]
D20_V1_1 <- D20_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D20_V1_1_split <- split(D20_V1_1, D20_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D20_V1_1_split[[i]][, `:=` (ageCtg = as.character(D20_V1_1_split[[i]][1,4]))]
}
D20_V1_1_A = do.call(rbind, D20_V1_1_split)
D20_V1_1_A_1 <- D20_V1_1_A[D20_V1_1_A$disGrp != "C"]
D20_V1_2 <- D20_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D20_V1_2_split <- split(D20_V1_2, D20_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D20_V1_2_split[[i]][, `:=` (ageCtg = as.character(D20_V1_2_split[[i]][1,4]))]
}
D20_V1_2_A = do.call(rbind, D20_V1_2_split)
D20_V1_2_A_1 <- D20_V1_2_A[D20_V1_2_A$disGrp != "C"]
D20_V1_3 <- D20_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D20_V1_3_split <- split(D20_V1_3, D20_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D20_V1_3_split[[i]][, `:=` (ageCtg = as.character(D20_V1_3_split[[i]][1,4]))]
}
D20_V1_3_A = do.call(rbind, D20_V1_3_split)
D20_V1_3_A_1 <- D20_V1_3_A[D20_V1_3_A$disGrp != "C"]
D20_V1_ALL <- rbind(D20_V1_1_A_1, D20_V1_2_A_1, D20_V1_3_A_1)
D20_V1_ALL_1 <- as.data.frame(apply(D20_V1_ALL, 2, function(x) gsub(",", "", x)))
D20_V1_ALL_GT_ST <- D20_V1_ALL_1[D20_V1_ALL_1$ToInOu == "ST" & D20_V1_ALL_1$ageCtg == "G.T",]
D20_V1_ALL_GT_ST1<-mutate(D20_V1_ALL_GT_ST, YR2020_nofPts=as.numeric(D20_V1_ALL_GT_ST$patients),  YR2020_exp=as.numeric(D20_V1_ALL_GT_ST$expense))
D20_V1_ALL_GT_ST_pts<-select(D20_V1_ALL_GT_ST1, disGrp, YR2020_nofPts, YR2020_exp)
ALL_pts_seleDis_2020 <- D20_V1_ALL_GT_ST_pts[D20_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2020)
str(ALL_pts_seleDis_2020)


# 2019

D19_NHIYB_T06_21 <- fread(input="NHI289_06_2_2019.tsv")
D19_NHIYB_T06_2 <- select(D19_NHIYB_T06_21, V1:V17)
str(D19_NHIYB_T06_2)
serNum<-data.frame(x1=1:8119)
D191<-cbind(serNum, D19_NHIYB_T06_2)
DF_sel<-D191[,c("x1","V1","V2")]
D19_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D19_sel1<-D19_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D19_sel1$V1, D19_sel1$lag_ref1, D19_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D19_V1<-as.data.table(merge(V1_shift2, D191, by="x1", all=TRUE))
D19_V1[V1=="구    분
Division",c(x1)]
D19_V1[,`:=` (V3_1 = V3)]
D19_V1[, `:=` (V8_1=V8)]
D19_V1[, `:=` (V13_1=V13)]
D19_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D19_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D19_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D19_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D19_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D19_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D19_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D19_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D19_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D19_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D19_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D19_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D19_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D19_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D19_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D19_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D19_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D19_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D19_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D19_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D19_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D19_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D19_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D19_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D19_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D19_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D19_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D19_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D19_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D19_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D19_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D19_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D19_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D19_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D19_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D19_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D19_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D19_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D19_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D19_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D19_V1[V3=="총                계\nG.T",]
D19_V1[V3=="5    ~    9세",]
D19_V1[V3=="20    ~    24세",]
D19_V1[V3=="35    ~    39세",]
D19_V1[V3=="50    ~    54세",]
D19_V1[V3=="65    ~    69세",]
D19_V1[V13=="75세이상",]
D19_V1[x1>=1 & x1<96,`:=` (pageX=1)]
D19_V1[x1>=96 & x1<190,`:=` (pageX=2)]
D19_V1[x1>=190 & x1<284,`:=` (pageX=3)]
D19_V1[x1>=284 & x1<378,`:=` (pageX=4)]
D19_V1[x1>=378 & x1<472,`:=` (pageX=5)]
D19_V1[x1>=472 & x1<566,`:=` (pageX=6)]
xx<-566
i<-7
repeat {
  D19_V1[x1>=xx & x1<xx+90,`:=` (pageX=i)]
  xx<-xx+90
  i<-i+1
  if (xx >= 8119) { break }
}
D19_V1[, V1 := NULL]
D19_V1[, V2 := NULL]
D19_V1[, V3 := NULL]
D19_V1[, V8 := NULL]
D19_V1[, V13 := NULL]
head(D19_V1)
D19_V1_0 <- D19_V1[D19_V1$V3 != ""]
D19_V1_01 <- D19_V1_0[D19_V1_0$V4 != "visits"]
D19_V1_1 <- D19_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D19_V1_1_split <- split(D19_V1_1, D19_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D19_V1_1_split[[i]][, `:=` (ageCtg = as.character(D19_V1_1_split[[i]][1,4]))]
}
D19_V1_1_A = do.call(rbind, D19_V1_1_split)
D19_V1_1_A_1 <- D19_V1_1_A[D19_V1_1_A$disGrp != "C"]
D19_V1_2 <- D19_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D19_V1_2_split <- split(D19_V1_2, D19_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D19_V1_2_split[[i]][, `:=` (ageCtg = as.character(D19_V1_2_split[[i]][1,4]))]
}
D19_V1_2_A = do.call(rbind, D19_V1_2_split)
D19_V1_2_A_1 <- D19_V1_2_A[D19_V1_2_A$disGrp != "C"]
D19_V1_3 <- D19_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D19_V1_3_split <- split(D19_V1_3, D19_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D19_V1_3_split[[i]][, `:=` (ageCtg = as.character(D19_V1_3_split[[i]][1,4]))]
}
D19_V1_3_A = do.call(rbind, D19_V1_3_split)
D19_V1_3_A_1 <- D19_V1_3_A[D19_V1_3_A$disGrp != "C"]
D19_V1_ALL <- rbind(D19_V1_1_A_1, D19_V1_2_A_1, D19_V1_3_A_1)
D19_V1_ALL_1 <- as.data.frame(apply(D19_V1_ALL, 2, function(x) gsub(",", "", x)))
D19_V1_ALL_GT_ST <- D19_V1_ALL_1[D19_V1_ALL_1$ToInOu == "ST" & D19_V1_ALL_1$ageCtg == "G.T",]
D19_V1_ALL_GT_ST1<-mutate(D19_V1_ALL_GT_ST, YR2019_nofPts=as.numeric(D19_V1_ALL_GT_ST$patients),  YR2019_exp=as.numeric(D19_V1_ALL_GT_ST$expense))
D19_V1_ALL_GT_ST_pts<-select(D19_V1_ALL_GT_ST1, disGrp, YR2019_nofPts, YR2019_exp)
ALL_pts_seleDis_2019 <- D19_V1_ALL_GT_ST_pts[D19_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2019)
str(ALL_pts_seleDis_2019)


# 2018

D18_NHIYB_T06_21 <- fread(input="NHI289_06_2_2018.tsv")
D18_NHIYB_T06_2 <- select(D18_NHIYB_T06_21, V1:V17)
str(D18_NHIYB_T06_2)
serNum<-data.frame(x1=1:8119)
D181<-cbind(serNum, D18_NHIYB_T06_2)
DF_sel<-D181[,c("x1","V1","V2")]
D18_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D18_sel1<-D18_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D18_sel1$V1, D18_sel1$lag_ref1, D18_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D18_V1<-as.data.table(merge(V1_shift2, D181, by="x1", all=TRUE))
D18_V1[V1=="구    분
Division",c(x1)]
D18_V1[,`:=` (V3_1 = V3)]
D18_V1[, `:=` (V8_1=V8)]
D18_V1[, `:=` (V13_1=V13)]
D18_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D18_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D18_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D18_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D18_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D18_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D18_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D18_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D18_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D18_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D18_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D18_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D18_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D18_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D18_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D18_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D18_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D18_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D18_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D18_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D18_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D18_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D18_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D18_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D18_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D18_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D18_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D18_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D18_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D18_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D18_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D18_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D18_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D18_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D18_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D18_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D18_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D18_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D18_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D18_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D18_V1[V3=="총                계\nG.T",]
D18_V1[V3=="5    ~    9세",]
D18_V1[V3=="20    ~    24세",]
D18_V1[V3=="35    ~    39세",]
D18_V1[V3=="50    ~    54세",]
D18_V1[V3=="65    ~    69세",]
D18_V1[V13=="75세이상",]
D18_V1[x1>=1 & x1<96,`:=` (pageX=1)]
D18_V1[x1>=96 & x1<190,`:=` (pageX=2)]
D18_V1[x1>=190 & x1<284,`:=` (pageX=3)]
D18_V1[x1>=284 & x1<378,`:=` (pageX=4)]
D18_V1[x1>=378 & x1<472,`:=` (pageX=5)]
D18_V1[x1>=472 & x1<566,`:=` (pageX=6)]
xx<-566
i<-7
repeat {
  D18_V1[x1>=xx & x1<xx+90,`:=` (pageX=i)]
  xx<-xx+90
  i<-i+1
  if (xx >= 8119) { break }
}
D18_V1[, V1 := NULL]
D18_V1[, V2 := NULL]
D18_V1[, V3 := NULL]
D18_V1[, V8 := NULL]
D18_V1[, V13 := NULL]
head(D18_V1)
D18_V1_0 <- D18_V1[D18_V1$V3 != ""]
D18_V1_01 <- D18_V1_0[D18_V1_0$V4 != "visits"]
D18_V1_1 <- D18_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D18_V1_1_split <- split(D18_V1_1, D18_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D18_V1_1_split[[i]][, `:=` (ageCtg = as.character(D18_V1_1_split[[i]][1,4]))]
}
D18_V1_1_A = do.call(rbind, D18_V1_1_split)
D18_V1_1_A_1 <- D18_V1_1_A[D18_V1_1_A$disGrp != "C"]
D18_V1_2 <- D18_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D18_V1_2_split <- split(D18_V1_2, D18_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D18_V1_2_split[[i]][, `:=` (ageCtg = as.character(D18_V1_2_split[[i]][1,4]))]
}
D18_V1_2_A = do.call(rbind, D18_V1_2_split)
D18_V1_2_A_1 <- D18_V1_2_A[D18_V1_2_A$disGrp != "C"]
D18_V1_3 <- D18_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D18_V1_3_split <- split(D18_V1_3, D18_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D18_V1_3_split[[i]][, `:=` (ageCtg = as.character(D18_V1_3_split[[i]][1,4]))]
}
D18_V1_3_A = do.call(rbind, D18_V1_3_split)
D18_V1_3_A_1 <- D18_V1_3_A[D18_V1_3_A$disGrp != "C"]
D18_V1_ALL <- rbind(D18_V1_1_A_1, D18_V1_2_A_1, D18_V1_3_A_1)
D18_V1_ALL_1 <- as.data.frame(apply(D18_V1_ALL, 2, function(x) gsub(",", "", x)))
D18_V1_ALL_GT_ST <- D18_V1_ALL_1[D18_V1_ALL_1$ToInOu == "ST" & D18_V1_ALL_1$ageCtg == "G.T",]
D18_V1_ALL_GT_ST1<-mutate(D18_V1_ALL_GT_ST, YR2018_nofPts=as.numeric(D18_V1_ALL_GT_ST$patients),  YR2018_exp=as.numeric(D18_V1_ALL_GT_ST$expense))
D18_V1_ALL_GT_ST_pts<-select(D18_V1_ALL_GT_ST1, disGrp, YR2018_nofPts, YR2018_exp)
ALL_pts_seleDis_2018 <- D18_V1_ALL_GT_ST_pts[D18_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2018)
str(ALL_pts_seleDis_2018)


# 2017

D17_NHIYB_T06_21 <- fread(input="NHI289_06_2_2017.tsv")
D17_NHIYB_T06_2 <- select(D17_NHIYB_T06_21, V1:V17)
str(D17_NHIYB_T06_2)
serNum<-data.frame(x1=1:8118)
D171<-cbind(serNum, D17_NHIYB_T06_2)
DF_sel<-D171[,c("x1","V1","V2")]
D17_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D17_sel1<-D17_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D17_sel1$V1, D17_sel1$lag_ref1, D17_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D17_V1<-as.data.table(merge(V1_shift2, D171, by="x1", all=TRUE))
D17_V1[V1=="구    분
Division",c(x1)]
D17_V1[,`:=` (V3_1 = V3)]
D17_V1[, `:=` (V8_1=V8)]
D17_V1[, `:=` (V13_1=V13)]
D17_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D17_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D17_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D17_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D17_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D17_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D17_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D17_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D17_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D17_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D17_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D17_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D17_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D17_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D17_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D17_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D17_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D17_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D17_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D17_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D17_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D17_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D17_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D17_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D17_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D17_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D17_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D17_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D17_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D17_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D17_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D17_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D17_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D17_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D17_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D17_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D17_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D17_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D17_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D17_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D17_V1[V3=="총                계\nG.T",]
D17_V1[V3=="5    ~    9세",]
D17_V1[V3=="20    ~    24세",]
D17_V1[V3=="35    ~    39세",]
D17_V1[V3=="50    ~    54세",]
D17_V1[V3=="65    ~    69세",]
D17_V1[V13=="75세이상",]
D17_V1[x1>=1 & x1<95,`:=` (pageX=1)]
D17_V1[x1>=95 & x1<189,`:=` (pageX=2)]
D17_V1[x1>=189 & x1<283,`:=` (pageX=3)]
D17_V1[x1>=283 & x1<377,`:=` (pageX=4)]
D17_V1[x1>=377 & x1<471,`:=` (pageX=5)]
D17_V1[x1>=471 & x1<565,`:=` (pageX=6)]
xx<-565
i<-7
repeat {
  D17_V1[x1>=xx & x1<xx+90,`:=` (pageX=i)]
  xx<-xx+90
  i<-i+1
  if (xx >= 8118) { break }
}
D17_V1[, V1 := NULL]
D17_V1[, V2 := NULL]
D17_V1[, V3 := NULL]
D17_V1[, V8 := NULL]
D17_V1[, V13 := NULL]
head(D17_V1)
D17_V1_0 <- D17_V1[D17_V1$V3 != ""]
D17_V1_01 <- D17_V1_0[D17_V1_0$V4 != "visits"]
D17_V1_1 <- D17_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D17_V1_1_split <- split(D17_V1_1, D17_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D17_V1_1_split[[i]][, `:=` (ageCtg = as.character(D17_V1_1_split[[i]][1,4]))]
}
D17_V1_1_A = do.call(rbind, D17_V1_1_split)
D17_V1_1_A_1 <- D17_V1_1_A[D17_V1_1_A$disGrp != "C"]
D17_V1_2 <- D17_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D17_V1_2_split <- split(D17_V1_2, D17_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D17_V1_2_split[[i]][, `:=` (ageCtg = as.character(D17_V1_2_split[[i]][1,4]))]
}
D17_V1_2_A = do.call(rbind, D17_V1_2_split)
D17_V1_2_A_1 <- D17_V1_2_A[D17_V1_2_A$disGrp != "C"]
D17_V1_3 <- D17_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D17_V1_3_split <- split(D17_V1_3, D17_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D17_V1_3_split[[i]][, `:=` (ageCtg = as.character(D17_V1_3_split[[i]][1,4]))]
}
D17_V1_3_A = do.call(rbind, D17_V1_3_split)
D17_V1_3_A_1 <- D17_V1_3_A[D17_V1_3_A$disGrp != "C"]
D17_V1_ALL <- rbind(D17_V1_1_A_1, D17_V1_2_A_1, D17_V1_3_A_1)
D17_V1_ALL_1 <- as.data.frame(apply(D17_V1_ALL, 2, function(x) gsub(",", "", x)))
D17_V1_ALL_GT_ST <- D17_V1_ALL_1[D17_V1_ALL_1$ToInOu == "ST" & D17_V1_ALL_1$ageCtg == "G.T",]
D17_V1_ALL_GT_ST1<-mutate(D17_V1_ALL_GT_ST, YR2017_nofPts=as.numeric(D17_V1_ALL_GT_ST$patients),  YR2017_exp=as.numeric(D17_V1_ALL_GT_ST$expense))
D17_V1_ALL_GT_ST_pts<-select(D17_V1_ALL_GT_ST1, disGrp, YR2017_nofPts, YR2017_exp)
ALL_pts_seleDis_2017 <- D17_V1_ALL_GT_ST_pts[D17_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2017)
str(ALL_pts_seleDis_2017)


# 2016

D16_NHIYB_T06_21 <- fread(input="NHI289_06_2_2016.tsv")
D16_NHIYB_T06_2 <- select(D16_NHIYB_T06_21, V1:V17)
str(D16_NHIYB_T06_2)
serNum<-data.frame(x1=1:8117)
D161<-cbind(serNum, D16_NHIYB_T06_2)
DF_sel<-D161[,c("x1","V1","V2")]
D16_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D16_sel1<-D16_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D16_sel1$V1, D16_sel1$lag_ref1, D16_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D16_V1<-as.data.table(merge(V1_shift2, D161, by="x1", all=TRUE))
D16_V1[V1=="구    분
Division",c(x1)]
D16_V1[,`:=` (V3_1 = V3)]
D16_V1[, `:=` (V8_1=V8)]
D16_V1[, `:=` (V13_1=V13)]
D16_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D16_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D16_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D16_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D16_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D16_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D16_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D16_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D16_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D16_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D16_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D16_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D16_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D16_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D16_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D16_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D16_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D16_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D16_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D16_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D16_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D16_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D16_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D16_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D16_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D16_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D16_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D16_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D16_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D16_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D16_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D16_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D16_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D16_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D16_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D16_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D16_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D16_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D16_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D16_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D16_V1[V3=="총                계\nG.T",]
D16_V1[V3=="5    ~    9세",]
D16_V1[V3=="20    ~    24세",]
D16_V1[V3=="35    ~    39세",]
D16_V1[V3=="50    ~    54세",]
D16_V1[V3=="65    ~    69세",]
D16_V1[V13=="75세이상",]
D16_V1[x1>=1 & x1<95,`:=` (pageX=1)]
D16_V1[x1>=95 & x1<189,`:=` (pageX=2)]
D16_V1[x1>=189 & x1<283,`:=` (pageX=3)]
D16_V1[x1>=283 & x1<377,`:=` (pageX=4)]
D16_V1[x1>=377 & x1<471,`:=` (pageX=5)]
D16_V1[x1>=471 & x1<565,`:=` (pageX=6)]
xx<-565
i<-7
repeat {
  D16_V1[x1>=xx & x1<xx+90,`:=` (pageX=i)]
  xx<-xx+90
  i<-i+1
  if (xx >= 8118) { break }
}
D16_V1[, V1 := NULL]
D16_V1[, V2 := NULL]
D16_V1[, V3 := NULL]
D16_V1[, V8 := NULL]
D16_V1[, V13 := NULL]
head(D16_V1)
D16_V1_0 <- D16_V1[D16_V1$V3 != ""]
D16_V1_01 <- D16_V1_0[D16_V1_0$V4 != "visits"]
D16_V1_1 <- D16_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D16_V1_1_split <- split(D16_V1_1, D16_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D16_V1_1_split[[i]][, `:=` (ageCtg = as.character(D16_V1_1_split[[i]][1,4]))]
}
D16_V1_1_A = do.call(rbind, D16_V1_1_split)
D16_V1_1_A_1 <- D16_V1_1_A[D16_V1_1_A$disGrp != "C"]
D16_V1_2 <- D16_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D16_V1_2_split <- split(D16_V1_2, D16_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D16_V1_2_split[[i]][, `:=` (ageCtg = as.character(D16_V1_2_split[[i]][1,4]))]
}
D16_V1_2_A = do.call(rbind, D16_V1_2_split)
D16_V1_2_A_1 <- D16_V1_2_A[D16_V1_2_A$disGrp != "C"]
D16_V1_3 <- D16_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D16_V1_3_split <- split(D16_V1_3, D16_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D16_V1_3_split[[i]][, `:=` (ageCtg = as.character(D16_V1_3_split[[i]][1,4]))]
}
D16_V1_3_A = do.call(rbind, D16_V1_3_split)
D16_V1_3_A_1 <- D16_V1_3_A[D16_V1_3_A$disGrp != "C"]
D16_V1_ALL <- rbind(D16_V1_1_A_1, D16_V1_2_A_1, D16_V1_3_A_1)
D16_V1_ALL_1 <- as.data.frame(apply(D16_V1_ALL, 2, function(x) gsub(",", "", x)))
D16_V1_ALL_GT_ST <- D16_V1_ALL_1[D16_V1_ALL_1$ToInOu == "ST" & D16_V1_ALL_1$ageCtg == "G.T",]
D16_V1_ALL_GT_ST1<-mutate(D16_V1_ALL_GT_ST, YR2016_nofPts=as.numeric(D16_V1_ALL_GT_ST$patients),  YR2016_exp=as.numeric(D16_V1_ALL_GT_ST$expense))
D16_V1_ALL_GT_ST_pts<-select(D16_V1_ALL_GT_ST1, disGrp, YR2016_nofPts, YR2016_exp)
ALL_pts_seleDis_2016 <- D16_V1_ALL_GT_ST_pts[D16_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2016)
str(ALL_pts_seleDis_2016)


# 2015

D15_NHIYB_T06_21 <- fread(input="NHI289_06_2_2015.tsv")
D15_NHIYB_T06_2 <- select(D15_NHIYB_T06_21, V1:V17)
str(D15_NHIYB_T06_2)
serNum<-data.frame(x1=1:8118)
D151<-cbind(serNum, D15_NHIYB_T06_2)
DF_sel<-D151[,c("x1","V1","V2")]
D15_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D15_sel1<-D15_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D15_sel1$V1, D15_sel1$lag_ref1, D15_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D15_V1<-as.data.table(merge(V1_shift2, D151, by="x1", all=TRUE))
D15_V1[V1=="구    분
Division",c(x1)]
D15_V1[,`:=` (V3_1 = V3)]
D15_V1[, `:=` (V8_1=V8)]
D15_V1[, `:=` (V13_1=V13)]
D15_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D15_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D15_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D15_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D15_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D15_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D15_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D15_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D15_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D15_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D15_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D15_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D15_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D15_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D15_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D15_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D15_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D15_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D15_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D15_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D15_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D15_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D15_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D15_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D15_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D15_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D15_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D15_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D15_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D15_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D15_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D15_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D15_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D15_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D15_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D15_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D15_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D15_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D15_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D15_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D15_V1[V3=="총                계\nG.T",]
D15_V1[V3=="5    ~    9세",]
D15_V1[V3=="20    ~    24세",]
D15_V1[V3=="35    ~    39세",]
D15_V1[V3=="50    ~    54세",]
D15_V1[V3=="65    ~    69세",]
D15_V1[V13=="75세이상",]
D15_V1[x1>=1 & x1<95,`:=` (pageX=1)]
D15_V1[x1>=95 & x1<189,`:=` (pageX=2)]
D15_V1[x1>=189 & x1<283,`:=` (pageX=3)]
D15_V1[x1>=283 & x1<377,`:=` (pageX=4)]
D15_V1[x1>=377 & x1<471,`:=` (pageX=5)]
D15_V1[x1>=471 & x1<565,`:=` (pageX=6)]
xx<-565
i<-7
repeat {
  D15_V1[x1>=xx & x1<xx+90,`:=` (pageX=i)]
  xx<-xx+90
  i<-i+1
  if (xx >= 8118) { break }
}
D15_V1[, V1 := NULL]
D15_V1[, V2 := NULL]
D15_V1[, V3 := NULL]
D15_V1[, V8 := NULL]
D15_V1[, V13 := NULL]
head(D15_V1)
D15_V1_0 <- D15_V1[D15_V1$V3 != ""]
D15_V1_01 <- D15_V1_0[D15_V1_0$V4 != "visits"]
D15_V1_1 <- D15_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D15_V1_1_split <- split(D15_V1_1, D15_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D15_V1_1_split[[i]][, `:=` (ageCtg = as.character(D15_V1_1_split[[i]][1,4]))]
}
D15_V1_1_A = do.call(rbind, D15_V1_1_split)
D15_V1_1_A_1 <- D15_V1_1_A[D15_V1_1_A$disGrp != "C"]
D15_V1_2 <- D15_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D15_V1_2_split <- split(D15_V1_2, D15_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D15_V1_2_split[[i]][, `:=` (ageCtg = as.character(D15_V1_2_split[[i]][1,4]))]
}
D15_V1_2_A = do.call(rbind, D15_V1_2_split)
D15_V1_2_A_1 <- D15_V1_2_A[D15_V1_2_A$disGrp != "C"]
D15_V1_3 <- D15_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D15_V1_3_split <- split(D15_V1_3, D15_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D15_V1_3_split[[i]][, `:=` (ageCtg = as.character(D15_V1_3_split[[i]][1,4]))]
}
D15_V1_3_A = do.call(rbind, D15_V1_3_split)
D15_V1_3_A_1 <- D15_V1_3_A[D15_V1_3_A$disGrp != "C"]
D15_V1_ALL <- rbind(D15_V1_1_A_1, D15_V1_2_A_1, D15_V1_3_A_1)
D15_V1_ALL_1 <- as.data.frame(apply(D15_V1_ALL, 2, function(x) gsub(",", "", x)))
D15_V1_ALL_GT_ST <- D15_V1_ALL_1[D15_V1_ALL_1$ToInOu == "ST" & D15_V1_ALL_1$ageCtg == "G.T",]
D15_V1_ALL_GT_ST1<-mutate(D15_V1_ALL_GT_ST, YR2015_nofPts=as.numeric(D15_V1_ALL_GT_ST$patients),  YR2015_exp=as.numeric(D15_V1_ALL_GT_ST$expense))
D15_V1_ALL_GT_ST_pts<-select(D15_V1_ALL_GT_ST1, disGrp, YR2015_nofPts, YR2015_exp)
ALL_pts_seleDis_2015 <- D15_V1_ALL_GT_ST_pts[D15_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2015)
str(ALL_pts_seleDis_2015)


# 2014

D14_NHIYB_T06_21 <- fread(input="NHI289_06_2_2014.tsv")
D14_NHIYB_T06_2 <- select(D14_NHIYB_T06_21, V1:V17)
str(D14_NHIYB_T06_2)
serNum<-data.frame(x1=1:8296)
D141<-cbind(serNum, D14_NHIYB_T06_2)
DF_sel<-D141[,c("x1","V1","V2")]
D14_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D14_sel1<-D14_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D14_sel1$V1, D14_sel1$lag_ref1, D14_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D14_V1<-as.data.table(merge(V1_shift2, D141, by="x1", all=TRUE))
D14_V1[V1=="구    분
Division",c(x1)]
D14_V1[,`:=` (V3_1 = V3)]
D14_V1[, `:=` (V8_1=V8)]
D14_V1[, `:=` (V13_1=V13)]
D14_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D14_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D14_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D14_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D14_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D14_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D14_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D14_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D14_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D14_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D14_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D14_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D14_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D14_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D14_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D14_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D14_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D14_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D14_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D14_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D14_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D14_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D14_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D14_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D14_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D14_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D14_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D14_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D14_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D14_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D14_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D14_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D14_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D14_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D14_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D14_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D14_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D14_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D14_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D14_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D14_V1[V3=="총                계\nG.T",]
D14_V1[V3=="5    ~    9세",]
D14_V1[V3=="20    ~    24세",]
D14_V1[V3=="35    ~    39세",]
D14_V1[V3=="50    ~    54세",]
D14_V1[V3=="65    ~    69세",]
D14_V1[V13=="75세이상",]
D14_V1[x1>=1 & x1<97,`:=` (pageX=1)]
D14_V1[x1>=97 & x1<193,`:=` (pageX=2)]
D14_V1[x1>=193 & x1<289,`:=` (pageX=3)]
D14_V1[x1>=289 & x1<385,`:=` (pageX=4)]
D14_V1[x1>=385 & x1<481,`:=` (pageX=5)]
D14_V1[x1>=481 & x1<577,`:=` (pageX=6)]
xx<-577
i<-7
repeat {
  D14_V1[x1>=xx & x1<xx+92,`:=` (pageX=i)]
  xx<-xx+92
  i<-i+1
  if (xx >= 8296) { break }
}
D14_V1[, V1 := NULL]
D14_V1[, V2 := NULL]
D14_V1[, V3 := NULL]
D14_V1[, V8 := NULL]
D14_V1[, V13 := NULL]
head(D14_V1)
D14_V1_0 <- D14_V1[D14_V1$V3 != ""]
D14_V1_01 <- D14_V1_0[D14_V1_0$V4 != "visits"]
D14_V1_1 <- D14_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D14_V1_1_split <- split(D14_V1_1, D14_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D14_V1_1_split[[i]][, `:=` (ageCtg = as.character(D14_V1_1_split[[i]][1,4]))]
}
D14_V1_1_A = do.call(rbind, D14_V1_1_split)
D14_V1_1_A_1 <- D14_V1_1_A[D14_V1_1_A$disGrp != "C"]
D14_V1_2 <- D14_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D14_V1_2_split <- split(D14_V1_2, D14_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D14_V1_2_split[[i]][, `:=` (ageCtg = as.character(D14_V1_2_split[[i]][1,4]))]
}
D14_V1_2_A = do.call(rbind, D14_V1_2_split)
D14_V1_2_A_1 <- D14_V1_2_A[D14_V1_2_A$disGrp != "C"]
D14_V1_3 <- D14_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D14_V1_3_split <- split(D14_V1_3, D14_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D14_V1_3_split[[i]][, `:=` (ageCtg = as.character(D14_V1_3_split[[i]][1,4]))]
}
D14_V1_3_A = do.call(rbind, D14_V1_3_split)
D14_V1_3_A_1 <- D14_V1_3_A[D14_V1_3_A$disGrp != "C"]
D14_V1_ALL <- rbind(D14_V1_1_A_1, D14_V1_2_A_1, D14_V1_3_A_1)
D14_V1_ALL_1 <- as.data.frame(apply(D14_V1_ALL, 2, function(x) gsub(",", "", x)))
D14_V1_ALL_GT_ST <- D14_V1_ALL_1[D14_V1_ALL_1$ToInOu == "ST" & D14_V1_ALL_1$ageCtg == "G.T",]
D14_V1_ALL_GT_ST1<-mutate(D14_V1_ALL_GT_ST, YR2014_nofPts=as.numeric(D14_V1_ALL_GT_ST$patients),  YR2014_exp=as.numeric(D14_V1_ALL_GT_ST$expense))
D14_V1_ALL_GT_ST_pts<-select(D14_V1_ALL_GT_ST1, disGrp, YR2014_nofPts, YR2014_exp)
ALL_pts_seleDis_2014 <- D14_V1_ALL_GT_ST_pts[D14_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2014)
str(ALL_pts_seleDis_2014)


# 2013

D13_NHIYB_T06_21 <- fread(input="NHI289_06_2_2013.tsv")
D13_NHIYB_T06_2 <- select(D13_NHIYB_T06_21, V1:V17)
str(D13_NHIYB_T06_2)
serNum<-data.frame(x1=1:8296)
D131<-cbind(serNum, D13_NHIYB_T06_2)
DF_sel<-D131[,c("x1","V1","V2")]
D13_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D13_sel1<-D13_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D13_sel1$V1, D13_sel1$lag_ref1, D13_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D13_V1<-as.data.table(merge(V1_shift2, D131, by="x1", all=TRUE))
D13_V1[V1=="구    분
Division",c(x1)]
D13_V1[,`:=` (V3_1 = V3)]
D13_V1[, `:=` (V8_1=V8)]
D13_V1[, `:=` (V13_1=V13)]
D13_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D13_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D13_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D13_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D13_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D13_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D13_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D13_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D13_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D13_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D13_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D13_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D13_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D13_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D13_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D13_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D13_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D13_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D13_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D13_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D13_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D13_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D13_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D13_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D13_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D13_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D13_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D13_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D13_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D13_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D13_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D13_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D13_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D13_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D13_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D13_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D13_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D13_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D13_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D13_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D13_V1[V3=="총                계\nG.T",]
D13_V1[V3=="5    ~    9세",]
D13_V1[V3=="20    ~    24세",]
D13_V1[V3=="35    ~    39세",]
D13_V1[V3=="50    ~    54세",]
D13_V1[V3=="65    ~    69세",]
D13_V1[V13=="75세이상",]
D13_V1[x1>=1 & x1<97,`:=` (pageX=1)]
D13_V1[x1>=97 & x1<193,`:=` (pageX=2)]
D13_V1[x1>=193 & x1<289,`:=` (pageX=3)]
D13_V1[x1>=289 & x1<385,`:=` (pageX=4)]
D13_V1[x1>=385 & x1<481,`:=` (pageX=5)]
D13_V1[x1>=481 & x1<577,`:=` (pageX=6)]
xx<-577
i<-7
repeat {
  D13_V1[x1>=xx & x1<xx+92,`:=` (pageX=i)]
  xx<-xx+92
  i<-i+1
  if (xx >= 8296) { break }
}
D13_V1[, V1 := NULL]
D13_V1[, V2 := NULL]
D13_V1[, V3 := NULL]
D13_V1[, V8 := NULL]
D13_V1[, V13 := NULL]
head(D13_V1)
D13_V1_0 <- D13_V1[D13_V1$V3 != ""]
D13_V1_01 <- D13_V1_0[D13_V1_0$V4 != "visits"]
D13_V1_1 <- D13_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D13_V1_1_split <- split(D13_V1_1, D13_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D13_V1_1_split[[i]][, `:=` (ageCtg = as.character(D13_V1_1_split[[i]][1,4]))]
}
D13_V1_1_A = do.call(rbind, D13_V1_1_split)
D13_V1_1_A_1 <- D13_V1_1_A[D13_V1_1_A$disGrp != "C"]
D13_V1_2 <- D13_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D13_V1_2_split <- split(D13_V1_2, D13_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D13_V1_2_split[[i]][, `:=` (ageCtg = as.character(D13_V1_2_split[[i]][1,4]))]
}
D13_V1_2_A = do.call(rbind, D13_V1_2_split)
D13_V1_2_A_1 <- D13_V1_2_A[D13_V1_2_A$disGrp != "C"]
D13_V1_3 <- D13_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D13_V1_3_split <- split(D13_V1_3, D13_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D13_V1_3_split[[i]][, `:=` (ageCtg = as.character(D13_V1_3_split[[i]][1,4]))]
}
D13_V1_3_A = do.call(rbind, D13_V1_3_split)
D13_V1_3_A_1 <- D13_V1_3_A[D13_V1_3_A$disGrp != "C"]
D13_V1_ALL <- rbind(D13_V1_1_A_1, D13_V1_2_A_1, D13_V1_3_A_1)
D13_V1_ALL_1 <- as.data.frame(apply(D13_V1_ALL, 2, function(x) gsub(",", "", x)))
D13_V1_ALL_GT_ST <- D13_V1_ALL_1[D13_V1_ALL_1$ToInOu == "ST" & D13_V1_ALL_1$ageCtg == "G.T",]
D13_V1_ALL_GT_ST1<-mutate(D13_V1_ALL_GT_ST, YR2013_nofPts=as.numeric(D13_V1_ALL_GT_ST$patients),  YR2013_exp=as.numeric(D13_V1_ALL_GT_ST$expense))
D13_V1_ALL_GT_ST_pts<-select(D13_V1_ALL_GT_ST1, disGrp, YR2013_nofPts, YR2013_exp)
ALL_pts_seleDis_2013 <- D13_V1_ALL_GT_ST_pts[D13_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2013)
str(ALL_pts_seleDis_2013)


# 2012

D12_NHIYB_T06_21 <- fread(input="NHI289_06_2_2012.tsv")
D12_NHIYB_T06_2 <- select(D12_NHIYB_T06_21, V1:V17)
str(D12_NHIYB_T06_2)
serNum<-data.frame(x1=1:8296)
D121<-cbind(serNum, D12_NHIYB_T06_2)
DF_sel<-D121[,c("x1","V1","V2")]
D12_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D12_sel1<-D12_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D12_sel1$V1, D12_sel1$lag_ref1, D12_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D12_V1<-as.data.table(merge(V1_shift2, D121, by="x1", all=TRUE))
D12_V1[V1=="구    분
Division",c(x1)]
D12_V1[,`:=` (V3_1 = V3)]
D12_V1[, `:=` (V8_1=V8)]
D12_V1[, `:=` (V13_1=V13)]
D12_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D12_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D12_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D12_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D12_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D12_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D12_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D12_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D12_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D12_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D12_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D12_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D12_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D12_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D12_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D12_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D12_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D12_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D12_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D12_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D12_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D12_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D12_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D12_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D12_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D12_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D12_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D12_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D12_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D12_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D12_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D12_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D12_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D12_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D12_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D12_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D12_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D12_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D12_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D12_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D12_V1[V3=="총                계\nG.T",]
D12_V1[V3=="5    ~    9세",]
D12_V1[V3=="20    ~    24세",]
D12_V1[V3=="35    ~    39세",]
D12_V1[V3=="50    ~    54세",]
D12_V1[V3=="65    ~    69세",]
D12_V1[V13=="75세이상",]
D12_V1[x1>=1 & x1<97,`:=` (pageX=1)]
D12_V1[x1>=97 & x1<193,`:=` (pageX=2)]
D12_V1[x1>=193 & x1<289,`:=` (pageX=3)]
D12_V1[x1>=289 & x1<385,`:=` (pageX=4)]
D12_V1[x1>=385 & x1<481,`:=` (pageX=5)]
D12_V1[x1>=481 & x1<577,`:=` (pageX=6)]
xx<-577
i<-7
repeat {
  D12_V1[x1>=xx & x1<xx+92,`:=` (pageX=i)]
  xx<-xx+92
  i<-i+1
  if (xx >= 8296) { break }
}
D12_V1[, V1 := NULL]
D12_V1[, V2 := NULL]
D12_V1[, V3 := NULL]
D12_V1[, V8 := NULL]
D12_V1[, V13 := NULL]
head(D12_V1)
D12_V1_0 <- D12_V1[D12_V1$V3 != ""]
D12_V1_01 <- D12_V1_0[D12_V1_0$V4 != "visits"]
D12_V1_1 <- D12_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D12_V1_1_split <- split(D12_V1_1, D12_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D12_V1_1_split[[i]][, `:=` (ageCtg = as.character(D12_V1_1_split[[i]][1,4]))]
}
D12_V1_1_A = do.call(rbind, D12_V1_1_split)
D12_V1_1_A_1 <- D12_V1_1_A[D12_V1_1_A$disGrp != "C"]
D12_V1_2 <- D12_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D12_V1_2_split <- split(D12_V1_2, D12_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D12_V1_2_split[[i]][, `:=` (ageCtg = as.character(D12_V1_2_split[[i]][1,4]))]
}
D12_V1_2_A = do.call(rbind, D12_V1_2_split)
D12_V1_2_A_1 <- D12_V1_2_A[D12_V1_2_A$disGrp != "C"]
D12_V1_3 <- D12_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D12_V1_3_split <- split(D12_V1_3, D12_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D12_V1_3_split[[i]][, `:=` (ageCtg = as.character(D12_V1_3_split[[i]][1,4]))]
}
D12_V1_3_A = do.call(rbind, D12_V1_3_split)
D12_V1_3_A_1 <- D12_V1_3_A[D12_V1_3_A$disGrp != "C"]
D12_V1_ALL <- rbind(D12_V1_1_A_1, D12_V1_2_A_1, D12_V1_3_A_1)
D12_V1_ALL_1 <- as.data.frame(apply(D12_V1_ALL, 2, function(x) gsub(",", "", x)))
D12_V1_ALL_GT_ST <- D12_V1_ALL_1[D12_V1_ALL_1$ToInOu == "ST" & D12_V1_ALL_1$ageCtg == "G.T",]
D12_V1_ALL_GT_ST1<-mutate(D12_V1_ALL_GT_ST, YR2012_nofPts=as.numeric(D12_V1_ALL_GT_ST$patients),  YR2012_exp=as.numeric(D12_V1_ALL_GT_ST$expense))
D12_V1_ALL_GT_ST_pts<-select(D12_V1_ALL_GT_ST1, disGrp, YR2012_nofPts, YR2012_exp)
ALL_pts_seleDis_2012 <- D12_V1_ALL_GT_ST_pts[D12_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2012)
str(ALL_pts_seleDis_2012)


# 2011

D11_NHIYB_T06_21 <- fread(input="NHI289_06_2_2011.tsv")
D11_NHIYB_T06_2 <- select(D11_NHIYB_T06_21, V1:V17)
str(D11_NHIYB_T06_2)
serNum<-data.frame(x1=1:8296)
D111<-cbind(serNum, D11_NHIYB_T06_2)
DF_sel<-D111[,c("x1","V1","V2")]
D11_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D11_sel1<-D11_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D11_sel1$V1, D11_sel1$lag_ref1, D11_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D11_V1<-as.data.table(merge(V1_shift2, D111, by="x1", all=TRUE))
D11_V1[V1=="구    분
Division",c(x1)]
D11_V1[,`:=` (V3_1 = V3)]
D11_V1[, `:=` (V8_1=V8)]
D11_V1[, `:=` (V13_1=V13)]
D11_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D11_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D11_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D11_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D11_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D11_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D11_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D11_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D11_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D11_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D11_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D11_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D11_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D11_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D11_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D11_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D11_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D11_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D11_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D11_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D11_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D11_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D11_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D11_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D11_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D11_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D11_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D11_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D11_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D11_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D11_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D11_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D11_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D11_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D11_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D11_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D11_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D11_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D11_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D11_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D11_V1[V3=="총                계\nG.T",]
D11_V1[V3=="5    ~    9세",]
D11_V1[V3=="20    ~    24세",]
D11_V1[V3=="35    ~    39세",]
D11_V1[V3=="50    ~    54세",]
D11_V1[V3=="65    ~    69세",]
D11_V1[V13=="75세이상",]
D11_V1[x1>=1 & x1<97,`:=` (pageX=1)]
D11_V1[x1>=97 & x1<193,`:=` (pageX=2)]
D11_V1[x1>=193 & x1<289,`:=` (pageX=3)]
D11_V1[x1>=289 & x1<385,`:=` (pageX=4)]
D11_V1[x1>=385 & x1<481,`:=` (pageX=5)]
D11_V1[x1>=481 & x1<577,`:=` (pageX=6)]
xx<-577
i<-7
repeat {
  D11_V1[x1>=xx & x1<xx+92,`:=` (pageX=i)]
  xx<-xx+92
  i<-i+1
  if (xx >= 8296) { break }
}
D11_V1[, V1 := NULL]
D11_V1[, V2 := NULL]
D11_V1[, V3 := NULL]
D11_V1[, V8 := NULL]
D11_V1[, V13 := NULL]
head(D11_V1)
D11_V1_0 <- D11_V1[D11_V1$V3 != ""]
D11_V1_01 <- D11_V1_0[D11_V1_0$V4 != "visits"]
D11_V1_1 <- D11_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D11_V1_1_split <- split(D11_V1_1, D11_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D11_V1_1_split[[i]][, `:=` (ageCtg = as.character(D11_V1_1_split[[i]][1,4]))]
}
D11_V1_1_A = do.call(rbind, D11_V1_1_split)
D11_V1_1_A_1 <- D11_V1_1_A[D11_V1_1_A$disGrp != "C"]
D11_V1_2 <- D11_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D11_V1_2_split <- split(D11_V1_2, D11_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D11_V1_2_split[[i]][, `:=` (ageCtg = as.character(D11_V1_2_split[[i]][1,4]))]
}
D11_V1_2_A = do.call(rbind, D11_V1_2_split)
D11_V1_2_A_1 <- D11_V1_2_A[D11_V1_2_A$disGrp != "C"]
D11_V1_3 <- D11_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D11_V1_3_split <- split(D11_V1_3, D11_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D11_V1_3_split[[i]][, `:=` (ageCtg = as.character(D11_V1_3_split[[i]][1,4]))]
}
D11_V1_3_A = do.call(rbind, D11_V1_3_split)
D11_V1_3_A_1 <- D11_V1_3_A[D11_V1_3_A$disGrp != "C"]
D11_V1_ALL <- rbind(D11_V1_1_A_1, D11_V1_2_A_1, D11_V1_3_A_1)
D11_V1_ALL_1 <- as.data.frame(apply(D11_V1_ALL, 2, function(x) gsub(",", "", x)))
D11_V1_ALL_GT_ST <- D11_V1_ALL_1[D11_V1_ALL_1$ToInOu == "ST" & D11_V1_ALL_1$ageCtg == "G.T",]
D11_V1_ALL_GT_ST1<-mutate(D11_V1_ALL_GT_ST, YR2011_nofPts=as.numeric(D11_V1_ALL_GT_ST$patients),  YR2011_exp=as.numeric(D11_V1_ALL_GT_ST$expense))
D11_V1_ALL_GT_ST_pts<-select(D11_V1_ALL_GT_ST1, disGrp, YR2011_nofPts, YR2011_exp)
ALL_pts_seleDis_2011 <- D11_V1_ALL_GT_ST_pts[D11_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]
head(ALL_pts_seleDis_2011)
str(ALL_pts_seleDis_2011)


# 2010

D10_NHIYB_T06_21 <- fread(input="NHI289_06_2_2010.tsv")
D10_NHIYB_T06_2 <- select(D10_NHIYB_T06_21, V1:V17)
str(D10_NHIYB_T06_2)
serNum<-data.frame(x1=1:8296)
D101<-cbind(serNum, D10_NHIYB_T06_2)
DF_sel<-D101[,c("x1","V1","V2")]
D10_sel<-as.data.table(DF_sel)
cols = c("ref1", "ref2", "ref3")
anscols = paste("lag", cols, sep="_")
D10_sel1<-D10_sel[, (anscols) := shift(.SD, 1:3, 0 , "lag"), .SDcols=c("V1")]
V1_shift <- data.table( D10_sel1$V1, D10_sel1$lag_ref1, D10_sel1$lag_ref2)
V1_shift1 <- apply(V1_shift, 1, max, na.rm = TRUE)
V1_shift2 <- cbind(serNum, V1_shift1)
D10_V1<-as.data.table(merge(V1_shift2, D101, by="x1", all=TRUE))
D10_V1[V1=="구    분
Division",c(x1)]
D10_V1[,`:=` (V3_1 = V3)]
D10_V1[, `:=` (V8_1=V8)]
D10_V1[, `:=` (V13_1=V13)]
D10_V1[V2=="계　　S.T" | V2=="계　T", `:=` (V2_1="ST")]
D10_V1[V2=="입원　I.P", `:=` (V2_1="IP")]
D10_V1[V2=="외래　O.P", `:=` (V2_1="OP")]
D10_V1[V3=="진료실인원수\nPatients", `:=` (V3_1="patients")]
D10_V1[V4=="입내원일수\nVisits", `:=` (V4="visits")]
D10_V1[V5=="요양급여일수\nReimbursed             Days", `:=` (V5="med_days")]
D10_V1[V6=="진료비\nMedical                  Expense", `:=` (V6="expense")]
D10_V1[V7=="급여비\nBenefit", `:=` (V7="benefit")]
D10_V1[V8=="진료실인원수\nPatients", `:=` (V8_1="patients")]
D10_V1[V9=="입내원일수\nVisits", `:=` (V9="visits")]
D10_V1[V10=="요양급여일수\nReimbursed             Days", `:=` (V10="med_days")]
D10_V1[V11=="진료비\nMedical                  Expense", `:=` (V11="expense")]
D10_V1[V12=="급여비\nBenefit", `:=` (V12="benefit")]
D10_V1[V13=="진료실인원수\nPatients", `:=` (V13_1="patients")]
D10_V1[V14=="입내원일수\nVisits", `:=` (V14="visits")]
D10_V1[V15=="요양급여일수\nReimbursed             Days", `:=` (V15="med_days")]
D10_V1[V16=="진료비\nMedical                  Expense", `:=` (V16="expense")]
D10_V1[V17=="급여비\nBenefit", `:=` (V17="benefit")]
D10_V1[V1_shift1=="총계\nTotal", `:=` (V1_shift1="A")]
D10_V1[V1_shift1=="구    분\nDivision", `:=` (V1_shift1="C")]
D10_V1[V3=="총                계\nG.T", `:=` (V3_1="G.T",V4 = "G.T", V5 = "G.T", V6 = "G.T", V7 = "G.T")]
D10_V1[V3=="5    ~    9세", `:=` (V3_1="5-9",V4 = "5-9", V5 = "5-9", V6 = "5-9", V7 = "5-9")]
D10_V1[V3=="20    ~    24세", `:=` (V3_1="20-24",V4 = "20-24", V5 = "20-24", V6 = "20-24", V7 = "20-24")]
D10_V1[V3=="35    ~    39세", `:=` (V3_1="35-39",V4 = "35-39", V5 = "35-39", V6 = "35-39", V7 = "35-39")]
D10_V1[V3=="50    ~    54세", `:=` (V3_1="50-54",V4 = "50-54", V5 = "50-54", V6 = "50-54", V7 = "50-54")]
D10_V1[V3=="65    ~    69세", `:=` (V3_1="65-69",V4 = "65-69", V5 = "65-69", V6 = "65-69", V7 = "65-69")]
D10_V1[V8=="0세", `:=` (V8_1="0",  V9 = "0", V10 = "0", V11 = "0", V12 = "0")]
D10_V1[V8=="10세    ~", `:=` (V8_1="10-14", V9 = "10-14", V10 = "10-14", V11 = "10-14", V12 = "10-14")]
D10_V1[V8=="25세    ~", `:=` (V8_1="25-29",V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")]
D10_V1[V8=="40세    ~", `:=` (V8_1="40-44",V9 = "40-44", V10 = "40-44", V11 = "40-44", V12 = "40-44")]
D10_V1[V8=="55세    ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")]
D10_V1[V8=="70세    ~", `:=` (V8_1="70-74", V9 = "70-74", V10 = "70-74", V11 = "70-74", V12 = "70-74")]
D10_V1[V13=="1    ~    4세", `:=` (V13_1="1-4",V14 = "1-4", V15 = "1-4", V16 = "1-4", V17 = "1-4")]
D10_V1[V13=="15    ~    19세", `:=` (V13_1="15-19",V14 = "15-19", V15 = "15-19", V16 = "15-19", V17 = "15-19")]
D10_V1[V13=="30    ~    34세", `:=` (V13_1="30-34",V14 = "30-34", V15 = "30-34", V16 = "30-34", V17 = "30-34")]
D10_V1[V13=="45세    ~    49세", `:=` (V13_1="45-49",V14 = "45-49", V15 = "45-49", V16 = "45-49", V17 = "45-49")]
D10_V1[V13=="60    ~    64세", `:=` (V13_1="60-64",V14 = "60-64", V15 = "60-64", V16 = "60-64", V17 = "60-64")]
D10_V1[V13=="75세이상", `:=` (V13_1="75<", V14 = "75<", V15 = "75<", V16 = "75<", V17 = "75<")]
D10_V1[V8=="25세   ~", `:=` (V8_1= "25-29", V9 = "25-29", V10 = "25-29", V11 = "25-29", V12 = "25-29")] #space 하나 부족
D10_V1[V8=="55세     ~", `:=` (V8_1="55-59",V9 = "55-59", V10 = "55-59", V11 = "55-59", V12 = "55-59")] #space 하나 많음
D10_V1[V3=="총                계\nG.T",]
D10_V1[V3=="5    ~    9세",]
D10_V1[V3=="20    ~    24세",]
D10_V1[V3=="35    ~    39세",]
D10_V1[V3=="50    ~    54세",]
D10_V1[V3=="65    ~    69세",]
D10_V1[V13=="75세이상",]
D10_V1[x1>=1 & x1<97,`:=` (pageX=1)]
D10_V1[x1>=97 & x1<193,`:=` (pageX=2)]
D10_V1[x1>=193 & x1<289,`:=` (pageX=3)]
D10_V1[x1>=289 & x1<385,`:=` (pageX=4)]
D10_V1[x1>=385 & x1<481,`:=` (pageX=5)]
D10_V1[x1>=481 & x1<577,`:=` (pageX=6)]
xx<-577
i<-7
repeat {
  D10_V1[x1>=xx & x1<xx+92,`:=` (pageX=i)]
  xx<-xx+92
  i<-i+1
  if (xx >= 8296) { break }
}
D10_V1[, V1 := NULL]
D10_V1[, V2 := NULL]
D10_V1[, V3 := NULL]
D10_V1[, V8 := NULL]
D10_V1[, V13 := NULL]
head(D10_V1)
D10_V1_0 <- D10_V1[D10_V1$V3 != ""]
D10_V1_01 <- D10_V1_0[D10_V1_0$V4 != "visits"]
D10_V1_1 <- D10_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V4, med_days=V5, expense=V6, benefit=V7, patients=V3_1, ToInOu=V2_1, pageX)]
D10_V1_1_split <- split(D10_V1_1, D10_V1_1$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D10_V1_1_split[[i]][, `:=` (ageCtg = as.character(D10_V1_1_split[[i]][1,4]))]
}
D10_V1_1_A = do.call(rbind, D10_V1_1_split)
D10_V1_1_A_1 <- D10_V1_1_A[D10_V1_1_A$disGrp != "C"]
D10_V1_2 <- D10_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V9, med_days=V10, expense=V11, benefit=V12, patients=V8_1, ToInOu=V2_1, pageX)]
D10_V1_2_split <- split(D10_V1_2, D10_V1_2$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D10_V1_2_split[[i]][, `:=` (ageCtg = as.character(D10_V1_2_split[[i]][1,4]))]
}
D10_V1_2_A = do.call(rbind, D10_V1_2_split)
D10_V1_2_A_1 <- D10_V1_2_A[D10_V1_2_A$disGrp != "C"]
D10_V1_3 <- D10_V1_01[, .(ser=x1, disGrp=V1_shift1, visits=V14, med_days=V15, expense=V16, benefit=V17, patients=V13_1, ToInOu=V2_1, pageX)]
D10_V1_3_split <- split(D10_V1_3, D10_V1_3$pageX)
eleNum = c(1:90)
for (i in eleNum) {
  D10_V1_3_split[[i]][, `:=` (ageCtg = as.character(D10_V1_3_split[[i]][1,4]))]
}
D10_V1_3_A = do.call(rbind, D10_V1_3_split)
D10_V1_3_A_1 <- D10_V1_3_A[D10_V1_3_A$disGrp != "C"]
D10_V1_ALL <- rbind(D10_V1_1_A_1, D10_V1_2_A_1, D10_V1_3_A_1)
D10_V1_ALL_1 <- as.data.table(apply(D10_V1_ALL, 2, function(x) gsub(",", "", x)))
D10_V1_ALL_GT_ST <- D10_V1_ALL_1[D10_V1_ALL_1$ToInOu == "ST" & D10_V1_ALL_1$ageCtg == "G.T",]
D10_V1_ALL_GT_ST1<-mutate(D10_V1_ALL_GT_ST, YR2010_nofPts=as.numeric(D10_V1_ALL_GT_ST$patients),  YR2010_exp=as.numeric(D10_V1_ALL_GT_ST$expense))
D10_V1_ALL_GT_ST_pts<-select(D10_V1_ALL_GT_ST1, disGrp, YR2010_nofPts, YR2010_exp)
ALL_pts_seleDis_2010 <- D10_V1_ALL_GT_ST_pts[D10_V1_ALL_GT_ST_pts$disGrp %in% c("104", "112", "121", "122", "145", "146", "147", "148", "151", "152", "153", "154", "155",  "214"),]

str(ALL_pts_seleDis_2010)


# 연도별 질병분류별 환자수를 합침

install.packages("dbplyr")
library(dbplyr, warn.conflicts = FALSE)
ptsByYearDisGrp2 <- ALL_pts_seleDis_2010 |> inner_join(ALL_pts_seleDis_2011, by = 'disGrp') |> inner_join(ALL_pts_seleDis_2012, by = 'disGrp') |> inner_join(ALL_pts_seleDis_2013, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2014, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2015, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2016, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2017, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2018, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2019, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2020, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2021, by = 'disGrp')|> inner_join(ALL_pts_seleDis_2022, by = 'disGrp')

install.packages("writexl")
library(writexl)
write_xlsx(ptsByYearDisGrp2, "ptsByYearDisGrp2.xlsx")

```
