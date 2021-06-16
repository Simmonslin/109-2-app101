
  
xfun::download_file("https://www.dropbox.com/s/ouurt80t6ailbif/PR0101A2Mc.csv?dl=1", mode="wb")

library(readr)
cpi <- read_csv("PR0101A2Mc.csv",
                locale = locale(encoding = "CP950"),
                skip = 3)
library(dplyr)


#留下總指數與大類:

cpi_all <- cpi[1:485,]

library(magrittr)

# 用 select( ) 留下所需項目

cpi_all%<>% select("總指數","一.食物類","二.衣著類","三.居住類","四.交通及通訊類","五.醫藥保健類","六.教養娛樂類","七.雜項類")

cpi_all

# cpi_all_copy 保留原先資料
 
cpi_all_copy <- cpi_all


# 2.計算年物價上漲率


# cpi_all -> 原先資料 + 上漲率

cpi_all %>% mutate_all( 
  
  function(data){
    
    for (i in seq_along(data)){
      
      if (i <= 473){
        
        ((data[i+12]-data[i])/data[i])*100 -> data[i] }
      
      
    }
    
    data
  }
  
  
) -> cpi_all$年上漲率


bind_cols(cpi_all[-c(1:12),1],cpi_all$年上漲率)


cpi_all <- cpi_all[-c(474:485),]


# 用cor.test() 得到 cor值


cor_list <- vector(mode="list",length=7)


for (i in 1:7){
  
  
  cor_list[[i]] <- cor.test(unlist(cpi_all$年上漲率[,1]),unlist(cpi_all$年上漲率[,i+1]))
  
  
}


# 取出cor.test 中的 cor 值並整理成data.frame (第1大類 ~ 第7大類)

unlist_cor_list <- unlist(cor_list)

cor_number_list <- vector(mode="list",length=7)

for (i in seq_along(cor_number_list)){
  
  cor_number_list[[i]] <- unlist_cor_list[[10*i-6]]
  
}




unlist_cor_number<- unlist(cor_number_list)

cor_number_frame <- cbind.data.frame(number=1:7,cor=unlist_cor_number)

cor_number_frame











