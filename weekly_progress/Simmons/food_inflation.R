xfun::download_file("https://www.dropbox.com/s/ouurt80t6ailbif/PR0101A2Mc.csv?dl=1", mode="wb")
library(readr)
cpi <- read_csv("PR0101A2Mc.csv",
locale = locale(encoding = "CP950"),
skip = 3)
data_explore <- list(
  original = cpi
)



library(magrittr)
library(dplyr)

# 挑選所需項目

cpi %>%select("一.食物類","1.穀類及其製品","2.肉類"  ,"3.肉類製品" ,"4.蛋類" ,"5.水產品","6.加工水產品","7.蔬菜","8.加工蔬菜", "9.水果" ,"10.加工水果","11.乳類","12.食用油","13.調味品","14.酒","15.非酒精性飲料及材料","16.調理食品","17.外食費","18.其他食品")  ->cpi_food

cpi_food <- cpi_food[-c(486:494),]


# 計算出上漲率

cpi_food %>% mutate_all(
  
  function(data){
    
    for (i in seq_along(data)){
      
      
      if (i<=473){
      
      ((data[[i+12]]-data[[i]])/data[[i]])*100 -> data[[i]]
        
      }
      
    }  
    
    return(data)

    
  }
  
)  -> cpi_food_risingRate

cpi_food_risingRate <- cpi_food_risingRate[-c(474:485),]



# 計算出 cor 值
cpi_food_cor <- vector(mode="list",length=18)

for (i in seq_along(cpi_food_risingRate)){
  
  if (i<=(length(cpi_food_risingRate)-1))
  
  cpi_food_cor[[i]] <- cor.test(unlist(cpi_food_risingRate[,1]),unlist(cpi_food_risingRate[,i+1]))
  
}



#直接取出cor值

cpi_food_only_cor<- vector(mode="list",length=18)


for (i in seq_along(cpi_food_cor)){
  
  cpi_food_only_cor[[i]] <- cpi_food_cor[[i]][["estimate"]][["cor"]]

}


# 用stringr 取出1到18類的名稱

library(stringr)

regex_pattern <- "(1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18)[.]"

names_food <- { str_subset(names(cpi),pattern=regex_pattern)

               names_food[c(1:18)]  }
               
# 然後帶入DataFrame中

table_foodInflation_dataFrame <- cbind.data.frame(類別=names_food,上漲率=unlist( cpi_food_only_cor))


# 辨識食物上漲率最大元兇

list_foodInflation <- as.list(as.character(table_foodInflation_dataFrame[,2]))

paste("牽動食物物價年上漲率元兇是第",str_which(list_foodInflation,as.character(max(table_foodInflation_dataFrame[,2]))),"類")












