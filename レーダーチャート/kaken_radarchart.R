#===================================================================================================

# パッケージの読み込み

#===================================================================================================


library(dplyr)
library(stringr)
library(tidyr)
library(DT)
library(purrr)
library(fmsb)
library(ggplot2)
library(gridExtra)
library(data.table)


#===================================================================================================

# データのインポート＆クリーニング

#===================================================================================================


# データの読み込み（2018年度と2019年度）
d <- bind_rows(
    fread("2018_20200301.csv", stringsAsFactors = F),
    fread("2019_20200301.csv", stringsAsFactors = F)
)

kubun <- read.csv("kaken_kubun.csv", stringsAsFactors = F) %>%
    mutate(小区分コード = ifelse(str_count(小区分コード) == 4, paste0("0", 小区分コード), 小区分コード)) %>%
    mutate(中区分コード = as.character(中区分コード))

NU <- read.csv("NationalUniv.csv", stringsAsFactors = F, fileEncoding = "CP932")


# データの整理
df <- d %>% select(`研究課題/領域番号`, 審査区分, 研究機関, 総配分額) %>%

    # 研究機関列に複数の機関情報が入っている際の処理
    mutate(研究機関 = str_replace(研究機関, "(^.+/ )(.+$)", "\\2")) %>%
    mutate(研究機関 = str_replace(研究機関, "\\(\\d+\\)$", "")) %>%

    # 審査区分の絞り込みと整理
    filter(str_detect(審査区分, "小区分|中区分|大区分")) %>%
    mutate(カテゴリー = ifelse(str_detect(審査区分, "小区分"), "小区分",
                          ifelse(str_detect(審査区分, "中区分"), "中区分",
                                 ifelse(str_detect(審査区分, "大区分"), "大区分", "")))) %>%
    mutate(コード = str_extract(審査区分, "\\d+")) %>%

    # 中区分列の作成
    left_join(kubun %>% select(小区分コード, 中区分コード) %>% unique, by = c("コード" = "小区分コード")) %>%
    mutate(中区分コード = ifelse(カテゴリー == "中区分",  コード, 中区分コード)) %>%
    
    # 大区分列を結合
    left_join(kubun %>% select(中区分コード, 大区分) %>% unique) %>%
    mutate(大区分 = ifelse(カテゴリー == "大区分",  審査区分, 大区分))
    

# 複数の中区分にまたがる小区分の研究課題は、中区分をつけた際にコピーが作られる。
# 中区分から大区分でも同様（中区分で集計する際には、大区分を除いてユニークな行を取るべし）
    

#===================================================================================================

# 各区分内でlogをとって正規化

#===================================================================================================


# 関数作成
clean.data <- function(x, y){

    x %>% group_by(研究機関) %>%
        summarize(N = n(), Total = sum(as.numeric(総配分額))) %>%
        mutate(LogAmount = log10(Total)-max(log10(Total)-4)) %>%
        mutate(LogCount = log10(N)-max(log10(N)-4)) %>%
        mutate(area = y)
    
}


# 関数の適用＆アウトプット
Mid <- df %>% select(-大区分) %>% unique %>%
    nest(-中区分コード) %>%
    mutate(clean = map2(data, 中区分コード, clean.data)) %>%
    filter(!is.na(中区分コード))

Lrg <- df %>% unique %>% nest(-大区分) %>%
    mutate(clean = map2(data, 大区分, clean.data)) %>%
    filter(!is.na(大区分))


#===================================================================================================

# レーダーチャート

#===================================================================================================


#----------------------
# 中区分
#----------------------


for (i in NU$NationalUniv){


MidLog <- Mid$clean %>% 
    
    # データの整理
    bind_rows %>% select(研究機関, LogAmount, LogCount, area) %>%
    rename(採択件数 = LogCount, 総配分額 = LogAmount) %>%
    gather(key = "key", value = "value", -研究機関, -area) %>%
    spread(key = area, value = value, fill = 0) %>%
    gather(key = "area", value = "value", -研究機関, -key) %>%
    mutate(area = as.numeric(area)) %>% arrange(area) %>%
    mutate(area = as.factor(area)) %>%
    
    # 大学名フィルター
    filter(研究機関 == i) %>%
    
    # 描画開始
    ggplot(aes(x = area, y = value, group = key)) +
    geom_polygon(aes(color = key), fill = NA, alpha = 0.5) +
    geom_point(aes(color = key), size = 1, alpha = 0.5) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 25)) +
    ylim(0,4) +
    coord_polar() +
    
    # 外周に大区分ラベルをつける（別になくてもいいけど、物寂しいから）
    annotate("segment", x = c(1, 11, 18, 26, 32, 38, 43, 47, 50, 60, 63),
             y = 4, yend = 4,
             xend = c(10, 17, 25, 31, 37, 42, 46, 49, 59, 62, 64),
             color = c("pink", "yellow", "skyblue", "coral", "darkseagreen1", "plum2", "orange1", "cyan", "greenyellow", "bisque", "lightblue1"),
             size = 2) +
    annotate("text", x = c(5.5, 14, 21.5, 28.5, 34.5, 40, 44.5, 48, 54.5, 61, 63.5),
             y = 4, label = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), size = 5, color = "navy") +
    annotate("text", x = 0.5, y = c(1, 2, 3, 4), label = c("1", "2", "3", "4"), size = 4, color = "gray") +
    ylab("") + xlab("") + labs(title = "Log-10 scale (Max = 4)")


# 出力：「中区分」フォルダを予め用意しておくこと
png(paste0("中区分/", i, "_中区分.png"), width = 1600, height = 800)
print(MidLog)
dev.off()

}



#----------------------
# 大区分
#----------------------


for (i in NU$NationalUniv){

# 大区分:対数スケール
LrgLog <- Lrg$clean %>% 
    
    # データの整理
    bind_rows %>% select(研究機関, LogAmount, LogCount, area) %>%
    rename(採択件数 = LogCount, 総配分額 = LogAmount) %>%
    gather(key = "key", value = "value", -研究機関, -area) %>%
    spread(key = area, value = value, fill = 0) %>%
    gather(key = "area", value = "value", -研究機関, -key) %>%
    mutate(area = as.factor(area)) %>%
    
    # 大学名フィルター
    filter(研究機関 == i) %>%

    # 描画開始
    ggplot(aes(x = area, y = value, group = key)) +
    geom_polygon(aes(color = key), fill = NA, alpha = 0.5) +
    geom_point(aes(color = key), size = 1, alpha = 0.5) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 25)) +
    ylim(0,4) +
    coord_polar() +
    ylab("") + xlab("") + labs(title = "Log-10 scale (Max = 4)") +
    
    # 外周に大区分ラベルつける（なくても良い）
    annotate("segment", x = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5),
             y = 4, yend = 4,
             xend = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5),
             color = c("pink", "yellow", "skyblue", "coral", "darkseagreen1", "plum2", "orange1", "cyan", "greenyellow", "bisque", "lightblue1"),
             size = 2) +
    annotate("text", x = c(1:11),
             y = 4, label = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), size = 5, color = "navy") +
    annotate("text", x = 0.5, y = c(1, 2, 3, 4), label = c("1", "2", "3", "4"), size = 3, color = "gray") 
    
# 出力：「大区分」フォルダをあらかじめ用意しておくこと
png(paste0("大区分/", i, "_大区分.png"), width = 1600, height = 800)
print(LrgLog)
dev.off()

}

