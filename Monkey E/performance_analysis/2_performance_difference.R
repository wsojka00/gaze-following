# install.packages('dplyr')
library('dplyr')

dir <- "C:/Users/48786/Downloads"
filename1 <- "eyehead.csv"
filename2 <- "eyeonly.csv"
df_he <- read.csv(file.path(dir, filename1))
df_e <- read.csv(file.path(dir, filename2))

##########################################################################################
names(df_he) <- c("idx", "soa", "perf", "cond")
names(df_e) <- c("idx", "soa", "perf", "cond")

df_he$soa <- factor(df_he$soa)
df_e$soa <- factor(df_e$soa)

df_he$gazeT <- rep(1, dim(df_he)[1], 1)
df_he$gazeT <- gsub("1", "headNeye", df_he$gazeT)

df_e$gazeT <- rep(2, dim(df_e)[1], 1)
df_e$gazeT <- gsub("2", "eyeOnly", df_e$gazeT)

df<- rbind(df_he, df_e)
#soas = perf_eyehead[3]
#perf_congr = perf_eyehead[4]
#perf_incongr = perf_eyehead[5]

IdentifyLims <- function(x){
  uplim <- quantile(x,0.75, na.rm = T)[[1]] + (1.57*IQR(x, na.rm = T))  
  lowlim <- quantile(x,0.25, na.rm = T)[[1]] - (1.57*IQR(x, na.rm = T))
  return(list(lowlim = lowlim, uplim = uplim)) 
}

(df_s <- 
    df %>%
    group_by(gazeT, soa, cond) %>%
    summarise(median = median(perf, na.rm = T),
              lowlim = boxplot.stats(perf)$conf[1],
              uplim = boxplot.stats(perf)$conf[2]))

df_temp <- df_s
for( i in seq(from = 1, to = dim(df_temp)[1], by = 2)){
  df_temp[c(i, i+1), c(4,5,6)] <- df_temp[c(i, i+1), c(4,5,6)]- rep(df_temp[i+1 ,4], 3,1)
}

df_temp[c(17,18), c(4,5,6)] <- NA

################################################################################
ggplot() +
  geom_boxplot(data=df[df$gazeT == 'headNeye',], aes(x = soa, y = perf, fill = cond),
               notch =T)

ggplot()+
   geom_bar(data= df_temp, aes(x=soa, y=median, fill = gazeT), stat="identity", alpha=0.7,
             width = 0.5, position="dodge") +
   geom_errorbar(data = df_temp[df_temp$gazeT == 'headNeye',], 
                 aes(ymin=lowlim, ymax=uplim, x=soa, color = cond), width=.1,
                 position= position_nudge(x = 0.125)) +
  geom_errorbar(data = df_temp[df_temp$gazeT == 'eyeOnly',], 
                aes(ymin=lowlim, ymax=uplim, x=soa, color = cond), width=.1,
                position= position_nudge(x = -0.125)) +
  labs(x = "Stimulus Onset Asynchrony (ms)",
        y = expression(atop(bold("Performance Difference (%)"),
                            paste("(", italic(median)[congruent], " - ", 
                                  italic(median)[incongruent], ")"))),
        color = 'error type') +
 theme(axis.text=element_text(size = 14, color = "black"),
       axis.ticks =  element_line(size = 0.5, color = "black"),
       legend.title = element_blank(),
       legend.position = "top",
       legend.text = element_text(size = 11),
       legend.key.size = unit(0.5, "cm"),
       panel.background = element_blank(),
       panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 'dashed'),
       panel.grid.minor = element_line(color = "grey", size = 0.2, linetype = 'dashed'),
       panel.grid.minor.x = element_blank(),
       panel.grid.major.x = element_blank(),
       axis.title = element_text(hjust = 0.5, size = 16, face = "bold"),
       axis.line = element_line(color = "black", size = 0.3))+
  scale_color_manual(values=c("#72B6A1", "#E99675")) +
  scale_fill_manual(values=c("#72B6A1", "#E99675"), 
                    labels=c("eye only", "head plus eye"))


###############################################################################
path_new <- 'G:/Other computers/My Computer/HIH/M Project/RL Project/Analysis/Analysis Scripts/Behavioral/Statistical analysis/R/Functions/Population analysis/Sfn Poster'
ggexport(figure, filename = file.path(path_new,"tbt_adaptation_poo.png"),
         width=6000,
         height=6000,
         res = 600)