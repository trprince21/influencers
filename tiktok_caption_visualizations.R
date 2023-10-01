# load the tidyverse package for using tibble
library(tidyverse)
library(dplyr)

# read in the CSV files and convert them to tibbles
most_popular_hashtags <- 
  read.csv("dataset_tiktok-scrape_50_most_popular_hastags.csv") %>% 
  as_tibble()

beauty_related_hashtags <- 
  read.csv("dataset_tiktok-scraper-task_beauty_hashtags.csv") %>%
  as_tibble()



count_non_empty <- function(row) {
  sum(row != "")
}

library(stringr)
colnames(most_popular_hashtags) <- str_remove(colnames(most_popular_hashtags), "authorMeta.")

most_popular_hashtags <- most_popular_hashtags %>% 
  select(diggCount, shareCount, playCount, commentCount,
         name, nickName, signature, verified, ttSeller, following, fans, heart, video,
         createTimeISO, isAd, isMuted, videoMeta.duration, text, webVideoUrl,
         paste0("mentions.", 0:51), paste0("hashtags.", 0:252, ".name")) %>%
  rowwise() %>% 
  mutate(num_of_mentions = count_non_empty(c_across(paste0("mentions.", 0:51))),
         num_of_hashtags = count_non_empty(c_across(paste0("hashtags.", 0:252, ".name"))))


# diggCount
# shareCount
# playCount
# commentCount

# name
# nickName
# signature
# verified
# ttSeller  (= tiktok seller?)
# following
# fans
# heart
# video

# createTimeISO
# isAd
# isMuted
# videoMeta.duration  (in seconds)
# text
# webVideoUrl

# [count mentions.0 to mentions.51]
# [count hashtags.0.name to hashtags.252.name] 
## should I add any musicMeta for info on music?



most_popular_hashtags <- most_popular_hashtags %>% 
  select(diggCount, shareCount, playCount, commentCount,
         name, nickName, signature, verified, ttSeller, following, fans, heart, video,
         createTimeISO, isAd, isMuted, videoMeta.duration, text, webVideoUrl,
         num_of_mentions, num_of_hashtags)


hashtag_list <- c("#tiktok(?![:lower:])", "#love(?![:lower:])", "#like(?![:lower:])",
                  "#follow(?![:lower:])", "#explore", "#memes", "#video", 
                  "#followforfollowback", "#duet", "#repost", "#tiktokchallenge", 
                  "#new", "#tiktokfamous", "#tiktoktrend", "#viralvideos", 
                  "#viralpost", "#slowmo", "#behindthescenes", "#dadsoftiktok",
                  "#momsoftiktok", "#family", "#reallifeathome", 
                  "#tiktokmademebuyit", "#mexico", "#challenge", 
                  "#youtube(?![:lower:])", "#youtuber", "#artistsoftiktok",
                  "#foryoupage", "#fyp", "#foryou(?![:lower:])", 
                  "#viral(?![:lower:])", "#funny", "#followme", 
                  "#cute(?![:lower:])", "#fun(?![:lower:])", "#music", "#happy",
                  "#fashion", "#comedy", "#bestvideo", "#tiktok4fun", 
                  "#thisis4u", "#loveyoutiktok", "#cutebaby", "#cutegirl", 
                  "#cuteness", "#cuteboy", "#style", "#work")

results <- data.frame()
for (hashtag in hashtag_list) {
  df <- as.data.frame(most_popular_hashtags %>% filter(str_detect(text, hashtag)))
  new_row <- data.frame(likes = mean(df$diggCount), shares = mean(df$shareCount),
                        plays = mean(df$playCount), comments = mean(df$commentCount))
  results <- rbind(results, new_row)
}

revised_hashtag_list <- c("#tiktok", "#love", "#like", "#follow", "#explore", 
                          "#memes", "#video", "#followforfollowback", "#duet", 
                          "#repost", "#tiktokchallenge", "#new", "#tiktokfamous", 
                          "#tiktoktrend", "#viralvideos", "#viralpost", "#slowmo", 
                          "#behindthescenes", "#dadsoftiktok", "#momsoftiktok", 
                          "#family", "#reallifeathome", "#tiktokmademebuyit", 
                          "#mexico", "#challenge", "#youtube", "#youtuber", 
                          "#artistsoftiktok", "#foryoupage", "#fyp", "#foryou", 
                          "#viral", "#funny", "#followme", "#cute", "#fun", 
                          "#music", "#happy", "#fashion", "#comedy", 
                          "#bestvideo", "#tiktok4fun", "#thisis4u", 
                          "#loveyoutiktok", "#cutebaby", "#cutegirl", 
                          "#cuteness", "#cuteboy", "#style", "#work")

results <- as_tibble(data.frame(hashtags = revised_hashtag_list, results)) %>% 
  arrange(desc(likes))

results <- results %>% 
  mutate(plays = plays / 10000,
         likes = likes / 10000,
         shares = shares / 10000,
         comments = comments / 10000)

print(results, n = 50)

library(tidyr)
library(ggplot2)

results_long <- results %>%
  pivot_longer(cols = c(plays, likes, shares, comments), names_to = "engagement_type", values_to = "value")

ggplot(results_long, aes(x = reorder(hashtags, value), y = value, fill = engagement_type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C")) +
  ylab("Average Engagement (in terms of 10,000)") +
  xlab("Hashtags") +
  ggtitle("Engagement Distribution for the 50 Most Popular Hashtags on TikTok") +
  labs(fill = "Engagement Type") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank())   





# SCATTERPLOT (performing logarithmic regression):
library(ggplot2)
library(broom)

# Convert data to a data frame
duration_vs_playCount_df <- as.data.frame(most_popular_hashtags %>%
                                            select(videoMeta.duration, playCount))

# Remove rows with videoMeta.duration = 0
duration_vs_playCount_df <- duration_vs_playCount_df[duration_vs_playCount_df$videoMeta.duration != 0, ]

# Fit a logarithmic regression model
model <- lm(playCount ~ log(videoMeta.duration), 
            data = duration_vs_playCount_df)

# Calculate predictions and standard errors
predictions <- predict(model, newdata = duration_vs_playCount_df, se.fit = TRUE)
duration_vs_playCount_df$pred <- predictions$fit
duration_vs_playCount_df$se <- predictions$se.fit

# Create scatterplot with error bars
ggplot(duration_vs_playCount_df, aes(x = videoMeta.duration, y = playCount)) +
  geom_point(color = "black", size = 0.8) +
  xlab("Duration (in seconds)") +
  ylab("Play Count") +
  ggtitle("Relationship Between Duration and Play Count of TikTok Videos") +
  theme_minimal()

# Summary of the model
summary(model)

# Add regression line to the plot
ggplot(duration_vs_playCount_df, aes(x = videoMeta.duration, y = playCount)) +
  geom_point(color = "black", size = 0.5) +
  geom_line(aes(y = pred), color = rgb(255/255, 0/255, 80/255), size = 0.8) +
  geom_errorbar(aes(ymin = pred - se, ymax = pred + se), width = 0.001, 
                color = rgb(0/255, 242/255, 234/255)) +
  xlim(0, 600) +
  ylim(0, 500000000) +
  xlab("Duration (in seconds)") +
  ylab("Play Count") +
  ggtitle("Relationship Between Duration and Play Count of TikTok Videos") +
  theme_minimal()




ggplot(duration_vs_playCount_df, aes(x = videoMeta.duration, y = playCount)) +
  geom_point(color = "black", size = 0.8) +
  geom_errorbar(aes(ymin = playCount - se, ymax = playCount + se), width = 0.0001, 
                color = rgb(0/255, 242/255, 234/255)) +
  xlim(0, 200) +
  ylim(17500000, 22500000) +
  xlab("Duration (in seconds)") +
  ylab("Play Count") +
  ggtitle("Relationship Between Duration and Play Count of TikTok Videos") +
  theme_minimal()


ggplot(duration_vs_playCount_df, aes(x = videoMeta.duration, y = playCount)) +
  geom_point(color = "black", size = 0.5) +
  geom_errorbar(aes(ymin = pred - se, ymax = pred + se), width = 0.0001, 
                color = rgb(0/255, 242/255, 234/255)) +
  geom_line(aes(y = pred), color = rgb(255/255, 0/255, 80/255), size = 0.6) +
  xlim(0, 200) +
  ylim(17500000, 22500000) +
  xlab("Duration (in seconds)") +
  ylab("Play Count") +
  ggtitle("Relationship Between Duration and Play Count of TikTok Videos") +
  theme_minimal()










# SCATTERPLOT (performing logarithmic regression):
library(ggplot2)
library(broom)

# Convert data to a data frame
duration_vs_diggCount_df <- as.data.frame(most_popular_hashtags %>%
                                            select(videoMeta.duration, diggCount))

# Remove rows with videoMeta.duration = 0
duration_vs_diggCount_df <- duration_vs_diggCount_df[duration_vs_diggCount_df$videoMeta.duration != 0, ]

# Fit a logarithmic regression model
model <- lm(diggCount ~ log(videoMeta.duration), 
            data = duration_vs_diggCount_df)

# Calculate predictions and standard errors
predictions <- predict(model, newdata = duration_vs_diggCount_df, se.fit = TRUE)
duration_vs_diggCount_df$pred <- predictions$fit
duration_vs_diggCount_df$se <- predictions$se.fit

# Create scatterplot with error bars
ggplot(duration_vs_diggCount_df, aes(x = videoMeta.duration, y = diggCount)) +
  geom_point(color = "black", size = 0.8) +
  geom_errorbar(aes(ymin = diggCount - se, ymax = diggCount + se), width = 0.001, 
                color = rgb(0/255, 242/255, 234/255)) + 
  xlim(0, 200) +
  ylim(0, 20000000) +
  xlab("Duration (in seconds)") +
  ylab("Like Count") +
  ggtitle("Relationship Between Duration and Like Count of TikTok Videos") +
  theme_minimal()

# Summary of the model
summary(model)

# Add regression line to the plot
ggplot(duration_vs_diggCount_df, aes(x = videoMeta.duration, y = diggCount)) +
  geom_point(color = "black", size = 0.5, alpha = 0.2) +
  geom_line(aes(y = pred), color = rgb(255/255, 0/255, 80/255), size = 0.8) +
  geom_errorbar(aes(ymin = pred - se, ymax = pred + se), width = 0.001, 
                color = rgb(0/255, 242/255, 234/255)) +
  xlim(0, 200) +
  ylim(0, 20000000) +
  xlab("Duration (in seconds)") +
  ylab("Like Count") +
  ggtitle("Relationship Between Duration and Like Count of TikTok Videos") +
  theme_minimal()




ggplot(duration_vs_diggCount_df, aes(x = videoMeta.duration, y = diggCount)) +
  geom_point(color = "black", size = 0.8) +
  geom_errorbar(aes(ymin = diggCount - se, ymax = diggCount + se), width = 0.0001, 
                color = rgb(0/255, 242/255, 234/255)) +
  xlim(0, 200) +
  ylim(0, 20000000) +
  xlab("Duration (in seconds)") +
  ylab("Like Count") +
  ggtitle("Relationship Between Duration and Like Count of TikTok Videos") +
  theme_minimal()


ggplot(duration_vs_diggCount_df, aes(x = videoMeta.duration, y = diggCount)) +
  geom_point(color = "black", size = 0.5) +
  geom_errorbar(aes(ymin = pred - se, ymax = pred + se), width = 0.0001, 
                color = rgb(0/255, 242/255, 234/255)) +
  geom_line(aes(y = pred), color = rgb(255/255, 0/255, 80/255), size = 0.6) +
  xlim(0, 200) +
  ylim(0, 20000000) +
  xlab("Duration (in seconds)") +
  ylab("Like Count") +
  ggtitle("Relationship Between Duration and Like Count of TikTok Videos") +
  theme_minimal()













colnames(beauty_related_hashtags) <- str_remove(colnames(beauty_related_hashtags), "authorMeta.")

beauty_related_hashtags <- beauty_related_hashtags %>% 
  select(diggCount, shareCount, playCount, commentCount,
         name, nickName, signature, verified, ttSeller, following, fans, heart, video,
         createTimeISO, isAd, isMuted, videoMeta.duration, text, webVideoUrl,
         paste0("mentions.", 0:52), paste0("hashtags.", 0:333, ".name")) %>%
  rowwise() %>% 
  mutate(num_of_mentions = count_non_empty(c_across(paste0("mentions.", 0:51))),
         num_of_hashtags = count_non_empty(c_across(paste0("hashtags.", 0:252, ".name"))))



beauty_hashtag_list <- c("#beautyls", "#beautyhacks", "#beautytips", "#beautyfull", 
                         "#unlockbeauty", "#sleepingbeauty", "#naturalbeauty", 
                         "#beautyofnature", "#beautytt", "#beautyblogger", 
                         "#beauty4charity", "#beautybeast", "#beautychallenge",
                         "#homebeautyhacks", "#danceforbeauty", "#showyourbeauty",
                         "#nofilter", "#makeuptutorial", "#makeup(?![:lower:])",
                         "#makeupartists", "#model", "#beautiful", "#ootd")


beauty_results <- data.frame()
for (hashtag in beauty_hashtag_list) {
  df <- as.data.frame(beauty_related_hashtags %>% filter(str_detect(text, hashtag)))
  new_row <- data.frame(likes = mean(df$diggCount), shares = mean(df$shareCount),
                        plays = mean(df$playCount), comments = mean(df$commentCount))
  beauty_results <- rbind(beauty_results, new_row)
}

revised_beauty_hashtag_list <- c("#beautyls", "#beautyhacks", "#beautytips", "#beautyfull", 
                                 "#unlockbeauty", "#sleepingbeauty", "#naturalbeauty", 
                                 "#beautyofnature", "#beautytt", "#beautyblogger", 
                                 "#beauty4charity", "#beautybeast", "#beautychallenge",
                                 "#homebeautyhacks", "#danceforbeauty", "#showyourbeauty",
                                 "#nofilter", "#makeuptutorial", "#makeup",
                                 "#makeupartists", "#model", "#beautiful", "#ootd")


beauty_results <- as_tibble(data.frame(hashtags = revised_beauty_hashtag_list, beauty_results)) %>% 
  arrange(desc(likes))

beauty_results <- beauty_results %>% 
  mutate(plays = plays / 10000,
         likes = likes / 10000,
         shares = shares / 10000,
         comments = comments / 10000)



library(tidyr)
library(ggplot2)

beauty_results_long <- beauty_results %>%
  pivot_longer(cols = c(plays, likes, shares, comments), names_to = "engagement_type", values_to = "value")

ggplot(beauty_results_long, aes(x = reorder(hashtags, value), y = value, fill = engagement_type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#1F78B4", "#A6CEE3", "#B2DF8A", "#33A02C")) +
  ylab("Average Engagement (in terms of 10,000)") +
  xlab("Hashtags") +
  ggtitle("Engagement Distribution for Beauty-Related Hashtags on TikTok") +
  labs(fill = "Engagement Type") +
  coord_flip() +
  theme_minimal()






# SCATTERPLOT:

num_of_hashtags_vs_playCount_df <- as.data.frame(most_popular_hashtags %>%
                                                   select(num_of_hashtags, playCount))

library(ggplot2)
require(graphics)

plot(num_of_hashtags_vs_playCount_df$num_of_hashtags, 
     num_of_hashtags_vs_playCount_df$playCount,
     xlim = c(0,80), ylim = c(0,300000000),
     xlab = "# of hashtags", ylab = "plays", 
     pch = 20, cex = 0.5)

md2 <- lm(playCount ~ poly(num_of_hashtags,2, raw = TRUE), 
          data = num_of_hashtags_vs_playCount_df)

xv = num_of_hashtags_vs_playCount_df$num_of_hashtags

yv = predict(md2, data.frame(num_of_hashtags = xv), type = "response")

lines(xv,yv,col = "blue", lwd = 0.015)













num_of_hashtags_vs_playCount_df <- as.data.frame(most_popular_hashtags %>%
                                                   select(num_of_hashtags, playCount))

library(ggplot2)
library(minpack.lm)

plot(num_of_hashtags_vs_playCount_df$num_of_hashtags, 
     num_of_hashtags_vs_playCount_df$playCount,
     xlim = c(0,80), ylim = c(0,300000000),
     xlab = "# of hashtags", ylab = "plays", 
     pch = 20, cex = 0.5)

exp.mod <- nlsLM(playCount ~ a*(1-exp(-b * num_of_hashtags)),
                 data = num_of_hashtags_vs_playCount_df,
                 start = list(a = max(num_of_hashtags_vs_playCount_df$playCount), 
                              b = 1))

xv = num_of_hashtags_vs_playCount_df$num_of_hashtags

yv = predict(exp.mod, data.frame(num_of_hashtags=xv))

lines(xv,yv,col = "blue", lwd = 2)




ggplot(na.omit(num_of_hashtags_vs_playCount_df), aes(x=num_of_hashtags, y=playCount)) +
  geom_point() +
  geom_smooth(method="gam", 
              formula = num_of_hashtags_vs_playCount_df$playCount ~ log(num_of_hashtags_vs_playCount_df$num_of_hashtags), 
              se=FALSE, color="blue")




