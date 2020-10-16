set.seed(45)
df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
                 variable=rep(paste0("category", 1:9), each=5))
# plot
ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=variable))

# plot
df2 <- as.data.frame(all_loc)
df_all <- filter(df2, Group.2=="All causes")

df3 <- tibble(x=df_all[,"Group.3"], val=df_all[,"x"], variable=df_all[,"Group.1"])

ggplot(data = df_all, aes(x=Group.3, y=x)) + geom_line(aes(colour=Group.1))

## need to remake plots as ggplot & dataframe
