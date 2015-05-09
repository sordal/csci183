music.all[is.na(music.all)] <- 0
music.all
musictest.frame <- prcomp(music.all[4:73], retx=TRUE, scale=TRUE)
plot(musictest.frame$sdev)
ggplot2(musictest.frame$x)

a <- data.frame(musictest.frame$x)
plot(a$PC1,a$PC2)
qplot(a$PC1,a$PC2)
p <- ggplot(a, aes(x=PC1, y=PC2, label=music.all$artist))

p + geom_text()
