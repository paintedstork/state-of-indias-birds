set.seed(120)
a = read.csv("guess_the_birdmap.csv")
b = sample(a$name[is.na(a$yn)],1)
b
a$yn[a$name == b] = 1
write.csv(a, "guess_the_birdmap.csv", row.names = F)

