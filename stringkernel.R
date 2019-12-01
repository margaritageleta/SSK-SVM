library(stringr)
alphabet = c(str_split("abcdefghijklmnopqrstuvwxyz ", ""))[[1]]

s <- "cat"
lambda <- 1

## NAIVE IMPLEMENTATION: THE WORST ONE
count.ngrams.with.decay <- function (word, n, decay) {
  sum <- 0
  for (i in n:nchar(word)){
    ngrams <- ngram.generator(alphabet, i)
    #print(ngrams)
    for (pattern in ngrams) {
      #print(pattern)
      if (str_detect(word, pattern)){
        print(paste0(pattern, " detected!"))
        print(paste0("adding ", decay^i))
        sum <- sum + decay^i
      }
    }
  }
  return(sum)
}

count.ngrams.with.decay("cat", 1, 0.5)

kernel <- function (s, t, n, decay) {
  sum <- 0
  for (i in n:min(nchar(s), nchar(t))){
    ngrams <- ngram.generator(alphabet, i)
    #print(ngrams)
    for (pattern in ngrams) {
      #print(pattern)
      aux1 <- aux2 <- 0
      matches.s <- str_count(s, pattern)
      matches.t <- str_count(t, pattern)
      if (matches.s == 0 | matches.t == 0) {
        next
      }
      for (match in 1:matches.s){
        aux1 <- aux1 + decay^i
      }
      for (match in 1:matches.t){
        aux2 <- aux2 + decay^i
      }
      # print(paste0("adding ", aux1 * aux2))
      # print(paste0(pattern, " detected in both strings!"))
      sum <- sum + (aux1 * aux2)
    }
  }
  return(sum)
}

n.kernel <- function (s, t, n, decay) {
  minimal <- ""
  if (nchar(s) < nchar(t)){
    minimal <- t
  } else {
    minimal <- s
  }
  return(kernel(s, t, n, decay) / kernel(minimal, minimal, n, decay))
}

n.kernel("car", "car", 2, 1)

kernel("child", "child", 2, 1)
n.kernel("child", "child", 2, 1)

kernel("child", "childhood", 2, 1)
n.kernel("child", "childhood", 2, 1)

n.kernel("childhood", "children", 2, 1)
n.kernel("childhood", "brotherhood", 2, 1)
n.kernel("childhood", "ghost", 2, 1)
n.kernel("childhood", "loneliness", 2, 1)

n.kernel("science is organized knowledge", "wisdom is organized life", 2, 0.2)

science <-"science is a systematic enterprise that builds and organizes knowledge in the form of testable explanations and predictions about the universe"
scifi <- "science fiction is a genre of speculative fiction that typically deals with imaginative and futuristic concepts such as advanced science and technology space exploration time travel parallel universes and extraterrestrial life"
religion<-"religion is a system of designated behaviors and practices morals worldviews texts sanctified places prophecies, ethics or organizations that relates humanity to supernatural transcendental or spiritual elements"

n.kernel(science, science, 4, 1)
n.kernel(science, scifi, 4, 1)
n.kernel(science, religion, 4, 1)
n.kernel(religion, scifi, 4, 1)

ngram.generator <- function (alphabet, n) {
  if (n == 1) {
    return (alphabet)
  }
  ngrams <- c()
  for (letter1 in alphabet) {
    for (letter2 in alphabet) {
      ngram <- ""
      ngram <- paste0(ngram, letter1)
      while (nchar(ngram) + 1 != n) {
        ngram <- paste0(ngram, ".")
      }
      ngram <- paste0(ngram, letter2)
      ngrams <- c(ngrams, ngram)
      
    }
  }
  return (ngrams)
}

ngram.generator(alphabet, 1)

######

# RECURSIVE VERSION
k_prim <- function (s, t, i, lbda) {
  if (i == 0){
    return (1)
  }
  if (min(nchar(s), nchar(t)) < i){
    return (0)
  }
  x = substr(s, nchar(s), nchar(s))
  s_ = substr(s, 1, nchar(s) - 1)
  sum <- 0
  
  for (j in 1:nchar(t)) {
    #print(paste0("CALL: ",CALL,", t[j]: ",substr(t, j, j), " x: ", x, " t[:j]: ", substr(t, 1, j-1)))
    #print(paste0(nchar(t), j,"exponent: ", nchar(t) - j + 2, " total: ", lbda^(nchar(t) - j + 2)))
    if (substr(t, j, j) == x) {
     # CALL <<- CALL + 1
      sum <- sum + k_prim(s_, substr(t, 1, j-1), i - 1, lbda) * (lbda^(nchar(t) - j + 2))
    }
  }
  toret = lbda * k_prim(s_, t, i, lbda) + sum
  return (toret)
}

# Kernel defined by Lodhi et al. (2002)
ssk <- function(s, t, n, lbda) {
  if (n <= 0){
    print("Error, n must be bigger than zero")
  }
  
  if (min(nchar(s), nchar(t)) < n){
    return (0)
  }
  
  x = substr(s, nchar(s), nchar(s))
  s_ = substr(s, 1, nchar(s) - 1)
  sum <- 0
  for (j in 1:nchar(t)) {
    #print(paste0("CALL: ",CALL,", t[j]: ",substr(t, j, j), " x: ", x, " t[:j]: ", substr(t, 1, j -1)))
    if (substr(t, j, j) == x) {
      #CALL <<- CALL + 1
      sum <- sum + k_prim(s_, substr(t, 1, j-1), n - 1, lbda) * lbda^2 
    }
  }
  toret = ssk(s_, t, n, lbda) + sum
  return (toret)
}

CALL <- 0
## Testing time (R ~ Python comparison)
microbenchmark::microbenchmark(ssk("science is organized knowledge", "wisdom is organized life", n = 2, lbda = 0.5))
ssk(science, science, n = 2, lbda = 0.5)
ssk(science, scifi, n = 2, lbda = 0.5)
ssk(science, religion, n = 2, lbda = 0.5)
ssk(scifi, religion, n = 2, lbda = 0.5)
ssk(scifi, scifi, n = 2, lbda = 0.5)
