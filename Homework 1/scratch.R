# 1

nNGH <- c(2, 32, 269)
pNGHA <- (2 + 32/2)/sum(nNGH)
pNGHa <- (269 + 32/2)/sum(nNGH)
eNGH <- c(pNGHA^2, pNGHa*pNGHA*2, pNGHa^2)
chisq.test(nNGH, p=eNGH)

nGI <- c(112, 74, 17)
pGIA <- (112 + 74/2)/sum(nGI)
pGIa <- (17 + 74/2)/sum(nGI)
eGI <- c(pGIA^2, pGIA*pGIa*2, pGIa^2)
chisq.test(nGI, p=eGI)

nBoth <- nNGH + nGI
pBothA <- (nBoth[1] + nBoth[2]/2)/sum(nBoth)
pBotha <- (nBoth[3] + nBoth[2]/2)/sum(nBoth)
eBoth <- c(pBothA^2, pBothA*pBotha*2, pBotha^2)
chisq.test(nBoth, p=eBoth)


#

countPairwiseDifferences <- function(sequence1, sequence2) {
  str1array = strsplit(sequence1, "")[[1]]
  str2array = strsplit(sequence2, "")[[1]]
  count = 0
  for (i in 1:length(str1array)) {
    if (str1array[i] != str2array[i]) {
      count = count + 1
    }
  }
  return(count)
}

sequences = c("AAAAAG", "CTAGAG", "AACATG", "AACATG", "CTAGAT")
N_pairs = 1200 * choose(5,2)
totalPairwiseDifferences <- 0
sequenceCombinations <- combn(sequences, 2)
for (i in 1:dim(sequenceCombinations)[2]) { 
  totalPairwiseDifferences = totalPairwiseDifferences + 
    countPairwiseDifferences(sequenceCombinations[1,i], sequenceCombinations[2,i])
}

totalPairwiseDifferences / N_pairs


sequences = c("ATAATG", "AAAGAG", "CACAAG", "ATCATG", "CAAGAT")
nSites = 1200
totalPairwiseDifferences <- 0
sequenceCombinations <- combn(sequences, 2)
for (i in 1:dim(sequenceCombinations)[2]) { 
  totalPairwiseDifferences = totalPairwiseDifferences + 
    countPairwiseDifferences(sequenceCombinations[1,i], sequenceCombinations[2,i])
}

totalPairwiseDifferences / nSites / 5 / 5



# seq1 = "AAAAAG"
# seq2 = "CTAGAG"
# seq3 = "AACATG"
# seq4 = "AACATG"
# seq5 = "CTAGAT"
# 
# 
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq1, seq2)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq1, seq3)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq1, seq4)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq1, seq5)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq2, seq3)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq2, seq4)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq2, seq5)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq3, seq4)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq3, seq5)
# totalPairwiseDifferences <- totalPairwiseDifferences + count_pairwise_differences(seq4, seq5)
# 
# totalPairwiseDifferences / N_pairs
# 
# 
# 
# count_pairwise_differences(seq1, seq2)
# count_pairwise_differences(seq1, seq3)
# count_pairwise_differences(seq1, seq4)
# count_pairwise_differences(seq1, seq5)
# count_pairwise_differences(seq2, seq3)
# count_pairwise_differences(seq2, seq4)
# count_pairwise_differences(seq2, seq5)
# count_pairwise_differences(seq3, seq4)
# count_pairwise_differences(seq3, seq5)
# count_pairwise_differences(seq4, seq5)

harmonic.sum <- function(n) {
  hsum = 0
  for (i in 1:n) {
    hsum = hsum + 1/i
  }
  return(hsum)
}
