### CHANGES IN text.alignment VERSION 0.1.2

- Added smith_waterman_misaligned to extract the information of what is misaligned
- Allow to pass arguments to the tokenizer

### CHANGES IN text.alignment VERSION 0.1.1

- Fix mismatch between R and Rcpp version of smith_waterman in case of an alignment with 1 letter/word only
- Also return similarity score to a and b individually instead of only to the shortest string. These 2 measurements are now also returned by as.data.frame.smith_waterman
- Added smith_waterman_pairwise

### CHANGES IN text.alignment VERSION 0.1.0

- Initial version
