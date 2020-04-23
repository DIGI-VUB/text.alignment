#' @title Tokenise text into a sequence of characters
#' @description Tokenise text into a sequence of characters
#' @param x a character string of length 1
#' @return a character vector with the sequence of characters in \code{x}
#' @seealso \code{\link{strsplit}}
#' @export
#' @examples
#' tokenize_letters("This function just chunks up text in letters\nOK?")
#' tokenize_letters("Joske  Vermeulen")
tokenize_letters <- function(x){
  if(length(x) != 1){
    stop("requires x to be of length 1")
  }
  x <- strsplit(x, "")
  x <- unlist(x, recursive = FALSE) 
  x
}


#' @title Tokenise text into a sequence of words
#' @description Tokenise text into a sequence of words. The function uses \code{\link{strsplit}} to split text into words
#' by using the [:space:] and [:punct:] character classes.
#' @param x a character string of length 1
#' @return a character vector with the sequence of words in \code{x}
#' @seealso \code{\link{strsplit}}
#' @export
#' @examples
#' tokenize_spaces_punct("This just splits. Text.alongside\nspaces right?")
#' tokenize_spaces_punct("Also .. multiple punctuations or ??marks")
#' tokenize_spaces_punct("Joske  Vermeulen")
tokenize_spaces_punct <- function(x){
  if(length(x) != 1){
    stop("requires x to be of length 1")
  }
  x <- strsplit(x, split = "[[:space:][:punct:]]+")
  x <- unlist(x, recursive = FALSE) 
  x
}




#' @title Align text using Smith-Waterman
#' @description Align text using the Smith-Waterman algorithm. 
#' The Smith–Waterman algorithm performs local sequence alignment. 
#' It finds similar regions between two strings.\cr
#' Similar regions are a sequence of either characters or words which are found by matching the characters or words of 2 sequences of strings.\cr 
#' If the word/letter is the same in each text, the alignment score is increased with the match score, while if they are not the same the local alignment score drops by the gap score.
#' If one of the 2 texts contains extra words/letters, the score drops by the mismatch score. 
#' @param a a character string of length one
#' @param b a character string of length one
#' @param type either 'characters' or 'words' indicating to align based on a sequence of characters or a sequence of words. Defaults to 'characters'.
#' @param match integer value of a score to assign a match (a letter/word from a and b which are the same during alignment). This value should be bigger than zero. Defaults to 2.
#' @param mismatch integer value of a score to assign a mismatch (leave out 1 word / 1 letter from 1 of the 2 input strings during alignment). This value should be smaller or equal to zero.  Defaults to -1.
#' @param gap integer value of a score to assign a gap (drop 1 word / letter from each of the 2 input strings during alignment). This value should be smaller or equal to zero.   Defaults to -1.
#' @param lower logical indicating to lowercase text before doing the alignment. Defaults to TRUE.
#' @param tokenizer a function to tokenise text into either a sequence of characters or a sequence of words.
#' Defaults to \code{\link{tokenize_letters}} if type is \code{'characters'} and \code{\link{tokenize_spaces_punct}} if type is \code{'words'}
#' @param similarity optionally, a function to compare 2 characters or words. 
#' This function should have 2 arguments x and y with the 2 letters / words to compare and should return 1 number indicating
#' the similarity between x and y. See the examples.
#' @param collapse separator used to combined characters / words back together in the output. Defaults to '' for type 'characters' and a space for type 'words'
#' @param edit_mark separator to indicated a gap/mismatch between sequences. Defaults to the hashtag symbol.
#' @param implementation either 'R' or 'Rcpp' indicating to perform the alignment in Rcpp or with plain R code. Defaults to 'R'.
#' @seealso \url{https://en.wikipedia.org/wiki/Smith-Waterman_algorithm}
#' @details The code uses similar code as the \code{textreuse::local_align} function and also allows to align character sequences next to aligning word sequences
#' @return an object of class smith_waterman which is a list with elements
#' \itemize{
#'  \item{type: }{The alignment \code{type}}
#'  \item{sw: }{The Smith-Waterman local alignment score}
#'  \item{similarity: }{Score between 0 and 1, calculated as the Smith-Waterman local alignment score / (the number of letters/words in the shortest text times the match weight)}
#'  \item{weights: }{The list of weights provided to the function: match, mismatch and gap}
#'  \item{matches: }{The number of matches found during alignment}
#'  \item{mismatches: }{The number of mismatches found during alignment}
#'  \item{a: }{A list with alignment information from the text provided in \code{a}. The list elements documented below}
#'  \item{b: }{A list with alignment information from the text provided in \code{b}. The list elements documented below}
#' }
#' Elements \code{a} and \code{b} are both lists which contain
#' \itemize{
#'  \item{text: }{The provided character string of either a or b}
#'  \item{tokens: }{A character vector of the tokenised texts of a or b}
#'  \item{n: }{The length of \code{tokens}}
#'  \item{similarity: }{The similarity to a calculated as the Smith-Waterman local alignment score / (the number of letters/words in the a or b text times the match weight)}
#'  \item{alignment: }{A list with the following elements}
#'    \itemize{
#'    \item{text: }{The aligned text from either a or b where gaps/mismatches are filled up with the \code{edit_mark} symbol}
#'    \item{tokens: }{The character vector of tokens which form the aligned \code{text}}
#'    \item{n: }{The length of the aligned \code{text}}
#'    \item{gaps: }{The number of gaps during alignment}
#'    \item{from: }{The starting position in the full tokenised \code{tokens} element from either a or b where the aligned text is found. See the example.}
#'    \item{to: }{The end position in the full tokenised \code{tokens} element from either a or b where the aligned text is found. See the example.}
#'   } 
#' } 
#' @export
#' @examples
#' ## align sequence of letters
#' smith_waterman("Joske Vermeulen", "Jiske Vermoelen")
#' smith_waterman("Joske Vermeulen", "Ik zoek naar Jiske Versmoelen, waar is die te vinden")
#' smith_waterman("Joske", "Jiske")
#' smith_waterman("Joske", "Jiske",
#'                similarity = function(x, y) ifelse(x == y | (x == "o" & y == "i"), 2L, -1L))
#' 
#' ## align sequence of words
#' a <- "The answer is blowin' in the wind."
#' b <- "As the Bob Dylan song says, the answer is blowing in the wind."
#' smith_waterman(a, b)
#' smith_waterman(a, b, type = "characters")
#' smith_waterman(a, b, type = "words")
#' smith_waterman(a, b, type = "words", similarity = function(x, y) adist(x, y))
#' smith_waterman(a, b, type = "words", 
#'                tokenizer = function(x) unlist(strsplit(x, "[[:space:]]")))
#' x <- smith_waterman(a, b, type = "words")
#' x$b$tokens[x$b$alignment$from:x$b$alignment$to]            
#'                
#' # examples on aligning text files
#' a <- system.file(package = "text.alignment", "extdata", "example1.txt")
#' a <- readLines(a)
#' a <- paste(a, collapse = "\n")
#' b <- system.file(package = "text.alignment", "extdata", "example2.txt")
#' b <- readLines(b)
#' b <- paste(b, collapse = "\n")
#' smith_waterman(a, b, type = "characters")
#' smith_waterman(a, b, type = "words")
#' smith_waterman("Gistel Hof", b, type = "characters")
#' smith_waterman("Bailiestraat", b, type = "characters")
#' smith_waterman("Lange rei", b, type = "characters")
#' 
#' # examples on extracting where elements were found
#' x <- smith_waterman("Lange rei", b)
#' x$b$tokens[x$b$alignment$from:x$b$alignment$to]
#' as.data.frame(x)
#' 
#' x <- lapply(c("Lange rei", "Gistel Hof", NA, "Test"), FUN = function(a){
#'   x <- smith_waterman(a, b)
#'   x <- as.data.frame(x)
#'   x
#' })
#' x <- do.call(rbind, x)
#' x
smith_waterman <- function(a, b, 
                           type = c("characters", "words"), 
                           match = 2L, mismatch = -1L, gap = -1L, 
                           lower = TRUE,
                           similarity = function(x, y) ifelse(x == y, 2L, -1L),
                           tokenizer,
                           collapse,
                           edit_mark = "#", 
                           implementation = c("R", "Rcpp")) {
  rcpp <- match.arg(implementation) == "Rcpp"
  type <- match.arg(type)
  match    <- as.integer(match)
  mismatch <- as.integer(mismatch)
  gap      <- as.integer(gap)
  if(missing(collapse)){
    collapse <- switch(type, characters = "", words = " ")
  }
  if(missing(tokenizer)){
    tokenizer <- switch(type, characters = tokenize_letters, words = tokenize_spaces_punct)
  }
  stopifnot(match >= 0)
  stopifnot(mismatch <= 0)
  stopifnot(gap <= 0)
  stopifnot(is.character(a) && length(a) == 1)
  stopifnot(is.character(b) && length(b) == 1)
  
  # Tokenise into letters or words
  original_a <- tokenizer(a)
  original_b <- tokenizer(b)
  
  # Lowercasing to standardise
  if(lower){
    standardised_a <- tolower(original_a)
    standardised_b <- tolower(original_b)  
  }else{
    standardised_a <- original_a
    standardised_b <- original_b
  }
  
  # Get Swith Waterman scoring matrix with local alignment scores
  if(missing(similarity)){
    m <- matrix(0L, length(standardised_b) + 1, length(standardised_a) + 1)
    m <- smith_waterman_matrix(a = standardised_a, b = standardised_b, score_match = match, score_mismatch = mismatch, score_gap = gap, x = m)  
  }else{
    m <- smith_waterman_function(a = standardised_a, b = standardised_b, score_gap = gap, similarity = similarity)  
  }
  
  # Find the starting place in the matrix at the maximum alignment score
  alignment_score <- max(m)
  max_match <- which(m == alignment_score, arr.ind = TRUE, useNames = FALSE)
  
  # Initialize counters for the matrix and the output vector
  row_i <- max_match[1, 1]
  col_i <- max_match[1, 2]
  
  if(row_i == 1 || col_i == 1){
    alignment <- list(type = type, 
                      weights = list(match = match, mismatch = mismatch, gap = gap),
                      sw = 0,
                      similarity = 0,
                      matches = 0L,
                      mismatches = 0L,
                      a = list(text = a,
                               tokens = original_a,
                               n = length(standardised_a),
                               similarity = 0,
                               alignment = list(text = character(),
                                                tokens = character(),
                                                n = 0L,
                                                gaps = 0L,
                                                from = integer(),
                                                to = integer())),
                      b = list(text = b,
                               tokens = original_b,
                               n = length(standardised_b),
                               similarity = 0,
                               alignment = list(text = character(),
                                                tokens = character(),
                                                n = 0L,
                                                gaps = 0L,
                                                from = integer(),
                                                to = integer())))
    class(alignment) <- c("smith_waterman")
    return(alignment)
  }
  # Follow the path starting from the selected position in row_i and col_i to the top left
  if(rcpp){
    if(typeof(m) == "integer"){
      path <- smith_waterman_path_integer(m = m, original_a = original_a, original_b = original_b, 
                                  row_i = row_i, col_i = col_i, 
                                  edit_mark = edit_mark)  
    }else{
      path <- smith_waterman_path(m = m, original_a = original_a, original_b = original_b, 
                                  row_i = row_i, col_i = col_i, 
                                  edit_mark = edit_mark)
    }
    
  }else{
    path <- alignment_path(m = m, original_a = original_a, original_b = original_b, 
                           row_i = row_i, col_i = col_i, max_match = max_match,
                           edit_mark = edit_mark)
  }
  # fix issue for alignment of 1 letter only
  if(length(path$b$sequence) == 1) 
    path$b$from <- path$b$to 
  if(length(path$a$sequence) == 1) 
    path$a$from <- path$a$to
  
  # similarity: alignment score / score in case of perfect match (note does not work when user provides his own similarity function)
  similarity <- alignment_score / (min(length(standardised_a), length(standardised_b)) * match)
  # Construct the output structure
  alignment <- list(type = type, 
                    weights = list(match = match, mismatch = mismatch, gap = gap),
                    sw = alignment_score,
                    similarity = similarity,
                    matches = path$matches,
                    mismatches = path$mismatches,
                    a = list(text = a,
                             tokens = original_a,
                             n = length(standardised_a),
                             similarity = alignment_score / (length(standardised_a) * match),
                             alignment = list(text = paste(path$a$sequence, collapse = collapse),
                                              tokens = path$a$sequence,
                                              n = length(path$a$sequence),
                                              gaps = path$a$gaps,
                                              from = path$a$from,
                                              to = path$a$to)),
                    b = list(text = b,
                             tokens = original_b,
                             n = length(standardised_b),
                             similarity = alignment_score / (length(standardised_b) * match),
                             alignment = list(text = paste(path$b$sequence, collapse = collapse),
                                              tokens = path$b$sequence,
                                              n = length(path$b$sequence),
                                              gaps = path$b$gaps,
                                              from = path$b$from,
                                              to = path$b$to)))
  class(alignment) <- c("smith_waterman")
  alignment
}

mark_chars <- function (word, char, edit) {
  paste(rep(char, nchar(word)), collapse = "")  
}

alignment_path <- function(m, original_a, original_b, row_i, col_i, max_match, edit_mark){
  # Create output vectors which are as long as conceivably necessary
  a_out <- rep.int(NA_character_, times = max(max_match))
  b_out <- rep.int(NA_character_, times = max(max_match))
  out_i <- 1L
  # Place our first known values in the output vectors
  b_out[out_i] <- original_b[row_i - 1]
  a_out[out_i] <- original_a[col_i - 1]
  out_i = out_i + 1L # Advance the out vector position
  
  which_end_b <- row_i - 1
  which_end_a <- col_i - 1
  
  matches <- 1L
  gaps_a <- 0L
  gaps_b <- 0L
  mismatches <- 0L
  which_start_a <- 0;
  which_start_b <- 0;
  
  # Begin moving up, left, or diagonally within the matrix till we hit a zero
  while (m[row_i - 1, col_i - 1] != 0) {
    
    # Values of the current cell, the cells up, left, diagonal, and the max
    up       <- m[row_i - 1, col_i]
    left     <- m[row_i, col_i - 1]
    diagn    <- m[row_i - 1, col_i - 1]
    max_cell <- max(up, left, diagn)
    
    # Move in the direction of the maximum cell. If there are ties, choose up
    # first, then left, then diagonal. Privilege up and left because they
    # preserve edits.
    #
    # In each case add the current words to the out vectors. For moves up and
    # and left there will be an insertion/deletion, so add a symbol like ####
    # that is the same number of characters as the word in the other vector.
    #
    # Note that the index of the matrix is offset by one from character vectors
    # a and b, so we use the row and column indices - 1. The column corresponds
    # to `a` and the rows correspond to `b`.
    if (up == max_cell) {
      gaps_a <- gaps_a + 1L
      row_i <- row_i - 1
      bword <- original_b[row_i - 1]
      b_out[out_i] <- bword
      a_out[out_i] <- mark_chars(bword, edit_mark)
    } else if (left == max_cell) {
      gaps_b <- gaps_b + 1L
      col_i <- col_i - 1
      aword <- original_a[col_i - 1]
      b_out[out_i] <- mark_chars(aword, edit_mark)
      a_out[out_i] <- aword
    } else if (diagn == max_cell) {
      row_i <- row_i - 1
      col_i <- col_i - 1
      bword <- original_b[row_i - 1]
      aword <- original_a[col_i - 1]
      # Diagonals are a special case, because instead of an insertion or a
      # deletion we might have a substitution. If that is the case,
      # then treat it like a double insertion and deletion.
      if (tolower(aword) == tolower(bword)) {
        matches <- matches + 1L
        b_out[out_i] <- bword
        a_out[out_i] <- aword
      } else {
        mismatches <- mismatches + 1L
        b_out[out_i] <- bword
        a_out[out_i] <- mark_chars(bword, edit_mark)
        out_i <- out_i + 1
        b_out[out_i] <- mark_chars(aword, edit_mark)
        a_out[out_i] <- aword
      }
    }
    which_start_b <- row_i
    which_start_a <- col_i
    
    # Move forward one position in the out vectors, no matter which direction
    # we moved
    out_i <- out_i + 1
    
  }
  which_start_b <- which_start_b - 1
  which_start_a <- which_start_a - 1
  
  # We went backwards so revert it
  b_out <- rev(b_out[!is.na(b_out)])
  a_out <- rev(a_out[!is.na(a_out)])
  list(matches = matches,
       mismatches = mismatches, 
       a = list(sequence = a_out, from = which_start_a, to = which_end_a, gaps = gaps_a),
       b = list(sequence = b_out, from = which_start_b, to = which_end_b, gaps = gaps_b))
}

#' @export
print.smith_waterman <- function(x, ...){
  cat(sprintf("Swith Waterman local alignment score: %s", x$sw), sep = "\n\n")
  cat("----------", sep = "\n")
  cat("Document a", sep = "\n")
  cat("----------", sep = "\n")
  cat(strwrap(x$a$alignment$text, width = 72), sep = "\n")
  cat("----------", sep = "\n")
  cat("Document b", sep = "\n")
  cat("----------", sep = "\n")
  cat(strwrap(x$b$alignment$text, width = 72), sep = "\n")
}



#' @export
as.data.frame.smith_waterman <- function(x, ...){
  if(x$a$alignment$n > 0){
    a_aligned <- x$a$alignment$text
    a_from <- x$a$alignment$from
    a_to <- x$a$alignment$to
    a_fromto <- list(x$a$tokens[a_from:a_to])
  }else{
    a_aligned <- NA_character_
    a_from <- NA_integer_
    a_to <- NA_integer_
    a_fromto <- NA
  }
  if(x$b$alignment$n > 0){
    b_aligned <- x$b$alignment$text
    b_from <- x$b$alignment$from
    b_to <- x$b$alignment$to
    b_fromto <- list(x$b$tokens[b_from:b_to])
  }else{
    b_aligned <- NA_character_
    b_from <- NA_integer_
    b_to <- NA_integer_
    b_fromto <- NA
  }
  data.frame(a = x$a$text, b = x$b$text, 
             sw = x$sw, 
             similarity = x$similarity,
             matches = x$matches,
             mismatches = x$mismatches,
             a_n = x$a$n,
             a_aligned = a_aligned, 
             a_similarity = x$a$similarity,
             a_gaps = x$a$alignment$gaps,
             a_from = a_from,
             a_to = a_to,
             a_fromto = I(a_fromto),
             b_n = x$b$n,
             b_aligned = b_aligned, 
             b_similarity = x$b$similarity,
             b_gaps = x$b$alignment$gaps,
             b_from = b_from,
             b_to = b_to, 
             b_fromto = I(b_fromto), stringsAsFactors = FALSE)
}





#' @title Perform multiple alignments using Smith-Waterman
#' @description Utility function to perform all pairwise combinations of alignments between text.
#' @param a a data.frame with columns doc_id and text. Or a character vector where the names of the character vector respresent a doc_id and the character vector corresponds to the text.
#' @param b a data.frame with columns doc_id and text. Or a character vector where the names of the character vector respresent a doc_id and the character vector corresponds to the text.
#' @param FUN a function to apply on an object of class \code{smith_waterman} which has done the pairwise alignment. 
#' Defaults to \code{identity}. Other options are as.data.frame or your own function. See the examples.
#' @param ... other arguments passed on to \code{\link{smith_waterman}}
#' @return a list of pairwise Smith-Waterman comparisons after which the FUN argument is applied on all of these pairwise alignments.
#' The output of the result of FUN is enriched by adding a list element 
#' a_doc_id and b_doc_id which correspond to the doc_id's provided in \code{a} and \code{b} and which can be used
#' in order to identify the match.
#' @seealso \code{\link{smith_waterman}}
#' @export
#' @examples
#' x <- data.frame(doc_id = c(1, 2),
#'                 text = c("This is some text", "Another set of texts."),
#'                 stringsAsFactors = FALSE)
#' y <- data.frame(doc_id = c(1, 2, 3),
#'                 text = c("were as some thing", "else, another set", NA_character_),
#'                 stringsAsFactors = FALSE)
#' alignments <- smith_waterman_pairwise(x, y)
#' alignments
#' alignments <- smith_waterman_pairwise(x, y, FUN = as.data.frame)
#' do.call(rbind, alignments)
#' alignments <- smith_waterman_pairwise(x, y, 
#'                                       FUN = function(x) list(sim = x$similarity))
#' do.call(rbind, alignments)
#' 
#' x <- c("1" = "This is some text", "2" = "Another set of texts.")
#' y <- c("1" = "were as some thing", "2" = "else, another set", "3" = NA_character_)
#' alignments <- smith_waterman_pairwise(x, y)
smith_waterman_pairwise <- function(a, b, FUN = identity, ...){
  as_tif <- function(x){
    if(is.character(x) | is.factor(x)){
      if(is.null(names(x))){
        x <- data.frame(doc_id = seq_along(x), text = as.character(x), stringsAsFactors = FALSE)
      }else{
        x <- data.frame(doc_id = names(x), text = as.character(x), stringsAsFactors = FALSE)
      }
    }
    x
  }
  set_names <- function(object, objectnames){
    names(object) <- objectnames
    object
  }
  
  a <- as_tif(a)
  b <- as_tif(b)
  stopifnot(is.data.frame(a) & is.data.frame(b))
  stopifnot(all(c("doc_id", "text") %in% colnames(a)))
  stopifnot(all(c("doc_id", "text") %in% colnames(b)))
  a <- a[, c("doc_id", "text"), drop = FALSE]
  b <- b[, c("doc_id", "text"), drop = FALSE]
  
  combinations <- merge(a, b, by = character(), suffixes = c(".a", ".b"))
  x <- mapply(a = set_names(combinations$text.a, combinations$doc_id.a), 
              b = set_names(combinations$text.b, combinations$doc_id.b), 
              FUN = function(a, b, ...){
                alignment <- smith_waterman(a = a, b = b, ...)
                alignment
              }, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  x <- lapply(x, FUN = FUN)
  x <- mapply(x, 
              a = combinations$doc_id.a, b = combinations$doc_id.b, 
              FUN = function(x, a, b, ...){
                x$a_doc_id <- a
                x$b_doc_id <- b
                x
              }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  x
}