#include <Rcpp.h>

// [[Rcpp::export]]
std::string smith_waterman_mark_chars(Rcpp::String text, char edit_mark) {
  int l = LENGTH(text.get_sexp());
  return(std::string(l, edit_mark));
} 

// [[Rcpp::export]]
std::string lowercase(std::string data) {
  std::transform(data.begin(), data.end(), data.begin(), ::tolower);
  return(data);
}  





// [[Rcpp::export]]
Rcpp::IntegerMatrix smith_waterman_matrix(
    Rcpp::CharacterVector a,
    Rcpp::CharacterVector b,
    int score_match, 
    int score_gap,
    int score_mismatch,
    Rcpp::IntegerMatrix x) {
  
  unsigned int ncols = a.length();
  unsigned int nrows = b.length();

  // Go from top left to right and next down
  for (unsigned int row_i = 0; row_i < nrows; ++row_i) {
    int score, deletion, insertion;
    
    for (unsigned int col_j = 0; col_j < ncols; ++col_j) {
      // We want to fill up at the end of the loop cell (row_i + 1) x (col_j + 1)
      // So fill cell X based on content in Y 
      // - - - - - - 
      // - - Y Y - - 
      // - - Y X - -
      // - - - - - - 
      
      // Go along the diagonal by applying the similarity function between the 2 texts
      if(a[col_j] == b[row_i]){
        score     = x(row_i, col_j) + score_match;
      }else{
        score     = x(row_i, col_j) + score_mismatch;
      }
      // Take one letter/word from the column and we have a gap
      deletion  = x(row_i, col_j + 1) + score_gap;
      // Take one letter/word from the row and we have a gap
      insertion = x(row_i + 1, col_j) + score_gap;
      // Final value is the maximum of 0, going alongside the diagonal, a deletion or an insertion
      x(row_i + 1, col_j + 1) = max(Rcpp::IntegerVector::create(0, score, deletion, insertion));
    }
  }
  return x;
}


// [[Rcpp::export]]
Rcpp::NumericMatrix smith_waterman_function(
    Rcpp::CharacterVector a,
    Rcpp::CharacterVector b,
    double score_gap,
    Rcpp::Function similarity) {
  
  unsigned int ncols = a.length();
  unsigned int nrows = b.length();
  Rcpp::NumericMatrix x(nrows + 1, ncols + 1);
  
  // Go from top left to right and next down
  for (unsigned int row_i = 0; row_i < nrows; ++row_i) {
    double score, deletion, insertion;

    for (unsigned int col_j = 0; col_j < ncols; ++col_j) {
      // We want to fill up at the end of the loop cell (row_i + 1) x (col_j + 1)
      // So fill cell X based on content in Y 
      // - - - - - - 
      // - - Y Y - - 
      // - - Y X - -
      // - - - - - - 

      // Go along the diagonal by applying the similarity function between the 2 texts
      score     = x(row_i, col_j) + Rcpp::as<double>(similarity(Rcpp::as<std::string>(a[col_j]), Rcpp::as<std::string>(b[row_i])));  
      // Take one letter/word from the column and we have a gap
      deletion  = x(row_i, col_j + 1) + score_gap;
      // Take one letter/word from the row and we have a gap
      insertion = x(row_i + 1, col_j) + score_gap;
      // Final value is the maximum of 0, going alongside the diagonal, a deletion or an insertion
      x(row_i + 1, col_j + 1) = max(Rcpp::NumericVector::create(0, score, deletion, insertion));
    }
  }
  return x;
}



// [[Rcpp::export]]
Rcpp::List smith_waterman_path(
    Rcpp::NumericMatrix m,
    Rcpp::CharacterVector original_a,
    Rcpp::CharacterVector original_b,
    int row_i,
    int col_i,
    char edit_mark) {
  row_i = row_i - 1;
  col_i = col_i - 1;
  
  int matches = 1;
  int mismatches = 0;
  int gaps_a = 0;
  int gaps_b = 0;
  int which_start_a = 0;
  int which_start_b = 0;
  int which_end_a;
  int which_end_b;
  std::vector<std::string> a_out;
  std::vector<std::string> b_out;
  
  b_out.push_back(Rcpp::as<std::string>(original_b[row_i - 1]));
  a_out.push_back(Rcpp::as<std::string>(original_a[col_i - 1]));

  which_end_b = row_i - 1;
  which_end_a = col_i - 1;
  
  while(m(row_i - 1, col_i - 1) != 0) {
    // Values of the current cell, the cells up, left, diagonal, and the max
    double up       = m(row_i - 1, col_i);
    double left     = m(row_i, col_i - 1);
    double diagn    = m(row_i - 1, col_i - 1);
    
    if ((up >= left) && (up >= diagn)) {
      gaps_a = gaps_a + 1;
      row_i = row_i - 1;
      std::string bword = Rcpp::as<std::string>(original_b[row_i - 1]);
      b_out.push_back(bword);
      a_out.push_back(smith_waterman_mark_chars(bword, edit_mark));
    } else if (left >= diagn) {
      gaps_b = gaps_b + 1;
      col_i = col_i - 1;
      std::string aword = Rcpp::as<std::string>(original_a[col_i - 1]);
      b_out.push_back(smith_waterman_mark_chars(aword, edit_mark));
      a_out.push_back(aword);
    } else{
      row_i = row_i - 1;
      col_i = col_i - 1;
      std::string bword = Rcpp::as<std::string>(original_b[row_i - 1]);
      std::string aword = Rcpp::as<std::string>(original_a[col_i - 1]);
      // Diagonals are a special case, because instead of an insertion or a
      // deletion we might have a substitution. If that is the case,
      // then treat it like a double insertion and deletion.
      if (lowercase(aword) == lowercase(bword)) {
        matches = matches + 1L;
        b_out.push_back(bword);
        a_out.push_back(aword);
      } else {
        mismatches = mismatches + 1L;
        b_out.push_back(bword);
        a_out.push_back(smith_waterman_mark_chars(bword, edit_mark));
        b_out.push_back(smith_waterman_mark_chars(aword, edit_mark));
        a_out.push_back(aword);
      }
    }
    which_start_b = row_i;
    which_start_a = col_i;
  }
  which_start_b = which_start_b - 1;
  which_start_a = which_start_a - 1;
  
  std::reverse(std::begin(a_out), std::end(a_out));
  std::reverse(std::begin(b_out), std::end(b_out));

  Rcpp::List output = Rcpp::List::create(
    Rcpp::Named("matches") = matches,
    Rcpp::Named("mismatches") = mismatches,
    Rcpp::Named("a") = Rcpp::List::create(
      Rcpp::Named("sequence") = a_out, 
      Rcpp::Named("from") = which_start_a + 1,
      Rcpp::Named("to") = which_end_a + 1,
      Rcpp::Named("gaps") = gaps_a),
    Rcpp::Named("b") = Rcpp::List::create(
      Rcpp::Named("sequence") = b_out, 
      Rcpp::Named("from") = which_start_b + 1,
      Rcpp::Named("to") = which_end_b + 1,
      Rcpp::Named("gaps") = gaps_b));
  return output;
}



// [[Rcpp::export]]
Rcpp::List smith_waterman_path_integer(
    Rcpp::IntegerMatrix m,
    Rcpp::CharacterVector original_a,
    Rcpp::CharacterVector original_b,
    int row_i,
    int col_i,
    char edit_mark) {
  // Same function as smith_waterman_path but where m is Rcpp::IntegerMatrix
  row_i = row_i - 1;
  col_i = col_i - 1;
  
  int matches = 1;
  int mismatches = 0;
  int gaps_a = 0;
  int gaps_b = 0;
  int which_start_a = 0;
  int which_start_b = 0;
  int which_end_a;
  int which_end_b;
  std::vector<std::string> a_out;
  std::vector<std::string> b_out;
  
  b_out.push_back(Rcpp::as<std::string>(original_b[row_i - 1]));
  a_out.push_back(Rcpp::as<std::string>(original_a[col_i - 1]));
  
  which_end_b = row_i - 1;
  which_end_a = col_i - 1;
  
  while(m(row_i - 1, col_i - 1) != 0) {
    // Values of the current cell, the cells up, left, diagonal, and the max
    double up       = m(row_i - 1, col_i);
    double left     = m(row_i, col_i - 1);
    double diagn    = m(row_i - 1, col_i - 1);
    
    if ((up >= left) && (up >= diagn)) {
      gaps_a = gaps_a + 1;
      row_i = row_i - 1;
      std::string bword = Rcpp::as<std::string>(original_b[row_i - 1]);
      b_out.push_back(bword);
      a_out.push_back(smith_waterman_mark_chars(bword, edit_mark));
    } else if (left >= diagn) {
      gaps_b = gaps_b + 1;
      col_i = col_i - 1;
      std::string aword = Rcpp::as<std::string>(original_a[col_i - 1]);
      b_out.push_back(smith_waterman_mark_chars(aword, edit_mark));
      a_out.push_back(aword);
    } else{
      row_i = row_i - 1;
      col_i = col_i - 1;
      std::string bword = Rcpp::as<std::string>(original_b[row_i - 1]);
      std::string aword = Rcpp::as<std::string>(original_a[col_i - 1]);
      // Diagonals are a special case, because instead of an insertion or a
      // deletion we might have a substitution. If that is the case,
      // then treat it like a double insertion and deletion.
      if (lowercase(aword) == lowercase(bword)) {
        matches = matches + 1L;
        b_out.push_back(bword);
        a_out.push_back(aword);
      } else {
        mismatches = mismatches + 1L;
        b_out.push_back(bword);
        a_out.push_back(smith_waterman_mark_chars(bword, edit_mark));
        b_out.push_back(smith_waterman_mark_chars(aword, edit_mark));
        a_out.push_back(aword);
      }
    }
    which_start_b = row_i;
    which_start_a = col_i;
  }
  which_start_b = which_start_b - 1;
  which_start_a = which_start_a - 1;
  
  std::reverse(std::begin(a_out), std::end(a_out));
  std::reverse(std::begin(b_out), std::end(b_out));
  
  Rcpp::List output = Rcpp::List::create(
    Rcpp::Named("matches") = matches,
    Rcpp::Named("mismatches") = mismatches,
    Rcpp::Named("a") = Rcpp::List::create(
      Rcpp::Named("sequence") = a_out, 
      Rcpp::Named("from") = which_start_a + 1,
      Rcpp::Named("to") = which_end_a + 1,
      Rcpp::Named("gaps") = gaps_a),
    Rcpp::Named("b") = Rcpp::List::create(
      Rcpp::Named("sequence") = b_out, 
      Rcpp::Named("from") = which_start_b + 1,
      Rcpp::Named("to") = which_end_b + 1,
      Rcpp::Named("gaps") = gaps_b));
  return output;
}
