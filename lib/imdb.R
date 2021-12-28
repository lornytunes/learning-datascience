#
# function that takes a training corpus (texts)
# and returns a vocabulary: 10,000 words that
# appear in at least 10% of the documents, but
# fewer than half.
#

create_pruned_vocabulary <- function(texts) {
    # create an iterator over the training set
    it_train <- text2vec::itoken(
        texts,
        preprocessor = tolower,
        tokenizer = text2vec::word_tokenizer,
        ids = names(texts),
        progressbar = FALSE
    )
    # tiny stop word list
    stop_words <- wrapr::qc(the, a, an, this, that, those, i, you)
    vocab <- text2vec::create_vocabulary(it_train, stopwords = stop_words)
    
    # prune the vocabulary
    # prune anything too common (appears in over half the documents)
    # prune anything too rare (appears in less than 0.1% of the documents)
    # limit to 10,000 words after that
    pruned_vocab <- text2vec::prune_vocabulary(
        vocab,
        doc_proportion_max = 0.5,
        doc_proportion_min = 0.001,
        vocab_term_max = 10000
    )
    
    pruned_vocab
}


# take a corpus and a vocabulary
# and return a sparse matrix (of the kind xgboost will take)
# rows are documents, columns are vocab words
# this representation loses the order or the words in the documents
make_matrix <- function(texts, vocab) {
    iter <- text2vec::itoken(
        texts,
        preprocessor = tolower,
        tokenizer = text2vec::word_tokenizer,
        ids = names(texts),
        progressbar = FALSE
    )
    text2vec::create_dtm(
        iter,
        text2vec::vocab_vectorizer(vocab)
    )
}