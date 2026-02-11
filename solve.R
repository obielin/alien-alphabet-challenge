library(stringi)
words <- c(
  "⪏⊕⪏⊥⊛",
  "⪏⊕⇲⊢⪏",
  "⪏⊟⊞⊝◴⇲◉",
  "⪏⊞⊢⪏◉",
  "⪏⨙↪⊡⇲",
  "⪏⊛⊟⇲⊣↔",
  "⪏⊝⊛∭⊞↔⪏",
  "⪏⊣⊞৲⇲◉",
  "⊚⪏⊢⇲◉",
  "⊚↔⊚⊝↔⇲",
  "⊚⇲⊤⊣⇲",
  "⊚⊢⇲⚆⊛",
  "⊕⪏⊝⪏",
  "⊕⊛",
  "⊕⊢⪏৲৲⪏",
  "⊕⊤⊣⪏↔⊠⪏",
  "⊟⪏◉⇲◉",
  "⊟⊞⊣⊡⊢⇲",
  "⊟⊢⇲৲⇲◉",
  "⊞⊟↪",
  "⊞↔⊠⇲⊣⪏",
  "⊞⊝⊞⊤∭⊞⊢↔⪏",
  "⊞⊝⊥↔⊟⪏",
  "⊞⊗⇲⊟⇲◉",
  "⊞⊢↪⊡⪏",
  "⊛⊝↔⇲◉",
  "⊛৲⊞⊢⪏",
  "∭⪏⊝⪏◉◉⪏",
  "∭⊞⇲◉",
  "↔⊟⊞⪏",
  "↔◉⇲◉",
  "↔◉⊡⇲⊢↔⪏",
  "↔⚆∭⊤◉",
  "⊠⪏⨙⪏⊣↔",
  "⊠⪏⊠⇲◉",
  "⊠⪏⊝⇲◉",
  "⊠⪏⊢⊟↔⪏",
  "⊠⇲◉৲⇲◉",
  "⊝⊞⊗⊛",
  "⊝↔৲⊣⊛",
  "⊝⇲⊕⇲◉",
  "৲⪏⨙↔",
  "৲⪏⊡↔",
  "৲⊞⊝↔",
  "৲⊞⊢⪏",
  "⊣⊞⊢⇲",
  "⊣⊛◉↔",
  "⊣⊤⚆⊡⪏",
  "⊗⊞⊣⇲◉",
  "⊗⊤⊝⇲",
  "⇲৲⇲⊢◴⇲",
  "⇲⊣⊞↔⊢⇲",
  "⇲⊗⊤",
  "⇲⊢⇲◉",
  "⊥⪏↔⊟↔",
  "⊥⇲⊝⊛",
  "⊥⇲⊡⪏৲⇲◉",
  "⊥⊤⊢",
  "⊢↔⨙⪏",
  "⊢⇲⊟⇲",
  "◉⊥↔⊡↔",
  "◉⊡⇲৲⪏",
  "⊡⪏⊗⊛",
  "⊡⊞⚆⊣⊛",
  "⊡⇲⊥⇲◉",
  "⊤⊕⊢⇲",
  "⊤⊥⊣⇲◉",
  "◴↔⊝⇲◉",
  "◴⊤◉⊛",
  "◴↪◉",
  "⚆⪏⊢⪏",
  "⚆⊢⇲⊣⇲◉",
  "⚆↪⊢⪏",
  "⊜⪏⊢↔",
  "⊜⊤⚆⊛",
  "↪⊠⊞⪏⊣⇲◉",
  "↪⊢⪏"
)

split_syms <- function(x) {
  unlist(stri_split_boundaries(x, type = "character"))
}
W <- lapply(words, split_syms)

edges <- list()
for (i in seq_len(length(W) - 1)) {
  a <- W[[i]]
  b <- W[[i + 1]]
  m <- min(length(a), length(b))

  diff_pos <- which(a[seq_len(m)] != b[seq_len(m)])[1]

  if (!is.na(diff_pos)) {
    edges[[length(edges) + 1]] <- c(a[diff_pos], b[diff_pos])
  } else if (length(a) > length(b)) {
    stop("Prefix rule violated")
  }
}

E <- unique(do.call(rbind, edges))
colnames(E) <- c("from", "to")


symbols <- sort(unique(unlist(W)))

adj <- setNames(vector("list", length(symbols)), symbols)
indeg <- setNames(rep(0L, length(symbols)), symbols)


for (i in seq_len(nrow(E))) {
  u <- E[i, "from"]
  v <- E[i, "to"]
  adj[[u]] <- unique(c(adj[[u]], v))
}

for (u in names(adj)) {
  for (v in adj[[u]]) {
    indeg[[v]] <- indeg[[v]] + 1L
  }
}


Q <- sort(names(indeg)[indeg == 0L])
out <- character()

while (length(Q) > 0) {
  u <- Q[1]
  Q <- Q[-1]
  out <- c(out, u)

  for (v in adj[[u]]) {
    indeg[[v]] <- indeg[[v]] - 1L
    if (indeg[[v]] == 0L) {
      Q <- sort(c(Q, v))
    }
  }
}

if (length(out) != length(symbols)) {
  stop("Cycle detected: inconsistent constraints")
}

alphabet <- paste0(out, collapse = "")
alphabet

rank <- setNames(seq_along(out), out)
compare_words <- function(a, b) {
  a <- split_syms(a)
  b <- split_syms(b)
  m <- min(length(a), length(b))

  for (i in seq_len(m)) {
    if (rank[a[i]] != rank[b[i]]) {
      return(rank[a[i]] - rank[b[i]])
    }
  }
  length(a) - length(b)
}
stopifnot(all(
  sapply(seq_len(length(words) - 1),
         function(i) compare_words(words[i], words[i + 1]) <= 0)
))


