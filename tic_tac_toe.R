positions <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
board <- matrix(as.character(positions), nrow=3, ncol=3)
player <- 'X'
make.move <- function() {
  write.table(board, row.names=F, col.names=F, quote = F)
  n <- as.numeric(readline(paste('Player', player, '[1-9]: ', sep=' ')))
  if (!(n %in% positions)) {
    cat('Invalid move!\n')
    make.move()
  }
  positions <<- positions[positions != n]
  board[board==n] <- player
  return (board)
}
check.win <- function() {
  for (i in 1:3) {
    if (all(board[i,] == board[i,1])) {
      return(TRUE)
    }
  }
  for(i in 1:3) {
    if (all(board[,i] == board[1,i])) {
      return(TRUE)
    }
  }
  if (all(diag(board) == board[1,1])) {
    return(TRUE)
  }
  if (all(diag(board[nrow(board):1,]) == board[1,1])) {
    return(TRUE)
  }
  return(FALSE)
}
tic.tac.toe <- function() {
  cat('Play in turns, first X and then O.\n')
  while(length(positions) != 0) {
    board <<- make.move()
    if (check.win() == TRUE) {
      cat("Player ", player, " wins!\n")
      return(invisible(NULL))
    }
    player <<- if (player=='X') 'O' else 'X'
  }
  print('Draw')
}
tic.tac.toe()

