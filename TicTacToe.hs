import Extras((<$>))

-- Modify this function, so that it draws any possible configuration of a board
--draw(configuration) = text("Show the configuration here")

-- Try different configurations to check that they show up correctly


main = interactionOf(initial,update,handle,draw)

type Board = [Mark]
data Mark = X | O | N
data State = Playing([Mark],Player,Winner) | GameOver Winner
data Player = PlayerX | PlayerO
data Winner = None | PlayerOWin | PlayerXWin | NoneWon

initial :: [Number] -> State
initial(random) = Playing(
  [ N, N, N
  , N, N, N
  , N, N, N ],
  PlayerO,None)

draw :: State -> Picture
draw(Playing(board,player,winner)) = 
  scaled(translated(text(winnerHeader(findWinner(board))),0,4.2),2,2)
  & drawBoard(Playing(board,player,winner))

draw(GameOver(winner)) = 
  translated(text(winnerHeader(winner)),5,5)
  & text("Game Over")

findMark :: Mark -> Text
findMark(X) = "X"
findMark(O) = "O"
findMark(N) = ""

winnerHeader :: Winner -> Text
winnerHeader(None) = ""
winnerHeader(PlayerOWin) = "O wins!"
winnerHeader(PlayerXWin) = "X wins!"
winnerHeader(NoneWon) = "Tie!"

findWinner :: ([Mark]) -> Winner
findWinner((board)) =
  --Rows
  if board#1 == X && board#2 == X && board#3 == X 
    then PlayerXWin
  else if board#1 == O && board#2 == O && board#3 == O 
    then PlayerOWin
  else if board#4 == X && board#5 == X && board#6 == X 
    then PlayerXWin
  else if board#4 == O && board#5 == O && board#6 == O 
    then PlayerOWin
  else if board#7 == X && board#8 == X && board#9 == X 
    then PlayerXWin
  else if board#7 == O && board#8 == O && board#9 == O 
    then PlayerOWin
  
  --Columns 
  else if board#1 == X && board#4 == X && board#7 == X 
    then PlayerXWin
  else if board#1 == O && board#4 == O && board#7 == O 
    then PlayerOWin
  else if board#2 == X && board#5 == X && board#8 == X 
    then PlayerXWin
  else if board#2 == O && board#5 == O && board#8 == O 
    then PlayerOWin
  else if board#3 == X && board#6 == X && board#9 == X 
    then PlayerXWin
  else if board#3 == O && board#6 == O && board#9 == O 
    then PlayerOWin
    
  --Diagonals
  else if board#1 == X && board#5 == X && board#9 == X 
    then PlayerXWin
  else if board#1 == O && board#5 == O && board#9 == O 
    then PlayerOWin
  else if board#3 == X && board#5 == X && board#7 == X 
    then PlayerXWin
  else if board#3 == O && board#5 == O && board#7 == O 
    then PlayerOWin
    
  --Checking for a full board
  else if board#1 /= N && board#2 /= N && board#3 /= N &&
          board#4 /= N && board#5 /= N && board#6 /= N &&
          board#7 /= N && board#8 /= N && board#9 /= N
    then NoneWon
  else None

set :: (Number,a,[a]) -> [a]
set(index,value,list) = change <$> [1..length(list)]
  where
  change(i) = if i == index then value else list#i


update :: (State,Number) -> State
update(state,elapsedTime) = state

changePlayer :: (Player) -> Player
changePlayer(PlayerX) = PlayerO
changePlayer(PlayerO) = PlayerX

markToPlayer :: Player -> Mark
markToPlayer(PlayerX) = X
markToPlayer(PlayerO) = O

handle :: (State,Event) -> State
handle(Playing(board,player,winner),MousePress(LeftButton,(x,y))) =
  if findWinner(board) == None then 
    if board#findClickedMark(x,y) == N 
      then Playing(set(findClickedMark(x,y),
                  markToPlayer(player),board),
                  changePlayer(player),
                  findWinner(board))
    else Playing(board,player,winner)
  else GameOver(winner)
  where
  findClickedMark(x,y) = if y >= 2.5 && x>=2.5 then 3
  else if y>=2.5 && x >= -2.5 then 2
  else if y>=2.5 then 1
  
  else if y >= -2.5 && x >= 2.5 then 6
  else if y >= -2.5 && x >= -2.5 then 5
  else if y>= -2.5 then 4

  else if x >= 2.5 then 9
  else if x >= -2.5 then 8
  else 7

handle(state,event) = state

drawBoard :: State -> Picture
drawBoard(Playing(board,player,winner)) = rectangle(-5,5)
  & translated(rectangle(-5,5),5,0)
  & translated(rectangle(-5,5),5,-5)
  & translated(rectangle(-5,5),5,5)
  & translated(rectangle(-5,5),-5,0)
  & translated(rectangle(-5,5),-5,-5)
  & translated(rectangle(-5,5),-5,5)
  & translated(rectangle(-5,5),0,-5)
  & translated(rectangle(-5,5),0,5)
  
  & translated(scaled(text(findMark(board#1)),5,5),-5,5)
  & translated(scaled(text(findMark(board#2)),5,5),0,5)
  & translated(scaled(text(findMark(board#3)),5,5),5,5)
  
  & translated(scaled(text(findMark(board#4)),5,5),-5,0)
  & translated(scaled(text(findMark(board#5)),5,5),0,0)
  & translated(scaled(text(findMark(board#6)),5,5),5,0)

  & translated(scaled(text(findMark(board#7)),5,5),-5,-5)
  & translated(scaled(text(findMark(board#8)),5,5),0,-5)
  & translated(scaled(text(findMark(board#9)),5,5),5,-5)