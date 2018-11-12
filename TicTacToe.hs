import Extras((<$>))

-- Modify this function, so that it draws any possible configuration of a board
--draw(configuration) = text("Show the configuration here")

-- Try different configurations to check that they show up correctly
initial :: [Number] -> State
initial(random) = ([ N, N, N
                  , N, N, N
                  , N, N, N ],PlayerO)

draw :: State -> Picture
draw(board,player) = rectangle(-5,5)
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

findMark :: Mark -> Text
findMark(X) = "X"
findMark(O) = "O"
findMark(N) = ""

set :: (Number,a,[a]) -> [a]
set(index,value,list) = change <$> [1..length(list)]
  where
  change(i) = if i == index then value else list#i


type Board = [Mark]
data Player = PlayerX | PlayerO
type State = ([Mark],Player)

data Mark = X | O | N

main = interactionOf(initial,update,handle,draw)

update :: (State,Number) -> State
update(state,elapsedTime) = state
changePlayer :: (Player) -> Player
changePlayer(PlayerX) = PlayerO
changePlayer(PlayerO) = PlayerX

markToPlayer :: Player -> Mark
markToPlayer(PlayerX) = X
markToPlayer(PlayerO) = O

handle :: (State,Event) -> State
handle((board,player),MousePress(LeftButton,(x,y))) =
  if board#findClickedMark(x,y) == N then (set(findClickedMark(x,y),markToPlayer(player),board),changePlayer(player))
  else (board,player)
  
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
