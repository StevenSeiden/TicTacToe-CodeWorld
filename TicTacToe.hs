import Extras((<$>))
--import Prelude()

-- Modify this function, so that it draws any possible configuration of a board
--draw(configuration) = text("Show the configuration here")

-- Try different configurations to check that they show up correctly
init(random) = random#1
initial :: [Number] -> Board
initial(random) = shuffled([ randomMark(1), randomMark(2), randomMark(3)
                             , randomMark(4), randomMark(5), randomMark(6)
                             , randomMark(7), randomMark(8), randomMark(9) ],random#1)
   where
   randomMark(number) =
     if(random#number < 0.3) then X
     else if(random#number < 0.6) then O
     else N
      
  --where randomNum = newtype UniversalTime
  --where [a, b, c, d, e, f] = first(randomNumbers(42), 6)
  --randomNum :: Number
  --where randomNum = first(randomNumbers([1,2],5))
  --where first(initial(randomNumbers))
  --where initial(rs) = (-8, rs # 1)
  --lkdasjfkas :: [Number]
  --lkdasjfkas = randomNumber(1)
  

draw :: Board -> Picture
draw(state) = rectangle(-5,5)
  & translated(rectangle(-5,5),5,0)
  & translated(rectangle(-5,5),5,-5)
  & translated(rectangle(-5,5),5,5)
  & translated(rectangle(-5,5),-5,0)
  & translated(rectangle(-5,5),-5,-5)
  & translated(rectangle(-5,5),-5,5)
  & translated(rectangle(-5,5),0,-5)
  & translated(rectangle(-5,5),0,5)
  
  & pictures(placeCell <$> [0..8])
  where
  placeCell(cell) = translated(scaled(text(findMark(state#(cell+1))),5,5)
                             , (-5*remainder(cell,3))+5
                             , (5*truncation(cell/3))-5
                             )

  

findMark :: Mark -> Text
findMark(X) = "X"
findMark(O) = "O"
findMark(N) = ""
{-findMark(currentState) =
  if(currentState == X) then "X"
  else if(currentState == O) then "O"
  else ""-}

--------------------------------------------------------------------------------
-- DO NOT MODIFY THE CODE BELOW THIS LINE
--------------------------------------------------------------------------------

type Board = [Mark]

data Mark = X | O | N

main = interactionOf(initial,update,handle,draw)

update :: (Board,Number) -> Board
update(state,elapsedTime) = state

handle :: (Board,Event) -> Board
handle(state,event) = state
