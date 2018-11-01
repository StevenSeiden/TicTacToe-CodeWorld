import Extras((<$>))
-- Modify this function, so that it draws any possible configuration of a board
--draw(configuration) = text("Show the configuration here")

-- Try different configurations to check that they show up correctly
initial :: [Number] -> Board
initial(_) = [ X, O, N
             , N, X, N
             , O, O, X ]
             




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
  
  --Upper row
  & translated(scaled(text(findMark(state#1)),5,5),-5,5) --Left
  & translated(scaled(text(findMark(state#2)),5,5),0,5)  --Center
  & translated(scaled(text(findMark(state#3)),5,5),5,5)  --Right
  
  --Middle row
  & translated(scaled(text(findMark(state#4)),5,5),-5,0) --Left
  & translated(scaled(text(findMark(state#5)),5,5),0,0)  --Center
  & translated(scaled(text(findMark(state#6)),5,5),5,0)  --Right

  --Lower row
  & translated(scaled(text(findMark(state#7)),5,5),-5,-5)--Left
  & translated(scaled(text(findMark(state#8)),5,5),0,-5) --Center
  & translated(scaled(text(findMark(state#9)),5,5),5,-5) --Right

  

findMark :: Mark -> Text
findMark(X) = "X"
findMark(O) = "O"
findMark(N) = ""
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
