module Main where

import System.Random (randomRIO)
import Control.Monad (forM) 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

{-
Usefull keybinds: 
"N" Next level, Works every time for easy debug
"P" Pauses the game
"L" Loses the game
"R" Restart
"-" deletes all blocks
-}

-- | raidus of the ball (pixels)           
ballRadius :: Float
ballRadius          = 10

-- | position Y of the platform
platformPositionY :: Float
platformPositionY   = -maxHeight + 50 -- ^ Platform Y position

-- | size of the platform
platformSizeStartX, platformSizeY :: Float
platformSizeStartX  = 100
platformSizeY       = 10

-- | Initial Speeds
plataformSpeed, ballSpeedX, ballSpeedY :: Float
plataformSpeed      = 250
ballSpeedX          = 150
ballSpeedY          = 150

-- | size of the blocks
blockSizeX, blockSizeY :: Float
blockSizeX          = 100
blockSizeY          = 40

-- | display dimensions
maxWidth, maxHeight :: Float
maxWidth            = 600
maxHeight           = 350



-- | Colors
platformColor, ballColor, block1hpColor, block2hpColor, block4hpColor, blockImmuneColor, backgroundColor :: Color
platformColor       = blue
ballColor           = white
block1hpColor       = green
block2hpColor       = yellow
block3hpColor       = orange
block4hpColor       = red
blockImmuneColor    = makeColorI 200 200 200 0 
backgroundColor     = black

-- | frames per second for game event loop
fps :: Int
fps                 = 60

-- | Ball Position X, the only coordenate needed because it is always on the same Y : platformY
type PlatformX      = Float

-- | Ball Velocity X, the only coordenate needed because it it doesen't move up or down
type PlatformVX     = Float

-- | Punch Power of the ball
type Power          = Int

-- | Size of the platform
type PlatformSize   = Float

-- | Position for the blocks (Float,Float)
type Position       = Point 

-- | Health Points of the block
type BlockHP        = Int

-- | Time Step to advence
type TimeStep       = Float

-- | State of the game, 0 = Initial State for the level; 1 = Playing; 2 = Win;
-- initial state not used
type GameStatus     = Int

-- | Game Level
type Level          = Int

-- | Game Score
type Score          = Int

-- | coordinates for movement calculations for the ball and platform
-- position and velocity vector
type Movement       = (Position,          -- ^ center position
                       Vector             -- ^ linear velocity
                      )

-- | the platform;
-- can only move from left to right
data Platform       = Platform
                      PlatformX          -- ^ center X position
                      PlatformVX         -- ^ linear X velocity
                      PlatformSize       -- ^ Size on X of the platform 

-- | the ball;                  
data Ball           = Ball       
                      Movement            -- ^ Movment (position point) (movment Vector)
                      Power               -- ^ Punch power of the ball

-- | a block;
-- can't move
data Block          = Block {
                      position :: Position,  -- ^ X,Y position
                      blockHP  :: BlockHP   -- ^ Block Hp
                    }  deriving (Eq)


-- | representation for the game state;
-- platform, ball and list of blocks
data GameState      = Game {
                      blocks      :: [Block],     -- ^ list of all the blocks. Block: X,Y position and Block Hp
                      platform    :: Platform,    -- ^ center X position; linear X velocity; Size on X of the platform 
                      ball        :: Ball,        -- ^ Movment (position point) (movment Vector) and Punch Power
                      gameStatus  :: GameStatus,  -- ^ State of the game, 0 = Initial State for the level; 1 = Playing; 2 = Win;
                      gameLevel   :: Level,
                      gamePause   :: Bool,         -- ^ Pause the game needed because of the unpause on gameStatus 1
                      gameScore       :: Score
                    } 
                    | GameOver Level Score     -- ^ Game Over!!
                    | Victory Score

-- First level
initialWorld :: GameState
initialWorld         = Game
                          [Block (0,100) 2  
                          ,Block (-250,100) 1 
                          ,Block (250,100) 1 
                          ,Block (-125,100) 1 
                          ,Block (125,100) 1 
                          ] -- Test the Blocks
                          (Platform 0 0 platformSizeStartX) -- The Platform
                          (Ball ((0,platformPositionY+ballRadius+platformSizeY/2),(ballSpeedX,ballSpeedY)) 1) -- The Ball
                          0  -- State
                          1  -- Level
                          False
                          0

-- Second level Blocks                         
world2Blocks :: [Block]
world2Blocks         =    [Block (0,100) 2  
                          ,Block (-250,100) 1 
                          ,Block (250,100) 1 
                          ,Block (-125,100) 1 
                          ,Block (125,100) 1 
                          ,Block (0,200) 3  
                          ,Block (-250,200) 2 
                          ,Block (250,200) 2 
                          ,Block (-125,200) 2 
                          ,Block (125,200) 2 
                          ]

-- Third level Blocks                           
world3Blocks :: [Block]
world3Blocks         =    [Block (0,100) 2  
                          ,Block (-250,100) 1 
                          ,Block (250,100) 1 
                          ,Block (-125,100) 1 
                          ,Block (125,100) 1 
                          ,Block (0,150) 3  
                          ,Block (-250,150) 2 
                          ,Block (250,150) 2 
                          ,Block (-125,150) 2 
                          ,Block (125,150) 2 
                          ,Block (0,200) 4  
                          ,Block (-250,200) 3 
                          ,Block (250,200) 3 
                          ,Block (-125,200) 3 
                          ,Block (125,200) 3 
                          ]

-- Fourth level Blocks                           
world4Blocks :: [Block]
world4Blocks         =    [Block (0,100) 2  
                          ,Block (-250,100) 1 
                          ,Block (250,100) 1 
                          ,Block (-125,100) 1 
                          ,Block (125,100) 1 
                          ,Block (0,150) 3  
                          ,Block (-250,150) 2 
                          ,Block (250,150) 2 
                          ,Block (-125,150) 2 
                          ,Block (125,150) 2 
                          ,Block (0,200) 4  
                          ,Block (-250,200) 3 
                          ,Block (250,200) 3 
                          ,Block (-125,200) 3 
                          ,Block (125,200) 3 
                          ,Block (0,250) 4  
                          ,Block (-250,250) 4 
                          ,Block (250,250) 4 
                          ,Block (-125,250) 4 
                          ,Block (125,250) 4 
                          ]
                          
-- | Changes levels to the next updating the score and the level blocks
nextWorld :: Level -> Score -> GameState
nextWorld lvl score = if lvl == 2 then 
    Game world2Blocks (Platform 0 0 platformSizeStartX) (Ball (ballPos,ballSpeed) 1) 0 lvl False score'
  else if lvl == 3 then
    Game world3Blocks (Platform 0 0 platformSizeStartX) (Ball (ballPos,ballSpeed) 1) 0 lvl False score'
  else if lvl == 4 then
    Game world4Blocks (Platform 0 0 platformSizeStartX) (Ball (ballPos,ballSpeed) 1) 0 lvl False score'
  else if lvl == 5 then
    Game blocks (Platform 0 0 platformSizeStartX) (Ball (ballPos,ballSpeed) 1) 0 lvl False score'
  else Victory score'
  where
    ballPos   = (0,platformPositionY+ballRadius+platformSizeY/2)
    ballSpeed = (ballSpeedX,ballSpeedY)
    blocks    = [ (Block (x*(blockSizeX+5),y*(blockSizeY + 5)+ 100) (hpHelp x y) ) | x<-[-5..5], y<-[-5..5] ]
    score'    = score + 10 * lvl

-- Helper function for the creation of the pattern on the last level 
hpHelp :: Float -> Float -> Int
hpHelp x y = if (x==y || -x== (y)) then 4 else 3

-- | Draw Everything
drawWorld :: GameState -> Picture

-- Draw the End Victory screen
drawWorld (Victory score)
  = pictures [victoryEndPicture, finalScorePicture, restartPrompt]
  where
    victoryEndPicture = scale 1 1 $ translate (-400) (-50) $ color green $ text $ "! You Won !"
    finalScorePicture = scale 0.2 0.2 $ translate (-2990) (-1700) $ color white $ text $ "Final Score: " ++ show score
    restartPrompt     = scale 0.5 0.5 $ translate (-675) (-275) $ color blue $ text $ "Press R to Restart"
    
-- Draw the lose screen
drawWorld (GameOver lvl score)
  = pictures [gameOver, restartPrompt, scorePicture]
  where 
    gameOver = scale 1 1 $ translate (-400) (25) $ color red $ text $ "Game Over!"
    restartPrompt = scale 0.5 0.5 $ translate (-675) (-175) $ color blue $ text $ "Press R to Restart"
    scorePicture      = scale 0.2 0.2 $ translate (-2990) (-1700) $ color white $ text $ "Level: " ++ show lvl ++ "  Score: " ++ show score

  
-- Draw the "level Completed" state, Pause state and game
drawWorld (Game blocks (Platform px pvx ps) (Ball ((bx,by),(bvx,bvy)) bp) gameState lvl pause score)
  = if gameState == 2 then pictures [victoryPicture, restartPrompt, nextLevelPrompt, scorePicture] else
    if pause          then pictures [platformPicturesP, blocksPicturesP, ballPicturesP, pausedPicture, scorePicture] else
    pictures [platformPictures, blocksPictures, ballPictures, scorePicture]
  where 
    victoryPicture    = scale 1 1 $ translate (-375) (-50) $ color green $ text $ "You Did It"
    nextLevelPrompt   = scale 0.5 0.5 $ translate (-850) (225) $ color yellow $ text $ "Press N for Next Level"
    pausedPicture     = scale 1 1 $ translate (-500) (0) $ color blue $ text $ "Taking a Break!"
    restartPrompt     = scale 0.5 0.5 $ translate (-675) (-275) $ color blue $ text $ "Press R to Restart"
    platformPicturesP = color white (pictures [translate px (platformPositionY) (rectangleSolid platformSizeStartX platformSizeY)])
    blocksPicturesP   = pictures [translate x y (color white (rectangleSolid blockSizeX blockSizeY)) 
                               | Block (x,y) _ <- blocks]
    ballPicturesP     = color white (pictures [translate bx by (circleSolid ballRadius)])
    platformPictures  = color red (pictures [translate px (platformPositionY) (rectangleSolid platformSizeStartX platformSizeY)])
    blocksPictures    = pictures [translate x y (color (getBlockColor hp) (rectangleSolid blockSizeX blockSizeY)) 
                               | Block (x,y) hp <- blocks]
    ballPictures      = color white (pictures [translate bx by (circleSolid ballRadius)])
    scorePicture      = scale 0.2 0.2 $ translate (-2990) (-1700) $ color white $ text $ "Level: " ++ show lvl ++ "  Score: " ++ show score

-- | So we can know what color the block will have
getBlockColor :: BlockHP -> Color
getBlockColor hp =  if hp == 1  then block1hpColor else
                    if hp == 2  then block2hpColor else
                    if hp == 3  then block3hpColor else
                    if hp == 4  then block4hpColor else
                    if hp == -1 then blockImmuneColor else blue


-- | update the game state;
updateWorld :: TimeStep -> GameState -> GameState

updateWorld _ (GameOver lvl score) = GameOver lvl score
updateWorld _ (Victory score) = Victory score

updateWorld timeStep (Game blocks platform (Ball ((bx,by),(bvx,bvy)) bp) gameState lvl pause score)  
  = if pause == True  then game else
    if gameState == 1 then collisions $ movePlatformAndBall timeStep game else -- run the game
    if blocks == []   then gameWin else
    if by < platformPositionY - ballRadius * 2 then (GameOver lvl score) else
    collisions $ movePlatformAndBall timeStep game  -- This should be the start the game and chose the position (starting the speed of the ball and changing the gamestate to 1) Para isso preciso de mudar o speed inicial para o, criar um evento de enviar a bola e mudar o estado. (aqui so deixa mover a plataforma e a bola vai com a plataforma)
    where 
      game = (Game blocks platform (Ball ((bx,by),(bvx,bvy)) bp) gameState lvl pause score)
      gameWin = (Game blocks platform (Ball ((bx,by),(bvx,bvy)) bp) 2 lvl pause score)

-- | Update position of the platform and the ball
movePlatformAndBall :: TimeStep -> GameState -> GameState
movePlatformAndBall timeStep (Game blocks (Platform px pvx ps) (Ball ((bx,by),(bvx,bvy)) bp) gameState lvl pause score) 
    = Game blocks (Platform px' pvx ps) (Ball ((bx',by'),(bvx',bvy')) bp) gameState lvl pause score
    where (px',dx') = clip px pvx (maxWidth -  ps/2)
          (bx',bvx') = clip bx bvx (maxWidth - ballRadius)
          (by',bvy') = clip by bvy (maxHeight - ballRadius)
          clip h dh max
              | h' > max  = (max, -dh)
              | h' < -max = (-max, -dh)
              | otherwise = (h', dh)
              where h' = h + timeStep*dh 

-- | Checks for any colision (Ball does not update well the vector when hitting blocks)
collisions :: GameState -> GameState
collisions (Game blocks (Platform px pvx ps) (Ball ((bx,by),(bvx,bvy)) bp) gameState lvl pause score)
    = Game blocks' (Platform px pvx ps) (Ball ((bx,by),(bvx',bvy')) bp)  gameState lvl pause score'
    where bvy' =  if platformCollision px ps (bx,by) then -bvy else 
                  if colidedBlocks == [] then bvy else 
                  if bx > cbx + blockSizeX/2 || bx < cbx - blockSizeX/2 then bvy else -bvy  
          bvx' =  if colidedBlocks == [] then bvx else
                  if bx > cbx + blockSizeX/2 || bx < cbx - blockSizeX/2 then -bvx
                  else bvx
    -- fazer como o ast2
          colidedBlocks       = [ block | block <- blocks, not (blockCollision (bx,by) block) ]
          blocksWithHP        = [ block | block <- colidedBlocks, blockHasMoreThan1HP block]
          updatedBlocks       = [ updateBlock block | block <- blocksWithHP]
          blocks'             = [ block | block <- blocks, blockCollision (bx,by) block ] ++ updatedBlocks
          score'              = score + length colidedBlocks
          (Block (cbx,_) _) = head colidedBlocks

-- | Check if there was a colision with the plataform.
platformCollision :: PlatformX -> PlatformSize -> Position -> Bool
platformCollision px ps (bx, by) = 
  by > platformPositionY + platformSizeY / 2 && by - ballRadius <= platformPositionY + platformSizeY / 2
  && bx >= px - ps / 2 - ballRadius && bx <= px + ps / 2 + ballRadius

-- | Check colition ball position to block
blockCollision :: Position -> Block -> Bool 
blockCollision (bx,by) (Block (blx,bly) _)  
  = if by + ballRadius > bly - blockSizeY / 2 &&
       by - ballRadius < bly + blockSizeY / 2 &&
       bx + ballRadius > blx - blockSizeX / 2 &&
       bx - ballRadius < blx + blockSizeX / 2 then False else True
hits _ _ = False

-- | Helper function to know if the block has more than 1 health points
blockHasMoreThan1HP :: Block -> Bool
blockHasMoreThan1HP (Block _ hp) = if hp == 1 then False else True

updateBlock :: Block -> Block
updateBlock (Block (blx,bly) hp)  = (Block (blx,bly) hp')
 where hp' = hp - 1 

 --blocksPictures = pictures [translate x y imagem | Block (x,y) _ <- blocks]

updateBallSpeed :: Ball -> Block -> Position
updateBallSpeed ball block  = (1,1)

-- | react to keyboard events
reactToEvent :: Event -> GameState -> GameState
-- In the Victory State:
-- Restart the game
reactToEvent (EventKey (Char 'r') Down _ _) (Victory _ ) = initialWorld
-- Ignore all other keys and events
reactToEvent _ (Victory score ) = (Victory score)

-- In the GameOver State:
-- Restart the game
reactToEvent (EventKey (Char 'r') Down _ _) (GameOver _ _) = initialWorld

-- Ignore all other keys and events
reactToEvent _ (GameOver lvl score) = (GameOver lvl score)

-- In the play State:
-- Lose
reactToEvent (EventKey (Char 'l') Down _ _) game = GameOver lvl score
    where 
      lvl = gameLevel game
      score = gameScore game

-- Remove all Blocks
reactToEvent (EventKey (Char '-') Down _ _) game = game { blocks = [] }

-- Restart
reactToEvent (EventKey (Char 'r') Down _ _) game = initialWorld

-- Pause the game
reactToEvent (EventKey (Char 'p') Down _ _) game = game { gamePause = if gamePause game then False else True }

-- Next Level
reactToEvent (EventKey (Char 'n') Down _ _) game = nextWorld lvl score
    where 
      lvl = gameLevel game + 1
      score = gameScore game

-- Move platform left
reactToEvent (EventKey (SpecialKey KeyLeft) keystate _ _) (Game blocks (Platform px pvx ps) ball gameState lvl pause score)
  = Game blocks (Platform px pvx' ps) ball gameState lvl pause score
  where pvx' = if keystate == Down then pvx - plataformSpeed else pvx + plataformSpeed

-- Move platform right
reactToEvent (EventKey (SpecialKey KeyRight) keystate _ _) (Game blocks (Platform px pvx ps) ball gameState lvl pause score)
  = Game blocks (Platform px pvx' ps) ball gameState lvl pause score
  where pvx' = if keystate == Down then pvx + plataformSpeed else pvx - plataformSpeed

-- ignore all other keys and events
reactToEvent _ world = world


-- | main entry point
main :: IO()
main = do
  play 
    window 
    black 
    fps 
    initialWorld 
    drawWorld 
    reactToEvent
    updateWorld

window :: Display
window = InWindow "BreakOut!" (2*round maxWidth,2*round maxHeight) (0,0)
