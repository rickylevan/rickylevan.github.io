import Color
import Graphics.Collage
import Graphics.Element exposing (Element)
import Keyboard
import Text
import Time
import Window

(gameWidth, gameHeight) = (500, 500)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)

type alias Ball = 
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , r : Float
    , c : Color.Color
    }

defaultBall1 = 
    { x = 0
    , y = 0
    , dx = 184
    , dy = 135
    , r = 25
    , c = lightBlue
    }

defaultBall2 = 
    { x = 80
    , y = 0
    , dx = 84
    , dy = 235
    , r = 20
    , c = lightRed
    }

defaultBall3 = 
    { x = -80
    , y = 0
    , dx = -300
    , dy = 435
    , r = 15
    , c = lightGreen
    }

defaultBalls = [defaultBall1, defaultBall2, defaultBall3]

white = Color.rgb 255 255 255
darkBlue = Color.rgb 0 0 70
lightBlue = Color.rgb 160 160 255
lightRed = Color.rgb 255 160 160
lightGreen = Color.rgb 160 255 160

view: (Int, Int) -> List Ball -> Element
view (w,h) ballList  = 
    Graphics.Element.container w h Graphics.Element.middle <|
    Graphics.Collage.collage gameWidth gameHeight
    ([ Graphics.Collage.rect gameWidth gameHeight
        |> Graphics.Collage.filled darkBlue
     ] ++ (viewBalls ballList))

viewBalls : List Ball -> List Graphics.Collage.Form
viewBalls ballList = 
    let f ball = Graphics.Collage.circle ball.r
        |> Graphics.Collage.filled ball.c
        |> Graphics.Collage.move (ball.x, ball.y)
    in List.map f ballList


update : Time.Time -> List Ball -> List Ball
update delta ballList =
    List.map (updateBall delta) ballList

updateBall : Time.Time -> Ball -> Ball
updateBall delta ball = 
    let 
        rightBound = ball.x + ball.r
        leftBound = ball.x - ball.r
        topBound = ball.y + ball.r
        bottomBound = ball.y - ball.r
        rightFlab = max 0 (rightBound - halfWidth)
        leftFlab = min 0 (leftBound + halfWidth)
        topFlab = max 0 (topBound - halfWidth)
        bottomFlab = min 0 (bottomBound + halfWidth)
        xVelFlip = if (rightFlab > 0 || leftFlab < 0) then -1 else 1
        yVelFlip = if (topFlab > 0 || bottomFlab < 0) then -1 else 1
    in
        {ball | 
        x = ball.x + xVelFlip*ball.dx*delta - 2*(rightFlab+leftFlab),
        y = ball.y + yVelFlip*ball.dy*delta - 2*(topFlab+bottomFlab),
        dx = xVelFlip * ball.dx,
        dy = yVelFlip * ball.dy
        }


delta = Signal.map Time.inSeconds (Time.fps 60)

ballStream : Signal (List Ball)
ballStream = Signal.foldp update defaultBalls delta

main = 
    Signal.map2 view Window.dimensions ballStream
