module Configuration exposing (levelCount, tickInterval, viewScale, viewWidth, viewHeight)

import Time exposing (Time, millisecond)

tickInterval = 60*millisecond
levelCount = 10

viewScale = 18

viewWidth = 100
viewHeight = 60
