module Configuration exposing (levelCount, tickInterval, viewScale, viewWidth, viewHeight, visionRadius, inventoryLimit)

import Time exposing (Time, millisecond)

tickInterval = 60*millisecond
levelCount = 10

viewScale = 18

viewWidth = 100
viewHeight = 60

visionRadius = 3
inventoryLimit = 10
