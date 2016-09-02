module Configuration exposing (levelCount, tickInterval, viewScale, viewWidth, viewHeight, visionRadius, inventoryLimit, maxRoomWidth, maxRoomHeight)

import Time exposing (Time, millisecond)

tickInterval = 60*millisecond
levelCount = 10

viewScale = 18

viewWidth = 100
viewHeight = 60

-- dungeon
maxRoomWidth = 15
maxRoomHeight = 8

-- character
visionRadius = 3
inventoryLimit = 10
