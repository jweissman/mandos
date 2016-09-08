module Configuration exposing (levelCount, tickInterval, viewScale, viewWidth, viewHeight, visionRadius, inventoryLimit, maxRoomWidth, maxRoomHeight, startingHitPoints)

import Time exposing (Time, millisecond)

tickInterval = 60*millisecond
levelCount = 10

viewScale = 18

viewWidth = 100
viewHeight = 60

-- dungeon
maxRoomWidth = 16
maxRoomHeight = 12

-- character
visionRadius = 2
inventoryLimit = 20
startingHitPoints = 14
