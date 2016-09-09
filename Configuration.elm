module Configuration exposing (levelCount, tickInterval, viewScale, viewWidth, viewHeight, visionRadius, inventoryLimit, maxRoomWidth, maxRoomHeight, startingHitPoints)

import Time exposing (Time, millisecond)

tickInterval = 60*millisecond
levelCount = 1

viewScale = 18

viewWidth = 100
viewHeight = 60

-- dungeon
maxRoomWidth = 20
maxRoomHeight = 14

-- character
visionRadius = 5
inventoryLimit = 20
startingHitPoints = 140
