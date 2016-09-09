module Configuration exposing (levelCount, tickInterval, viewScale, viewWidth, viewHeight, visionRadius, inventoryLimit, maxRoomWidth, maxRoomHeight, startingHitPoints, startingStrength)

import Time exposing (Time, millisecond)

tickInterval = 60*millisecond
levelCount = 20

viewScale = 18

viewWidth = 100
viewHeight = 60

-- dungeon
maxRoomWidth = 12
maxRoomHeight = 10

-- character
visionRadius = 4
inventoryLimit = 24
startingHitPoints = 35
startingStrength  = 5
