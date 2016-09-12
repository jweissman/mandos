module Configuration exposing (levelCount, tickInterval, viewScale, viewWidth, viewHeight, visionRadius, inventoryLimit, maxRoomWidth, maxRoomHeight, startingHitPoints, startingStrength, candidateRooms)

import Time exposing (Time, millisecond)

tickInterval = 25*millisecond
levelCount = 10

viewScale = 16
viewWidth = 90
viewHeight = 56

-- dungeon
candidateRooms = 500
maxRoomWidth = 14
maxRoomHeight = 10

-- character
visionRadius = 4
inventoryLimit = 20
startingHitPoints = 35
startingStrength  = 5
