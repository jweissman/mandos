module Configuration exposing (levelCount, tickInterval, visionRadius, inventoryLimit, maxRoomWidth, maxRoomHeight, startingHitPoints, startingStrength, candidateRooms, viewWidth, viewHeight)

import Time exposing (Time, millisecond)

tickInterval = 25*millisecond
levelCount = 10

--viewScale = 16
viewWidth = 60
viewHeight = 40

-- dungeon
candidateRooms = 500
maxRoomWidth = 14
maxRoomHeight = 10

-- character
visionRadius = 4
inventoryLimit = 14
startingHitPoints = 35
startingStrength  = 5
