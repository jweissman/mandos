module Configuration exposing (levelCount, tickInterval, viewScale, viewWidth, viewHeight, visionRadius, inventoryLimit, maxRoomWidth, maxRoomHeight, startingHitPoints, startingStrength, candidateRooms)

import Time exposing (Time, millisecond)

tickInterval = 60*millisecond
levelCount = 10

viewScale = 16

viewWidth = 90
viewHeight = 56

-- dungeon
candidateRooms = 75
maxRoomWidth = 18
maxRoomHeight = 14

-- character
visionRadius = 5
inventoryLimit = 20
startingHitPoints = 35
startingStrength  = 5
