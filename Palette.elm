module Palette exposing (..)

warning  = "red"
alert    = "yellow"
info     = bright

active   = bright --tertiaryLighter
inactive = "darkgrey" --"darkgrey"
default  = "lightgrey" --secondaryLight

bright   = "white" --secondaryLighter --"white"
dim      = "lightgray" --"lightgrey"
dark     = "darkgrey" --secondaryLight --"darkgrey" --tertiaryDark

primary' shade alpha = rgba (purple' shade alpha)
primaryLighter = rgb (purple 0)
primaryLight   = rgb (purple 1)
primary        = rgb (purple 2)
primaryDark    = rgb (purple 3)
primaryDarker  = rgb (purple 4)

secondary' shade alpha = rgba (yellow' shade alpha)
secondaryLighter = rgb (yellow 0)
secondaryLight   = rgb (yellow 1)
secondary        = rgb (yellow 2)
secondaryDark    = rgb (yellow 3)
secondaryDarker  = rgb (yellow 4)

tertiary' shade alpha = rgba (blue' shade alpha)
tertiaryLighter  = rgb (blue 0)
tertiaryLight    = rgb (blue 1)
tertiary         = rgb (blue 2)
tertiaryDark     = rgb (blue 3)
tertiaryDarker   = rgb (blue 4)

accent' shade alpha = rgba (green' shade alpha)
accentLighter = rgb (green 0)
accentLight   = rgb (green 1)
accent        = rgb (green 2)
accentDark    = rgb (green 3)
accentDarker  = rgb (green 4)

rgb : (Int, Int, Int) -> String
rgb (r, g, b) =
  "rgb(" 
  ++ (toString r) 
  ++ ", " 
  ++ (toString g) 
  ++ ", " 
  ++ (toString b) 
  ++ ")"

rgba : (Int, Int, Int, Float) -> String
rgba (r, g, b, a) =
  "rgba(" 
  ++ (toString r) 
  ++ ", " 
  ++ (toString g) 
  ++ ", " 
  ++ (toString b) 
  ++ ", " 
  ++ (toString a) 
  ++ ")"


purple' shade alpha =
  let (r,g,b) = purple shade in
  (r,g,b,alpha)

yellow' shade alpha =
  let (r,g,b) = yellow shade in
  (r,g,b,alpha)

blue' shade alpha =
  let (r,g,b) = blue shade in
  (r,g,b,alpha)

green' shade alpha =
  let (r,g,b) = green shade in
  (r,g,b,alpha)

purple : Int -> (Int,Int,Int)
purple shade =
  case shade of
   2 -> (154, 25,103)
   0 -> (233,179,212)
   1 -> (190, 77,145)
   3 -> (106,  0, 64)
   _ -> ( 35,  0, 21)

yellow : Int -> (Int, Int, Int)
yellow shade =
  case shade of
   2 -> (196,173, 32)
   0 -> (255,247,195)
   1 -> (241,221, 97)
   3 -> (134,116,  0)
   _ -> ( 44, 38,  0)

blue : Int -> (Int, Int, Int)
blue shade =
  case shade of
   2 -> ( 65, 33,134)
   0 -> (191,176,223)
   1 -> (104, 77,165)
   3 -> ( 35,  9, 92)
   _ -> ( 10,  1, 30)
green : Int -> (Int, Int, Int)
green shade =
  case shade of
   2 -> (136,184, 30)
   0 -> (231,249,190)
   1 -> (184,226, 91)
   3 -> ( 87,126,  0)
   _ -> ( 28, 41,  0)

