module Palette exposing (..)

warning  = "red"
alert    = "yellow"
info     = bright

default  = dark
active   = bright --tertiaryLight
inactive = dim --"darkgrey"

bright   = "white"
dim      = "lightgrey"
dark     = "darkgrey" --tertiaryDark

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
    0 -> (228,175,199)
    1 -> (197,113,151)
    2 -> (155, 58,102)
    3 -> (115, 22, 64)
    _ -> ( 70,  0, 32)

yellow : Int -> (Int, Int, Int)
yellow shade =
  case shade of
    0 -> (255,247,196)
    1 -> (235,221,134)
    2 -> (185,168, 69)
    3 -> (137,121, 26)
    _ -> ( 83, 71,  0)

blue : Int -> (Int, Int, Int)
blue shade =
  case shade of
    0 -> (173,159,201)
    1 -> (118, 99,161)
    2 -> ( 78, 55,126)
    3 -> ( 47, 25, 94)
    _ -> ( 22,  5, 57)

green : Int -> (Int, Int, Int)
green shade =
  case shade of
    0 -> (218,241,185)
    1 -> (178,216,123)
    2 -> (126,170, 63)
    3 -> ( 84,126, 24)
    _ -> ( 45, 76,  0)

