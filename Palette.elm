module Palette exposing (..)

primary' shade alpha = rgba (purple' shade alpha)
primary        = rgb (purple 0)
primaryLighter   = rgb (purple 1)
primaryLight = rgb (purple 2)
primaryDark    = rgb (purple 3)
primaryDarker  = rgb (purple 4)

secondary' shade alpha = rgba (yellow' shade alpha)
secondary        = rgb (yellow 0)
secondaryLighter   = rgb (yellow 1)
secondaryLight = rgb (yellow 2)
secondaryDark    = rgb (yellow 3)
secondaryDarker  = rgb (yellow 4)

tertiary' shade alpha = rgba (blue' shade alpha)
tertiary         = rgb (blue 0)
tertiaryLighter    = rgb (blue 1)
tertiaryLight  = rgb (blue 2)
tertiaryDark     = rgb (blue 3)
tertiaryDarker   = rgb (blue 4)

accent        = rgb (green 0)
accent' shade alpha = rgba (green' shade alpha)
accentLighter   = rgb (green 1)
accentLight = rgb (green 2)
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
   0 -> ( 97, 52, 97)
   1 -> (181,157,181)
   2 -> (130, 88,130)
   3 -> ( 72, 25, 72)
   _ -> ( 40,  3, 40)


yellow : Int -> (Int, Int, Int)
yellow shade =
  case shade of
    0 -> (150,127, 80)
    1 -> (255,244,221)
    2 -> (200,179,136)
    3 -> (111, 87, 38)
    _ -> ( 61, 43,  5)

blue : Int -> (Int, Int, Int)
blue shade =
  case shade of
    0 -> ( 59, 70,101)
    1 -> (164,170,186)
    2 -> ( 96,107,134)
    3 -> ( 31, 43, 74)
    _ -> (  7, 17, 41)

green : Int -> (Int, Int, Int)
green shade =
  case shade of
    0 -> (133,145, 78)
    1 -> (242,248,215)
    2 -> (182,193,131)
    3 -> ( 95,107, 37)
    _ -> ( 50, 59,  5)
