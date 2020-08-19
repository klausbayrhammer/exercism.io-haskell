module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = earthYear seconds / planetYearToEarthYear
    where
        planetYearToEarthYear = yearMultiplier planet

earthYear :: Float -> Float
earthYear seconds = seconds / 31557600

yearMultiplier :: Planet -> Float
yearMultiplier Earth = 1
yearMultiplier Mercury = 0.2408467
yearMultiplier Venus = 0.61519726
yearMultiplier Mars = 1.8808158
yearMultiplier Jupiter = 11.862615
yearMultiplier Saturn = 29.447498
yearMultiplier Uranus = 84.016846
yearMultiplier Neptune = 164.79132