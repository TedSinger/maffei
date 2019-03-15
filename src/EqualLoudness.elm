module EqualLoudness exposing (fletcherMunson, linearInterpolate)


linearInterpolate : Float -> List ( Float, Float ) -> Float
linearInterpolate freq lc =
    case lc of
        ( headFreq, headGain ) :: ( nextFreq, nextGain ) :: tail ->
            if headFreq > freq then
                headGain

            else if nextFreq > freq then
                let
                    slope =
                        (nextGain - headGain) / (nextFreq - headFreq)
                in
                slope * (freq - headFreq) + headGain

            else
                linearInterpolate freq <| ( nextFreq, nextGain ) :: tail

        ( lastFreq, lastGain ) :: [] ->
            lastGain

        [] ->
            0

-- Originally sketched from https://en.wikipedia.org/wiki/Fletcher%E2%80%93Munson_curves#/media/File:Lindos4.svg
-- Munged from dB SPL to gain, wrongly
-- Assume these numbers are completely devoid of any theoretical justification
-- To my ear, they sound right.
fletcherMunson : List ( Float, Float )
fletcherMunson =
    [ ( 35, 1 )
    , ( 110, 0.883 )
    , ( 116.5, 0.826 )
    , ( 123.5, 0.771 )
    , ( 130.8, 0.726 )
    , ( 138.6, 0.681 )
    , ( 146.8, 0.642 )
    , ( 155.6, 0.607 )
    , ( 164.8, 0.573 )
    , ( 174.6, 0.543 )
    , ( 185.0, 0.514 )
    , ( 196.0, 0.489 )
    , ( 207.7, 0.465 )
    , ( 220, 0.443 )
    , ( 233.1, 0.423 )
    , ( 246.9, 0.403 )
    , ( 261.6, 0.387 )
    , ( 277.2, 0.371 )
    , ( 293.7, 0.357 )
    , ( 311.1, 0.344 )
    , ( 329.6, 0.332 )
    , ( 349.2, 0.322 )
    , ( 370.0, 0.312 )
    , ( 392.0, 0.307 )
    , ( 415.3, 0.302 )
    , ( 440, 0.298 )
    , ( 466.2, 0.295 )
    , ( 493.9, 0.293 )
    , ( 523.3, 0.291 )
    , ( 554.4, 0.289 )
    , ( 587.3, 0.288 )
    , ( 622.3, 0.287 )
    , ( 659.3, 0.286 )
    , ( 698.5, 0.287 )
    , ( 740.0, 0.287 )
    , ( 784.0, 0.289 )
    , ( 830.6, 0.291 )
    , ( 880, 0.295 )
    , ( 932.3, 0.3 )
    , ( 987.8, 0.305 )
    , ( 1046.5, 0.306 )
    , ( 1108.7, 0.307 )
    , ( 1174.7, 0.304 )
    , ( 1244.5, 0.3 )
    , ( 1318.5, 0.296 )
    , ( 1396.9, 0.29 )
    , ( 1480.0, 0.283 )
    , ( 1568.0, 0.272 )
    , ( 1661.2, 0.26 )
    , ( 1760, 0.247 )
    , ( 1864.7, 0.232 )
    , ( 1975.5, 0.22 )
    , ( 2093.0, 0.21 )
    , ( 2217.5, 0.2 )
    , ( 2349.3, 0.195 )
    , ( 2489.0, 0.189 )
    , ( 2637.0, 0.186 )
    , ( 2793.8, 0.183 )
    , ( 2960.0, 0.181 )
    , ( 3136.0, 0.182 )
    , ( 3322.4, 0.183 )
    , ( 3520, 0.186 )
    ]
