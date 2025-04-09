main :: IO ()
main = do
    putStrLn "Welcome!"
    let satirlar =
            [ "X A - - X"
            , "B - - - Z"
            , "X C - - X"
            ]
    mapM_ putStrLn satirlar