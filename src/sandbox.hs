f :: Int -> Int
f x = let sq = x * x
          asdf str = putStrLn str
      in (asdf "")
         sq

main = putStrLn $ show $ f 1