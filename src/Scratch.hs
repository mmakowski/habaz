data IntConsumer = IntConsumer (Int -> IO IntConsumer)

c :: Int -> IO IntConsumer
c i = return $ IntConsumer $ \j -> do
  print i
  c (i + j)

s :: String -> IO IntConsumer
s str = return $ IntConsumer $ \i -> do
  print str
  s (str ++ show i)

go c i = do
  (IntConsumer c1) <- c i
  (IntConsumer c2) <- c1 2
  (IntConsumer c3) <- c2 3
  return ()

