import Data.Array.Parallel.Base.UArr

replicateBU_test :: UAE e => Int -> e -> BUArr e
replicateBU_test n e =
  runST (do
    arr <- newMBU n
    fill arr n
    unsafeFreezeMBU arr n
  )
  where
    fill arr 0 = return ()
    fill arr i = 
      do
        let i' = i - 1
	writeMBU arr i' e
	fill arr i'


main = print $ sumBU (replicateBU_test 5 (10 :: Int))
