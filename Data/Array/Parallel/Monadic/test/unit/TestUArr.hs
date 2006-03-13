import PAPrim (ST, runST)
import PABase

replicateU :: MUA e => Int -> e -> UArr e
replicateU n e =
  runST (do
    arr <- newMU n
    fill arr n
    unsafeFreezeMU arr n
  )
  where
    fill arr 0 = return ()
    fill arr i = 
      do
        let i' = i - 1
	writeMU arr i' e
	fill arr i'

sumU :: (Num e, UA e) => UArr e -> e
sumU arr = sumUp (lengthU arr) 0
  where
    sumUp 0 acc = acc
    sumUp i acc = 
      let
        i'   = i - 1
	acc' = acc + arr `indexU` i'
      in
      acc' `seq` sumUp i' acc'

main = print $ sumU (replicateU 5 (10 :: Int))

-- !!!FIXME: GHC 6.5 seems to optimise the writes away...
