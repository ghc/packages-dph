module PrefixSum
where

import Data.Array.Parallel.Unlifted

test :: UArr Int -> UArr Int
test = loopArr . loopU (\a x -> (a + x, Just a)) 0


{- Inner loop:

  $wtrans_sV2 :: GHC.Prim.Int#
		 -> GHC.Prim.Int#
		 -> GHC.Prim.Int#
		 -> GHC.Prim.State# s_aIq
		 -> (# GHC.Prim.State# s_aIq, (GHC.Base.Int, GHC.Base.Int) #)
  [Arity 4
   Str: DmdType LLLL]
  $wtrans_sV2 =
    \ (ww_sUt :: GHC.Prim.Int#)
      (ww1_sUx :: GHC.Prim.Int#)
      (ww2_sUB :: GHC.Prim.Int#)
      (w_sUD :: GHC.Prim.State# s_aIq) ->
      case GHC.Prim.==# ww_sUt rb1_aU1 of wild12_aH7 {
	GHC.Base.False ->
	  case GHC.Prim.writeIntArray# @ s_aIq marr#_aOv ww1_sUx ww2_sUB w_sUD
	  of s2#1_aRd { __DEFAULT ->
	  $wtrans_sV2
	    (GHC.Prim.+# ww_sUt 1)
	    (GHC.Prim.+# ww1_sUx 1)
	    (GHC.Prim.+#
	       ww2_sUB (GHC.Prim.indexIntArray# rb2_aU2 (GHC.Prim.+# rb_aKE ww_sUt)))
	    s2#1_aRd
	  };
	GHC.Base.True -> (# w_sUD, ((GHC.Base.I# ww2_sUB), (GHC.Base.I# ww1_sUx)) #)
      };

-}
