module DotProd
where

import PArray

test :: PArr UFloat Float -> PArr UFloat Float -> Float
test v w =   loopAcc
           . loopP (\a (x, y) -> (a + x * y, Nothing::Maybe ())) 0
	   $ zipP v w


{- Inner loop:

	    $wtrans
	      = \ ww3 :: GHC.Prim.Int#
		  w2 :: GHC.Base.Int
		  ww4 :: GHC.Prim.Float#
		  w3 :: (GHC.Prim.State# GHC.Prim.RealWorld) ->
		  case GHC.Prim.==# ww3 wild of wild2 {
		    GHC.Base.True ->
		      case w2 of tpl { GHC.Base.I# a1 ->
		      (# w3, ((GHC.Float.$wF# ww4), tpl) #)
		      };
		    GHC.Base.False ->
		      $wtrans
			(GHC.Prim.+# ww3 1)
			w2
			(GHC.Prim.plusFloat#
			   ww4
			   (GHC.Prim.timesFloat#
			      (GHC.Prim.indexFloatArray# ww1 ww3)
			      (GHC.Prim.indexFloatArray# ba# ww3)))
			w3
		  };

-}
