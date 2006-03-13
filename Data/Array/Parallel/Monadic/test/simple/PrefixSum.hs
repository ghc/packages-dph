module PrefixSum
where

import PArray

test :: PArrInt -> Int
test = loopAcc . loopP (\a x -> (a + x, Just a)) 0


{- Inner loop:

	$wtrans :: (GHC.Prim.Int#
		    -> GHC.Prim.Int#
		       -> GHC.Prim.Int#
			  -> GHC.Prim.State# GHC.Prim.RealWorld
			     -> (# GHC.Prim.State# GHC.Prim.RealWorld,
				   (GHC.Base.Int, GHC.Base.Int) #))
	Arity 4 Just DmdType LLLL
	$wtrans
	  = \ ww2 :: GHC.Prim.Int#
	      ww3 :: GHC.Prim.Int#
	      ww4 :: GHC.Prim.Int#
	      w :: (GHC.Prim.State# GHC.Prim.RealWorld) ->
	      case GHC.Prim.==# ww2 ww of wild {
		GHC.Base.True ->
		  (# w, ((GHC.Base.$wI# ww4), (GHC.Base.$wI# ww3)) #);
		GHC.Base.False ->
		  case GHC.Prim.writeIntArray# @ GHC.Prim.RealWorld marr# ww3 ww4 w
		  of s2#1 { __DEFAULT ->
		  $wtrans
		    (GHC.Prim.+# ww2 1)
		    (GHC.Prim.+# ww3 1)
		    (GHC.Prim.+# ww4 (GHC.Prim.indexIntArray# ww1 ww2))
		    s2#1
		  }
	      };

-}
