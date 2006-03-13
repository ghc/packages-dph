module Sum
where

import PArray

test :: PArrInt -> Int
test = loopAcc . loopP (\a x -> (a + x, Nothing::Maybe ())) 0


{- Inner loop:

	$wtrans :: (GHC.Prim.Int#
		    -> GHC.Base.Int
		       -> GHC.Prim.Int#
			  -> GHC.Prim.State# GHC.Prim.RealWorld
			     -> (# GHC.Prim.State# GHC.Prim.RealWorld,
				   (GHC.Base.Int, GHC.Base.Int) #))
	Arity 4 Just DmdType LS(A)LL
	$wtrans
	  = \ ww2 :: GHC.Prim.Int#
	      w :: GHC.Base.Int
	      ww3 :: GHC.Prim.Int#
	      w1 :: (GHC.Prim.State# GHC.Prim.RealWorld) ->
	      case GHC.Prim.==# ww2 ww of wild {
		GHC.Base.True ->
		  case w of tpl { GHC.Base.I# a ->
		  (# w1, ((GHC.Base.$wI# ww3), tpl) #)
		  };
		GHC.Base.False ->
		  $wtrans
		    (GHC.Prim.+# ww2 1)
		    w
		    (GHC.Prim.+# ww3 (GHC.Prim.indexIntArray# ww1 ww2))
		    w1
	      };
      } in 

-}
