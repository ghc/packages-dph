module MapInc 
where

import PArray

test :: PArrInt -> PArrInt
test = loopArr . loopP (\_ x -> ((), Just $ x + 1 :: Maybe Int)) () 


{- Inner loop:

	$wtrans :: (GHC.Prim.Int#
		    -> GHC.Prim.Int#
		       -> ()
			  -> GHC.Prim.State# GHC.Prim.RealWorld
			     -> (# GHC.Prim.State# GHC.Prim.RealWorld, ((), GHC.Base.Int) #))
	Arity 4 Just DmdType LLLL
	$wtrans
	  = \ ww2 :: GHC.Prim.Int#
	      ww3 :: GHC.Prim.Int#
	      w :: ()
	      w1 :: (GHC.Prim.State# GHC.Prim.RealWorld) ->
	      case GHC.Prim.==# ww2 ww of wild {
		GHC.Base.True ->
		  case w of tpl1 { () ->
		  (# w1, (GHC.Base.$w(), (GHC.Base.$wI# ww3)) #)
		  };
		GHC.Base.False ->
		  case GHC.Prim.writeIntArray#
			 @ GHC.Prim.RealWorld
			 marr#
			 ww3
			 (GHC.Prim.+# (GHC.Prim.indexIntArray# ww1 ww2) 1)
			 w1
		  of s2#1 { __DEFAULT ->
		  $wtrans (GHC.Prim.+# ww2 1) (GHC.Prim.+# ww3 1) GHC.Base.$w() s2#1
		  }
	      };

-}
