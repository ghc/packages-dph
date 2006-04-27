module DotProd
where

import Data.Array.Parallel.Unlifted

test :: UArr Float -> UArr Float -> Float
test v w =   loopAcc
           . loopU (\a (x:*:y) -> (a + x * y, Nothing::Maybe ())) 0
	   $ zipU v w


{- Inner loop:

      poly_$wtrans_s15C :: forall s1_aZb.
			   GHC.Prim.Int#
			   -> GHC.Base.Int
			   -> GHC.Prim.Float#
			   -> GHC.Prim.State# s1_aZb
			   -> (# GHC.Prim.State# s1_aZb, (GHC.Float.Float, GHC.Base.Int) #)
      [Arity 4]
      poly_$wtrans_s15C =
	\ (@ s1_X10d)
	  (ww_X164 :: GHC.Prim.Int#)
	  (w1_X167 :: GHC.Base.Int)
	  (ww1_X16b :: GHC.Prim.Float#)
	  (w2_X16e :: GHC.Prim.State# s1_X10d) ->
	  case GHC.Prim.==# ww_X164 wild2_B1 of wild4_XVx {
	    GHC.Base.False ->
	      poly_$wtrans_s15C
		@ s1_X10d
		(GHC.Prim.+# ww_X164 1)
		w1_X167
		(GHC.Prim.plusFloat#
		   ww1_X16b
		   (GHC.Prim.timesFloat#
		      (GHC.Prim.indexFloatArray# rb2_aXC (GHC.Prim.+# rb_aXx ww_X164))
		      (GHC.Prim.indexFloatArray# rb21_X11Y (GHC.Prim.+# rb11_X11T ww_X164))))
		w2_X16e;
	    GHC.Base.True ->
	      case w1_X167 of tpl_aZj { GHC.Base.I# a1_aZk ->
	      (# w2_X16e, ((GHC.Float.F# ww1_X16b), tpl_aZj) #)
	      }
	  };
    } in 

-}
