module Sum
where

import Data.Array.Parallel.Unlifted

test :: UArr Int -> Int
test = loopAcc . loopU (\a x -> (a + x :*: (Nothing::Maybe ()))) 0


{- Inner loop:

	poly_$wtrans_sPp :: forall s1_aIm.
			    GHC.Prim.Int#
			    -> GHC.Base.Int
			    -> GHC.Prim.Int#
			    -> GHC.Prim.State# s1_aIm
			    -> (# GHC.Prim.State# s1_aIm, (GHC.Base.Int, GHC.Base.Int) #)
	[Arity 4]
	poly_$wtrans_sPp =
	  \ (@ s1_XJ3)
	    (ww_XPz :: GHC.Prim.Int#)
	    (w_XPC :: GHC.Base.Int)
	    (ww1_XPG :: GHC.Prim.Int#)
	    (w1_XPJ :: GHC.Prim.State# s1_XJ3) ->
	    case GHC.Prim.==# ww_XPz wild11_B1 of wild2_XHP {
	      GHC.Base.False ->
		poly_$wtrans_sPp
		  @ s1_XJ3
		  (GHC.Prim.+# ww_XPz 1)
		  w_XPC
		  (GHC.Prim.+#
		     ww1_XPG (GHC.Prim.indexIntArray# rb2_aPT (GHC.Prim.+# rb_aKA ww_XPz)))
		  w1_XPJ;
	      GHC.Base.True ->
		case w_XPC of tpl_aIu { GHC.Base.I# a1_aIv ->
		(# w1_XPJ, ((GHC.Base.I# ww1_XPG), tpl_aIu) #)
		}
	    };

-}
