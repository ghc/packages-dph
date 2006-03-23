module MapInc 
where

import Data.Array.Parallel.Unlifted

test :: UArr Int -> UArr Int
test = loopArr . loopU (\_ x -> ((), Just $ x + 1 :: Maybe Int)) ()


{- Inner loop:

  $wtrans_sVe =
    \ (ww_sUI :: GHC.Prim.Int#)
      (ww1_sUM :: GHC.Prim.Int#)
      (w_sUO :: ())
      (w1_sUP :: GHC.Prim.State# s_aIR) ->
      case GHC.Prim.==# ww_sUI rb1_aUd of wild12_aHq {
	GHC.Base.False ->
	  case GHC.Prim.writeIntArray#
		 @ s_aIR
		 marr#_aOV
		 ww1_sUM
		 (GHC.Prim.+# (GHC.Prim.indexIntArray# rb2_aUe (GHC.Prim.+# rb_aL4 ww_sUI)) 1)
		 w1_sUP
	  of s2#1_aRq { __DEFAULT ->
	  $wtrans_sVe (GHC.Prim.+# ww_sUI 1) (GHC.Prim.+# ww1_sUM 1) GHC.Base.() s2#1_aRq
	  };
	GHC.Base.True ->
	  case w_sUO of tpl1_aJ1 { () ->
	  (# w1_sUP, (GHC.Base.(), (GHC.Base.I# ww1_sUM)) #)
	  }
      };

-}
