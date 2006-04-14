module SegPrefixSum
where

import Data.Array.Parallel.Unlifted

test :: UArr (UArr Int) -> UArr (UArr Int)
test =   fst
       . loopArr
       . loopSU (\a x -> (a + x::Int, Just a)) 
		(\a i -> (a, Nothing :: Maybe ())) 0


{- Inner loop:

	$wtrans :: (GHC.Prim.Int#
		    -> GHC.Prim.Int#
		       -> GHC.Prim.Int#
			  -> GHC.Prim.Int#
			     -> GHC.Prim.Int#
				-> GHC.Prim.State# GHC.Prim.RealWorld
				   -> (# GHC.Prim.State# GHC.Prim.RealWorld,
					 (GHC.Base.Int, GHC.Base.Int) #))
	Arity 6 Just DmdType LLLLLL
	$wtrans
	  = \ ww4 :: GHC.Prim.Int#
	      ww5 :: GHC.Prim.Int#
	      ww6 :: GHC.Prim.Int#
	      ww7 :: GHC.Prim.Int#
	      ww8 :: GHC.Prim.Int#
	      w :: (GHC.Prim.State# GHC.Prim.RealWorld) ->
	      case ww5 of wild {
		__DEFAULT ->
		  case GHC.Prim.readIntArray# @ GHC.Prim.RealWorld marr#1 ww6 w
		  of wild2 { (# s2#4, r# #) ->
		  case GHC.Prim.readIntArray# @ GHC.Prim.RealWorld marr# ww6 s2#4
		  of wild21 { (# s2#5, r#1 #) ->
		  case GHC.Prim.writeIntArray#
			 @ GHC.Prim.RealWorld marr#2 r# ww8 s2#5
		  of s2#6 { __DEFAULT ->
		  case GHC.Prim.writeIntArray#
			 @ GHC.Prim.RealWorld marr#1 ww6 (GHC.Prim.+# r# 1) s2#6
		  of s2#7 { __DEFAULT ->
		  case GHC.Prim.writeIntArray#
			 @ GHC.Prim.RealWorld marr# ww6 (GHC.Prim.+# r#1 1) s2#7
		  of s2#8 { __DEFAULT ->
		  $wtrans
		    (GHC.Prim.+# ww4 1)
		    (GHC.Prim.-# wild 1)
		    ww6
		    ww7
		    (GHC.Prim.+# ww8 (GHC.Prim.indexIntArray# ww3 ww4))
		    s2#8
		  }
		  }
		  }
		  }
		  };
		0 ->
		  let {
		    $wzdj :: (GHC.Prim.State# GHC.Prim.RealWorld
			      -> GHC.Prim.Int#
				 -> (# GHC.Prim.State# GHC.Prim.RealWorld,
				       (GHC.Base.Int, GHC.Base.Int) #))
		    Arity 2 Just DmdType LL
		    $wzdj
		      = \ w1 :: (GHC.Prim.State# GHC.Prim.RealWorld)
			  ww9 :: GHC.Prim.Int# ->
			  case GHC.Prim.==# ww6 ww of wild2 {
			    GHC.Base.True ->
			      (# w1, ((GHC.Base.$wI# ww9), (GHC.Base.$wI# ww8)) #);
			    GHC.Base.False ->
			      case GHC.Prim.+# ww6 1 of wild14 {
				__DEFAULT ->
				  case GHC.Prim.readIntArray#
					 @ GHC.Prim.RealWorld marr#1 (GHC.Prim.-# wild14 1) w1
				  of wild21 { (# s2#4, r# #) ->
				  case GHC.Prim.writeIntArray#
					 @ GHC.Prim.RealWorld marr#1 wild14 r# s2#4
				  of s2#5 { __DEFAULT ->
				  case GHC.Prim.writeIntArray# @ GHC.Prim.RealWorld marr# 0 0 s2#5
				  of s2#6 { __DEFAULT ->
				  $wtrans ww4 (GHC.Prim.indexIntArray# ww1 wild14) wild14 ww7 0 s2#6
				  }
				  }
				  };
				0 ->
				  case GHC.Prim.writeIntArray# @ GHC.Prim.RealWorld marr#1 0 0 w1
				  of s2#4 { __DEFAULT ->
				  case GHC.Prim.writeIntArray# @ GHC.Prim.RealWorld marr# 0 0 s2#4
				  of s2#5 { __DEFAULT ->
				  $wtrans ww4 (GHC.Prim.indexIntArray# ww1 0) 0 ww7 0 s2#5
				  }
				  }
			      }
			  }
		  } in 
		    case ww6 of wild14 {
		      __DEFAULT ->
			case GHC.Prim.writeIntArray# @ GHC.Prim.RealWorld marr#3 ww7 ww8 w
			of s2#4 { __DEFAULT ->
			$wzdj s2#4 (GHC.Prim.+# ww7 1)
			};
		      (-1) -> $wzdj w ww7
		    }
	      };

-}
