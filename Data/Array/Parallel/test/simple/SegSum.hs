module SegSum
where

import PArray

test :: SPArr UInt Int -> PArr UInt Int
test =   snd
       . loopArr
       . loopSP (\a x -> (a + x::Int, Nothing::Maybe ())) 
		(\a i -> (a, Just a)) 0

-- Need to recompute the following, as the definition changed.
{- Inner loop:
   [This might look scary, but it seems to be about the optimal Core code
   possible for the performed computation.

   On my x86/RH7.3 system with GHC 5.04, I get a 1876 byte stripped .o, whereas
   the corresponding doubly nested for-loop in C compiles to 516 bytes.]

		    $wtrans1 :: (GHC.Prim.Int#
				 -> GHC.Prim.Int#
				    -> GHC.Prim.Int#
				       -> GHC.Prim.Int#
					  -> GHC.Prim.Int#
					     -> GHC.Prim.State# GHC.Prim.RealWorld
						-> (# GHC.Prim.State# GHC.Prim.RealWorld,
						      (GHC.Base.Int, GHC.Base.Int) #))
		    Arity 6 Just DmdType LLLLLL
		    $wtrans1
		      = \ ww8 :: GHC.Prim.Int#
			  ww9 :: GHC.Prim.Int#
			  ww10 :: GHC.Prim.Int#
			  ww11 :: GHC.Prim.Int#
			  ww12 :: GHC.Prim.Int#
			  w1 :: (GHC.Prim.State# GHC.Prim.RealWorld) ->
			  case ww9 of wild2 {
			    __DEFAULT ->
			      $wtrans1
				(GHC.Prim.+# ww8 1)
				(GHC.Prim.-# wild2 1)
				ww10
				ww11
				(GHC.Prim.+# ww12 (GHC.Prim.indexIntArray# ba# ww8))
				w1;
			    0 ->
			      let {
				$wzdj :: (GHC.Prim.State# GHC.Prim.RealWorld
					  -> GHC.Prim.Int#
					     -> (# GHC.Prim.State# GHC.Prim.RealWorld,
						   (GHC.Base.Int, GHC.Base.Int) #))
				Arity 2 Just DmdType LL
				$wzdj
				  = \ w2 :: (GHC.Prim.State# GHC.Prim.RealWorld)
				      ww13 :: GHC.Prim.Int# ->
				      case GHC.Prim.==# ww10 ww of wild21 {
					GHC.Base.True ->
					  (# w2, ((GHC.Base.$wI# ww13), (GHC.Base.$wI# ww12)) #);
					GHC.Base.False ->
					  case GHC.Prim.+# ww10 1 of wild3 {
					    __DEFAULT ->
					      case GHC.Prim.readIntArray#
						     @ GHC.Prim.RealWorld
						     marr#1
						     (GHC.Prim.-# wild3 1)
						     w2
					      of wild22 { (# s2#3, r# #) ->
					      case GHC.Prim.writeIntArray#
						     @ GHC.Prim.RealWorld marr#1 wild3 r# s2#3
					      of s2#4 { __DEFAULT ->
					      case GHC.Prim.writeIntArray#
						     @ GHC.Prim.RealWorld marr# 0 0 s2#4
					      of s2#5 { __DEFAULT ->
					      $wtrans1
						ww8
						(GHC.Prim.indexIntArray# ww1 wild3)
						wild3
						ww11
						0
						s2#5
					      }
					      }
					      };
					    0 ->
					      case GHC.Prim.writeIntArray#
						     @ GHC.Prim.RealWorld marr#1 0 0 w2
					      of s2#3 { __DEFAULT ->
					      case GHC.Prim.writeIntArray#
						     @ GHC.Prim.RealWorld marr# 0 0 s2#3
					      of s2#4 { __DEFAULT ->
					      $wtrans1
						ww8 (GHC.Prim.indexIntArray# ww1 0) 0 ww11 0 s2#4
					      }
					      }
					  }
				      }
			      } in 
				case ww10 of wild3 {
				  __DEFAULT ->
				    case GHC.Prim.writeIntArray#
					   @ GHC.Prim.RealWorld marr#2 ww11 ww12 w1
				    of s2#3 { __DEFAULT ->
				    $wzdj s2#3 (GHC.Prim.+# ww11 1)
				    };
				  (-1) -> $wzdj w1 ww11
				}
			  };


The matching C routine:

void test (int arr[], int segd[], int n, int m, int out[], int *len)
{
  int acc = 0;
  int arr_i, segd_i, seg_cnt;

  arr_i = 0;
  for (segd_i = 0; segd_i < m; segd_i++) {
    acc = 0;
    for (seg_cnt = segd[segd_i]; seg_cnt == 0; seg_cnt--)
      acc += arr[arr_i++];
    out[segd_i] = acc;
  }
  *len = m;
}

-}
