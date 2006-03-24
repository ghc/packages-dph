module SegSum
where

import Data.Array.Parallel.Unlifted

test :: UArr (UArr Int) -> UArr Int
test =   snd
       . loopArr
       . loopSU (\a x -> (a + x::Int, Nothing::Maybe ())) 
		(\a i -> (a, Just a)) 0

{- Inner loop:

  FIXME: This is strange.  The case of `ds1_aJu' has a pattern match failure
         __DEFAULT (as has the following case), but it shouldn't as the GADT
         can really only have this one variant at the given type.

     $wtrans_s19q :: GHC.Prim.Int#
		     -> GHC.Prim.Int#
		     -> GHC.Prim.Int#
		     -> GHC.Prim.Int#
		     -> GHC.Prim.Int#
		     -> GHC.Prim.State# s_aLy
		     -> (# GHC.Prim.State# s_aLy, (GHC.Base.Int, GHC.Base.Int) #)
     [Arity 6
      Str: DmdType LLLLLL]
     $wtrans_s19q =
       \ (ww_X19I :: GHC.Prim.Int#)
	 (ww1_X19N :: GHC.Prim.Int#)
	 (ww2_X19S :: GHC.Prim.Int#)
	 (ww3_X19X :: GHC.Prim.Int#)
	 (ww4_X1a2 :: GHC.Prim.Int#)
	 (w_X1a5 :: GHC.Prim.State# s_aLy) ->
	 case ww1_X19N of wild3_X1p {
	   __DEFAULT ->
	     case ds1_aJu of wild4_aTe {
	       __DEFAULT ->
		 __coerce (# GHC.Prim.State# s_aLy, (GHC.Base.Int, GHC.Base.Int) #) fail6_r1ae;
	       Data.Array.Parallel.Monadic.UArr.UAPrim ds11_aTd ->
		 case ds11_aTd of wild14_aTi {
		   __DEFAULT ->
		     __coerce (# GHC.Prim.State# s_aLy, (GHC.Base.Int, GHC.Base.Int) #)
		     fail6_r1ae;
		   Data.Array.Parallel.Base.Prim.PrimInt rb22_aTh rb13_aTj rb23_aTk ->
		     $wtrans_s19q
		       (GHC.Prim.+# ww_X19I 1)
		       (GHC.Prim.-# wild3_X1p 1)
		       ww2_X19S
		       ww3_X19X
		       (GHC.Prim.+#
			  ww4_X1a2
			  (GHC.Prim.indexIntArray# rb23_aTk (GHC.Prim.+# rb22_aTh ww_X19I)))
		       w_X1a5
		 }
	     };
	   0 ->
	     let {
	       $w$j_X1aV :: GHC.Prim.State# s_aLy
			    -> GHC.Prim.Int#
			    -> GHC.Prim.Int#
			    -> (# GHC.Prim.State# s_aLy, (GHC.Base.Int, GHC.Base.Int) #)
	       [Arity 3
		Str: DmdType LLL]
	       $w$j_X1aV =
		 \ (w1_X19C :: GHC.Prim.State# s_aLy)
		   (ww5_X1ba :: GHC.Prim.Int#)
		   (ww6_X1bg :: GHC.Prim.Int#) ->
		   let {
		     a_X112 [Just L] :: GHC.Prim.Int#
		     [Str: DmdType]
		     a_X112 = GHC.Prim.+# ww2_X19S 1
		   } in 
		     case GHC.Prim.==# a_X112 wild13_B1 of wild31_XN8 {
		       GHC.Base.False ->
			 case a_X112 of wild4_X2J {
			   __DEFAULT ->
			     case GHC.Prim.readIntArray#
				    @ s_aLy rb31_aWu (GHC.Prim.-# wild4_X2J 1) w1_X19C
			     of wild21_aX8 { (# s2#5_aXa, r#_aXb #) ->
			     case GHC.Prim.readIntArray#
				    @ s_aLy rb12_aWs (GHC.Prim.-# wild4_X2J 1) s2#5_aXa
			     of wild22_XZs { (# s2#6_XZv, r#1_XZx #) ->
			     case GHC.Prim.writeIntArray#
				    @ s_aLy
				    rb31_aWu
				    wild4_X2J
				    (GHC.Prim.+# r#_aXb r#1_XZx)
				    s2#6_XZv
			     of s2#7_XXe { __DEFAULT ->
			     case GHC.Prim.writeIntArray# @ s_aLy rb12_aWs wild4_X2J 0 s2#7_XXe
			     of s2#8_XZu { __DEFAULT ->
			     $wtrans_s19q
			       ww_X19I
			       (GHC.Prim.indexIntArray# rb2_aJq (GHC.Prim.+# rb_aIN wild4_X2J))
			       wild4_X2J
			       ww5_X1ba
			       ww6_X1bg
			       s2#8_XZu
			     }
			     }
			     }
			     };
			   0 ->
			     case GHC.Prim.writeIntArray# @ s_aLy rb31_aWu 0 0 w1_X19C
			     of s2#5_XX6 { __DEFAULT ->
			     case GHC.Prim.writeIntArray# @ s_aLy rb12_aWs 0 0 s2#5_XX6
			     of s2#6_XZm { __DEFAULT ->
			     $wtrans_s19q
			       ww_X19I
			       (GHC.Prim.indexIntArray# rb2_aJq rb_aIN)
			       0
			       ww5_X1ba
			       ww6_X1bg
			       s2#6_XZm
			     }
			     }
			 };
		       GHC.Base.True ->
			 (# w1_X19C, ((GHC.Base.I# ww5_X1ba), (GHC.Base.I# ww6_X1bg)) #)
		     }
	     } in 
	       case ww2_X19S of wild4_X1P {
		 __DEFAULT ->
		   case GHC.Prim.writeIntArray# @ s_aLy marr#2_XQe ww3_X19X ww4_X1a2 w_X1a5
		   of s2#5_XWX { __DEFAULT ->
		   $w$j_X1aV s2#5_XWX (GHC.Prim.+# ww3_X19X 1) ww4_X1a2
		   };
		 (-1) -> $w$j_X1aV w_X1a5 ww3_X19X ww4_X1a2
	       }
	 };

 --------------- OLD ------------------

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
