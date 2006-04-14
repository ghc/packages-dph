module SegSum
where

import Data.Array.Parallel.Unlifted

test :: UArr (UArr Int) -> UArr Int
test =   snd
       . loopArr
       . loopSU (\a x -> (a + x::Int, Nothing::Maybe ())) 
		(\a i -> (a, Just a)) 0

{- Inner loop:

     $wtrans_s19S :: GHC.Prim.Int#
		     -> GHC.Prim.Int#
		     -> GHC.Prim.Int#
		     -> GHC.Prim.Int#
		     -> GHC.Prim.Int#
		     -> GHC.Prim.State# s_aLn
		     -> (# GHC.Prim.State# s_aLn, (GHC.Base.Int, GHC.Base.Int) #)
     [Arity 6
      Str: DmdType LLLLLL]
     $wtrans_s19S =
       \ (ww_s18Y :: GHC.Prim.Int#)
	 (ww1_s192 :: GHC.Prim.Int#)
	 (ww2_s196 :: GHC.Prim.Int#)
	 (ww3_s19a :: GHC.Prim.Int#)
	 (ww4_s19e :: GHC.Prim.Int#)
	 (w_s19g :: GHC.Prim.State# s_aLn) ->
	 case ww1_s192 of wild2_X1p {
	   __DEFAULT ->
	     $wtrans_s19S
	       (GHC.Prim.+# ww_s18Y 1)
	       (GHC.Prim.-# wild2_X1p 1)
	       ww2_s196
	       ww3_s19a
	       (GHC.Prim.+#
		  ww4_s19e (GHC.Prim.indexIntArray# rb21_aO7 (GHC.Prim.+# rb11_aO4 ww_s18Y)))
	       w_s19g;
	   0 ->
	     let {
	       $w$j_s19W :: GHC.Prim.State# s_aLn
			    -> GHC.Prim.Int#
			    -> GHC.Prim.Int#
			    -> (# GHC.Prim.State# s_aLn, (GHC.Base.Int, GHC.Base.Int) #)
	       [Arity 3
		Str: DmdType LLL]
	       $w$j_s19W =
		 \ (w1_s18H :: GHC.Prim.State# s_aLn)
		   (ww5_s18M :: GHC.Prim.Int#)
		   (ww6_s18Q :: GHC.Prim.Int#) ->
		   let {
		     a_s104 [Just L] :: GHC.Prim.Int#
		     [Str: DmdType]
		     a_s104 = GHC.Prim.+# ww2_s196 1
		   } in 
		     case GHC.Prim.==# a_s104 rb1_aJe of wild3_aLq {
		       GHC.Base.False ->
			 case a_s104 of wild31_X2J {
			   __DEFAULT ->
			     case GHC.Prim.readIntArray#
				    @ s_aLn marr#1_XPR (GHC.Prim.-# wild31_X2J 1) w1_s18H
			     of wild21_aXE { (# s2#3_aXG, r#_aXH #) ->
			     case GHC.Prim.readIntArray#
				    @ s_aLn marr#_aOP (GHC.Prim.-# wild31_X2J 1) s2#3_aXG
			     of wild22_XZY { (# s2#4_X101, r#1_X103 #) ->
			     case GHC.Prim.writeIntArray#
				    @ s_aLn marr#1_XPR wild31_X2J (GHC.Prim.+# r#_aXH r#1_X103) s2#4_X101
			     of s2#5_aVX { __DEFAULT ->
			     case GHC.Prim.writeIntArray# @ s_aLn marr#_aOP wild31_X2J 0 s2#5_aVX
			     of s2#6_XYb { __DEFAULT ->
			     $wtrans_s19S
			       ww_s18Y
			       (GHC.Prim.indexIntArray# rb2_aJf (GHC.Prim.+# rb_aIC wild31_X2J))
			       wild31_X2J
			       ww5_s18M
			       ww6_s18Q
			       s2#6_XYb
			     }
			     }
			     }
			     };
			   0 ->
			     case GHC.Prim.writeIntArray# @ s_aLn marr#1_XPR 0 0 w1_s18H
			     of s2#3_aVX { __DEFAULT ->
			     case GHC.Prim.writeIntArray# @ s_aLn marr#_aOP 0 0 s2#3_aVX
			     of s2#4_XYb { __DEFAULT ->
			     $wtrans_s19S
			       ww_s18Y
			       (GHC.Prim.indexIntArray# rb2_aJf rb_aIC)
			       0
			       ww5_s18M
			       ww6_s18Q
			       s2#4_XYb
			     }
			     }
			 };
		       GHC.Base.True -> (# w1_s18H, ((GHC.Base.I# ww5_s18M), (GHC.Base.I# ww6_s18Q)) #)
		     }
	     } in 
	       case ww2_s196 of wild3_X1P {
		 __DEFAULT ->
		   case GHC.Prim.writeIntArray# @ s_aLn marr#2_XQ3 ww3_s19a ww4_s19e w_s19g
		   of s2#3_aVX { __DEFAULT ->
		   $w$j_s19W s2#3_aVX (GHC.Prim.+# ww3_s19a 1) ww4_s19e
		   };
		 (-1) -> $w$j_s19W w_s19g ww3_s19a ww4_s19e
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
