module SegPrefixSum
where

import Data.Array.Parallel.Unlifted

test :: SUArr Int -> SUArr Int
test =   fst
       . loopArr
       . loopSU (\a x -> (a + x::Int, Just a)) 
		(\a i -> (a, Nothing :: Maybe ())) 0


{- Inner loop:

      $wtrans_s1aH :: GHC.Prim.Int#
		      -> GHC.Prim.Int#
		      -> GHC.Prim.Int#
		      -> GHC.Prim.Int#
		      -> GHC.Prim.Int#
		      -> GHC.Prim.State# s_aLt
		      -> (# GHC.Prim.State# s_aLt, (GHC.Base.Int, GHC.Base.Int) #)
      [Arity 6
       Str: DmdType LLLLLL]
      $wtrans_s1aH =
	\ (ww7_s19k :: GHC.Prim.Int#)
	  (ww8_s19o :: GHC.Prim.Int#)
	  (ww9_s19s :: GHC.Prim.Int#)
	  (ww10_s19w :: GHC.Prim.Int#)
	  (ww11_s19A :: GHC.Prim.Int#)
	  (w_s19C :: GHC.Prim.State# s_aLt) ->
	  case ww8_s19o of wild14_X1X {
	    __DEFAULT ->
	      case GHC.Prim.readIntArray# @ s_aLt marr#1_XQl ww9_s19s w_s19C
	      of wild2_aVH { (# s2#3_aVJ, r#_aVK #) ->
	      case GHC.Prim.readIntArray# @ s_aLt marr#_aPk ww9_s19s s2#3_aVJ
	      of wild21_XXy { (# s2#4_XXB, r#1_XXD #) ->
	      case GHC.Prim.writeIntArray#
		     @ s_aLt marr#2_XQt (GHC.Prim.+# r#_aVK r#1_XXD) ww11_s19A s2#4_XXB
	      of s2#5_aWC { __DEFAULT ->
	      case GHC.Prim.writeIntArray#
		     @ s_aLt marr#_aPk ww9_s19s (GHC.Prim.+# r#1_XXD 1) s2#5_aWC
	      of s2#6_XZ5 { __DEFAULT ->
	      $wtrans_s1aH
		(GHC.Prim.+# ww7_s19k 1)
		(GHC.Prim.-# wild14_X1X 1)
		ww9_s19s
		ww10_s19w
		(GHC.Prim.+#
		   ww11_s19A (GHC.Prim.indexIntArray# rb2_aOg (GHC.Prim.+# rb_aOd ww7_s19k)))
		s2#6_XZ5
	      }
	      }
	      }
	      };
	    0 ->
	      let {
		a_s10T [Just L] :: GHC.Prim.Int#
		[Str: DmdType]
		a_s10T = GHC.Prim.+# ww9_s19s 1
	      } in 
		case GHC.Prim.==# a_s10T ww1_s19O of wild3_aLw {
		  GHC.Base.False ->
		    case a_s10T of wild2_X3e {
		      __DEFAULT ->
			case GHC.Prim.readIntArray# @ s_aLt marr#1_XQl (GHC.Prim.-# wild2_X3e 1) w_s19C
			of wild21_aVH { (# s2#3_aVJ, r#_aVK #) ->
			case GHC.Prim.readIntArray# @ s_aLt marr#_aPk (GHC.Prim.-# wild2_X3e 1) s2#3_aVJ
			of wild22_XXY { (# s2#4_XY1, r#1_XY3 #) ->
			case GHC.Prim.writeIntArray#
			       @ s_aLt marr#1_XQl wild2_X3e (GHC.Prim.+# r#_aVK r#1_XY3) s2#4_XY1
			of s2#5_aWC { __DEFAULT ->
			case GHC.Prim.writeIntArray# @ s_aLt marr#_aPk wild2_X3e 0 s2#5_aWC
			of s2#6_XYN { __DEFAULT ->
			$wtrans_s1aH
			  ww7_s19k
			  (GHC.Prim.indexIntArray# ww2_s19P (GHC.Prim.+# ww_s19N wild2_X3e))
			  wild2_X3e
			  ww10_s19w
			  ww11_s19A
			  s2#6_XYN
			}
			}
			}
			};
		      0 ->
			case GHC.Prim.writeIntArray# @ s_aLt marr#1_XQl 0 0 w_s19C
			of s2#3_aWC { __DEFAULT ->
			case GHC.Prim.writeIntArray# @ s_aLt marr#_aPk 0 0 s2#3_aWC
			of s2#4_XYN { __DEFAULT ->
			$wtrans_s1aH
			  ww7_s19k
			  (GHC.Prim.indexIntArray# ww2_s19P ww_s19N)
			  0
			  ww10_s19w
			  ww11_s19A
			  s2#4_XYN
			}
			}
		    };
		  GHC.Base.True ->
		    (# w_s19C, ((GHC.Base.I# ww10_s19w), (GHC.Base.I# ww11_s19A)) #)
		}
	  };
    } in 

-}
