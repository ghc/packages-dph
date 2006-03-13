-- |External interface to the parallel arrays library
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--
--  $Id: PArray.hs,v 1.20 2003/03/13 05:24:44 chak Exp $
--
--  This file may be used, modified, and distributed under the same conditions
--  and the same warranty disclaimer as set out in the X11 license.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98
--
--- Todo ----------------------------------------------------------------------
--

module PArray (
  -- * Classes
  EP, PArray,

  -- * Types
  PArr, SPArr, PAUnit, PAProd, PAPrimU, PAPArr, UInt, UChar, UFloat, UDouble,
  PArrBool, PArrChar, PArrInt, PArrFloat, PArrDouble, SPArrBool, SPArrChar,
  SPArrInt, SPArrFloat, SPArrDouble, PSegd,

  -- * Combinators imitating basic list processing functions
  mapP,	(+:+), filterP, concatP, {-concatMapP,-} nullP, lengthP, (!:), foldlP,
  foldlSP, {-foldl1P,-} scanlP, {-scanl1P, foldrP, foldr1P, scanrP, scanr1P,-}
  foldP, foldSP, {-fold1P, fold1SP,-} scanP, {-scanSP, scan1P, scan1SP,-}
  takeP, dropP,	splitAtP, {-takeWhileP, dropWhileP, spanP, breakP,-}
--  lines, words, unlines, unwords,  -- is string processing really needed
  reverseP, andP, andSP, orP, orSP, anyP, allP, elemP, notElemP, {-lookupP,-}
  sumP, sumSP, productP, productSP, maximumP, maximumSP, minimumP, minimumSP,
  zipP, zip3P, zipWithP, zipWith3P, unzipP, unzip3P, enumFromToP,
  enumFromToSP, enumFromThenToP, enumFromThenToSP, 

  -- * Array-oriented combinators
  --
  flattenP, (>:), toSegd, fromSegd, segmentP, toP, toSP, fromP, emptyP, sliceP,
  permuteP, bpermuteP, bpermuteSP, bpermuteDftP, {-crossP, indexOfP, -}

  -- * Loop/replicate combinators
  replicateP, loopP, replicateSP, loopSP,

  -- * Projection combinators for loops
  loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc,

  -- * Special forms of loop mutators
  noEFL, noSFL, noAL, mapEFL, filterEFL, foldEFL, scanEFL, transSFL, keepSFL,

  -- * Library id
  idstr, name, versnum, date, version, copyright, disclaimer
) where

import PABase   (PArray, PArr, SPArr, PAUnit, PAProd, PAPrimU, PAPArr, UInt,
		 UChar, UFloat, UDouble, PArrChar, PArrInt, PArrFloat,
		 PArrDouble, SPArrChar, SPArrInt, SPArrFloat, SPArrDouble,
		 PSegd, toSegd, fromSegd) 
import PALoop   (loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc)
import PAEP     (EP, lengthP, sliceP, replicateP, loopP, replicateSP, loopSP)
import PAFusion
import PAOps


-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: PArray.hs,v 1.20 2003/03/13 05:24:44 chak Exp $"
name       = "Parallel Array Library"
versnum    = "0.4.1"
date	   = "13 Mar 2003"
version    = name ++ ", version " ++ versnum ++ ", " ++ date
copyright  = "Copyright (c) [2001..2003] \
	     \Manuel M T Chakravarty & Gabriele Keller"
disclaimer = "This software is distributed under the terms \
	     \of the X11 license.  NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."


-- |Parallel array instances of standard classes
-- ---------------------------------------------

-- |
instance (Show e, EP e r, PArray r arr) => Show (PArr arr e) where
  showsPrec _  = (showString "toP " .) . showList . fromP

-- |
instance Show PSegd where
  showsPrec _ segd =   showString "toSegd (toP "
		     . showList (fromP (fromSegd segd))
		     . showChar ')'

-- |
instance (Eq e, EP e r, PArray r arr) => Eq (PArr arr e) where
  a1 == a2 = foldlP cmp True (zipP a1 a2)
	     where
	       cmp r (e1, e2) = e1 == e2 && r
