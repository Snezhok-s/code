   -- ���������� ४��ᨢ���� ��।������ ���⮪
   --
   --  foldr, foldl, foldr1, foldl1 (�� Prelude)
   --
   -- **********************************************
   import Prelude hiding (foldr,foldl,foldr1,foldl1)

   -- ===================================
   foldr:: (a -> b -> b) -> b -> [a] -> b
   foldr f e []     = e
   foldr f e (x:xs) = x `f` foldr f e xs
   --------------------------------------
   foldr1:: (a -> a -> a) -> [a] -> a
   foldr1 f [x]      = x
   foldr1 f (x:xs)   = f x (foldr1 f xs)

   -- =================================
   -- ���������� ��ਠ�⮢ ॠ����樨
   -- �㭪樮���� foldr
   ---------------------------------------
   foldr':: (a -> b -> b) -> b -> [a] -> b
   foldr' f z xs = go xs
        where go []     = z
              go (y:ys) = y `f` go ys
   ----------------------------------
   foldr'' cons nil = \x -> case x of
                              a:as -> a `cons` foldr cons nil as

   -- =================================
   -- ���������� ��ਠ�⮢ ॠ����樨
   -- �㭪樮���� foldl
   -------------------------------------- 
   foldl:: (b -> a -> b) -> b -> [a] -> b
   foldl f e []     = e
   foldl f e (x:xs) = foldl f (e `f` x) xs
   ---------------------------------------
   foldl1:: (a -> a -> a) -> [a] -> a
   foldl1 f (x:xs)  = foldl f x xs

   ---------------------------------------
   foldl':: (a -> b -> a) -> a -> [b] -> a
   foldl' f z0 xs0 = lgo z0 xs0
                where lgo z []     =  z
                      lgo z (x:xs) = lgo (f z x) xs
   ------------------------------------------------
   foldl'':: (a -> b -> a) -> a -> [b] -> a
   foldl'' f z0 xs0 = lgo z0 xs0
       where lgo z []     = z
             lgo z (x:xs) = let z' = f z x
                            in z' `seq` lgo z' xs
   ----------------------------------------------
   -- ��������� �� ������⥪� Prelude
   -----------------------------------------
   foldl''':: (a -> b -> a) -> a -> [b] -> a
   foldl''' f a []     = a
   foldl''' f a (x:xs) = (foldl''' f $! f a x) xs

  {-
   > (const 2) (1 / 0)        > (const 2) $! (1 / 0)
   2 :: Integer             
   (14 reductions, 40 cells)  Program error: {primDivDouble 1.0 0.0}
                              (19 reductions, 101 cells)
  -}

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   --------------------------------------------
   test1 =   foldr (+)  0 []             ==   0
          && foldr (*)  4 [5,6,7]        == 840
          && foldr (-)  0 [1,2,3,4,5]    ==   3
          && foldr min 10 [-1,2,13,-4,5] ==  -4
   --------------------------------------------
   test2 =   foldl (+)  0 []             ==   0
          && foldl (*)  4 [5,6,7]        == 840
          && foldl (-)  0 [1,2,3,4,5]    == -15
          && foldl max 10 [-1,2,13,-4,5] ==  13
   ---------------------------------------------------   
   test3 =   foldr1 max [-1,2,13,-4,5]              == 
             max (-1) (max 2 (max 13 (max (-4) 5)))           
          && max (-1) (max 2 (max 13 (max (-4) 5))) == 13
   ------------------------------------------------------   
   test4 =   foldl1 max [-1,2,13,-4,5] == 
             max (max (max (max (max (-1) (-1)) 2) 13) (-4)) 5 
          && max (max (max (max (max (-1) (-1)) 2) 13) (-4)) 5
                                       == 13
   ----------------------------------------------------
   test5 = foldr   (+) 0 [1..3500]  -- foldr �� Prelude
   test6 = foldr'  (+) 0 [1..3500]
   -----------------------------------------
   test7 = foldr'' (++) [] [[1,2,3],[5,6,7]]
   test8 = foldr'' (:) [1,2,3] [4,5,6]
   -----------------------------------
   test9  = foldl    (+) 0 [1..5310]
   test10 = foldl'   (+) 0 [1..5310]
   test11 = foldl''  (+) 0 [1..7000]
   test12 = foldl''' (+) 0 [1..7000]  -- foldl �� Prelude
