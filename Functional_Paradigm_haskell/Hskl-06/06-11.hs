   -- ������ �ࠣ��⨪� (�����祭��) �㭪権.
   --
   -- �����: C.��������, �.���쭨檨�, �.�.���檨� 
   --         (14.11.2016)
   -- ********************
   import List

   -- ***********************************
   task = filter (\x -> not $ isPower2 x)
      where isPower2 x | x == 1         = True
                       | x `mod` 2 == 1 = False
                       | True           = isPower2 (x `div` 2)

   -- **************
   wout2n n l@(x:xs) 
      | l  == []           = []
      | xs == [] && x == n = []
      | xs == [] && x /= n = [x]
      | x  == n            = wout2n (n * 2) xs
      | True               = x : wout2n n xs

   -- ****************
   abc :: Int -> [Int]
   abc n = take n $ (\\) [1..n] (map (2^) [1..z])
              where z = floor $ logBase 2.0 (fromInt n)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ----------------------------------
   test1 = last $ wout2n 2 [1..10000]
   test2 = last $ [1] ++ task [1..10000]
   test3 = last $ abc 10000
