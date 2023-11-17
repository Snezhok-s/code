   -- ������������ ���������� � ��������� �������� ���������,
   -- � ������ ������� ������������ ����������� if-then-else
   -- ******************************************************
   -- �������, ������������ ���������� �������� ��������� n
   --------------------------------------------------------
   abs1':: (->) Integer Integer
   abs1' n = if n<0 
               then -n 
               else n
   -------------------------------------------------
   -- �������, ������������ ���������� �� ���� �����
   -------------------------------------------------
   max2:: (->) Int ((->) Int Int)
   max2 x y = if x>y 
                then x
                else y
   -------------------
   {-
      �������, ������������ �������� ������� |n|^|n|
   -}
   ---------------------------
   abs1'':: Integer -> Integer
   abs1'' n = (if n<0 then -n else n)^(if n<0 then -n else n) 
   ----------------------------------------------------------
   -- �������, ������������ ���������� �� ��� �����
   -- (� ������� ��������� �����������)
   ------------------------------------
   max3:: Int -> Int -> Int -> Int
   max3 m n k = if m>n && m>k 
                  then m
                  else if n>k
                         then n
                         else k
   -------------------------------------------------------
   -- �������, ������������ ����� ���� ������ ����� n<1000
   -------------------------------------------------------
   summa:: Integer -> Integer 
   summa n = if n>=1000
               then undefined  "n>=1000"
               else n `div` 100+n `mod` 100 `div` 10
                               +n `mod` 10
   -------------------------------------------------------
   -- �������, ������������ ����� ���� ������ ����� n<1000
   -- (� �������������� ������������)
   ----------------------------------
   summa':: Int -> Int 
   summa' n = sum $map digitToInt (show n)

   -- ***************************
   -- ��������� �������� �������:
   -----------------------------------------------
   test1 =   abs1' 12345                  == 12345
          && abs1' (-12345)               == 12345
          && abs1' 0                      == 0
          && abs1' 2222222222222222222    == 2222222222222222222
          && abs1' (-2222222222222222222) == 2222222222222222222
   -------------------------------------------------------------
   test2 = max2 3 5 == 3 `max2` 5 && max2 3 5 == (max2) 3 5 
   --------------------------------------------------------
   test3 =   max3 1 2 3    ==  3
          && max3 34 12 32 == 34
   -----------------------------
   test4 =   summa 45  ==  9
          && summa 512 ==  8
          && summa 555 == 15