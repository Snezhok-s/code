   -- ������������ ���������� ������� �� ���������� 
   -- Prelude (2001)
   -- ***************************************
   -- �������, ������������� ����� ����� ���� 
   -- Integer � ����� � ��������� ������
   -------------------------------------
   f1:: Integer -> Float
   f1 x = fromInteger x

   ------------------------------------------
   -- �������, ������������� ����� ����� ����
   -- Int � ����� � ��������� ������
   ---------------------------------
   f2:: Int -> Float
   f2 x = fromInt x

   ------------------------------------------
   -- �������, ������������� ����� ����� ����
   -- Integer � ����� � ��������� ������
   -------------------------------------
   f3:: Integer -> Double
   f3 x = fromInteger x

   ---------------------------------------------
   -- �������, ������������ �������� ��������� x
   -- �� ���������  ��������������  ��������� a,
   -- ��������� �� �������
   -- (� � � � � �  ������)
   -------------------------------
   f4:: Double -> Double -> Double
   f4 a x = logBase a x
    
   ---------------------------------------------
   -- �������, ������������ �������� ��������� x
   -- �� ���������  ��������������  ��������� a,
   -- ��������� �� ������� 
   -- (� � � � � �  ������)
   --------------------------------
   f4':: Double -> Double -> Double
   f4' a x = (log x) / (log a)
    
   --------------------------------------------------
   -- �������, ������������  ���������� ����� �������
   -- ���� ����� ����� (�� ������������ �������� lcm)
   --------------------------------------------------
   lcm':: Integer -> Integer -> Integer
   lcm' x y = abs ((x*y) `div` (gcd x y))

   ------------------------------------------------
   -- ������������ �������� ������� rem � mod, ���-
   -- ��������� ������� �� ������� ����� �����
   -- (������� ���������� ��� ��������)
   ------------------------------------
   prov:: Int -> Int -> (Int,Int)
   prov a b = (a `rem` b, a `mod` b)

   ---------------------------------------------------
   -- ������������ ������������� ����� Pi ������������
   -- ���������� ������ � ��������� delta
   -------------------------------------- 
   approxPI:: Float -> (Integer,Integer)
   approxPI delta = (numerator a, denominator a)
         where a = approxRational pi delta 

   -- ***************************
   -- ��������� �������� �������:
   -------------------------------------------
   test1 = "Results: "++" "++show (prov 23 9)
                           ++" "++show (prov (-23) 9)
                           ++" "++show (prov 23 (-9))
   --------------------------------------------------
   test2 = (approxPI 0.002, approxPI 0.0000000001,
            approxPI 0.00000000000000000001) 
   -----------------------------------------
   test3 = f4 (exp 1) ((exp 1)**12.0)
   test4 = f4' (exp 1) ((exp 1)**12.0)
   test5 = f4' 10.0 1000.0
   test6 = lcm' 10 1000
   test7 = lcm' 0 10
