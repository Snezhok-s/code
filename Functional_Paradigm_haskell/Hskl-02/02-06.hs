   -- ������������ ����������� ���������� �������
   -- � ������� ������-������� 
   -- ***************************************
   -- ������� ������ ���������, �������������
   -- �������� ��������� �� 1
   --------------------------
   inc:: Int -> Int
   inc = \(x:: Int) -> x + 1
   ---------------------------------------------
   -- ������� ���� ����������, ����������� �����
   -- ���� �����
   ------------------------
   add1:: Int -> Int -> Int
   add1 = \x -> \y -> x + y       
   -------------------------------------------
   -- ������� ���� ���������� (!), �����������
   -- ����� ���� �����
   ------------------------
   add2:: Int -> Int -> Int
   add2 x = \y -> x + y
   -------------------------------------------
   -- �������, ������������ ���������� �������
   -- � �������������� ���������� �������
   --------------------------------------
   f1 x = (.) (\x -> x+5) (\z -> z^2) x
   ------------------------------------
   f2 x = (\x -> x+5) ((\z -> z^2) x)

   -- ***************************
   -- ��������� �������� �������:
   ------------------------------
   test1 = inc 2^31 == 1264544299
   ---------------------------------------------------------
   test2 =   (\x -> \y -> \z -> x^2 + y^2 + z^2) 2 3 4 == 29 
          && (\x y z         -> x * y * z) 2 3 4       == 24
          --------------------------------------------------
          && add1 12 13 == 25
          && add2 12 13 == 25
          && f2 3       == 14
          && f1 3       == 14
   ---------------------------------------------
   test3 = ((\x -> x + 5):: Int -> Int) 10 == 15
