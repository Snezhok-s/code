
------------------------
  f1  x y = (x*y) / (2*(x+y))
  f2  x y = (5*(x-y)) / (x+y)
  f3  x y = (x-y) / (x-2*y)
  f4  x y = (x+10*y) / (x-y)
  f5  x y = (x*y-4) / (x+y)
  f6  x y = (2*x-y) / (x-y)
  f7  x y = (x+y) / (2*(x-y))
  f8  x y = (x*y) / (2*x-y)
  f9  x   = abs ( (x-9) / (x-4) )
  f10 x   = abs ( (x-1) / (x+1) )
------------------------
  tX = 4.0  -- ���������� ��� �����.                       
  tY = 3.0  -- ���� ���� �������� � ���������.             
            -- �� ������ ��������� �������� ��������������.

{-
  t1  = f1 tX tY == 12/14
  t2  = f1 tX tY == 5/7
  t3  = f1 tX tY == (-0.5)
  t4  = f1 tX tY == 34
  t5  = f1 tX tY == 8/7
  t6  = f1 tX tY == 5
  t7  = f1 tX tY == 7/2
  t8  = f1 tX tY == 12/5
  t9  = f1 5.0   == 4
  t10 = f1 10.0  == 9/11

------------------------
  test = t1 && t2 && t3 && t4 && t5 &&
         t6 && t7 && t8 && t9 && t10 
------------------------ -}
