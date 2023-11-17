   -- ���������� ���᫥��� ���祭�� �����୮�
   -- �㭪樨 y=arcsin(x)
   -- *********************
   asin' x | x==1    = pi/2
           | x==(-1) = -pi/2
           | True    = atan (x / sqrt (1-x^2))

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test1   = (asin 1, asin' 1)
   test2   = (asin (-1), asin' (-1))
   test3 x = (asin x, asin' x)
   test4   = map (\x ->  asin' (sqrt (abs (1-x))) 
                       - acos (abs (sqrt x))) 
                 [0,0.02..1.0]
