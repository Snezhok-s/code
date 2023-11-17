   -- �㭪�� �����頥� १���� ���᫥��� ��ࠦ����
   --
   -- sqrt(1+3*(sqrt(3+9*(sqrt(9+27*(sqrt(27+81*(sqrt(81...))))))))
   --
   -- n - ������⢮ �ᯮ��㥬�� ��୥�
   -- (�� �맮�� a=1, b=3)
   ----------------------------------------
   f:: Double -> Double -> Double -> Double
   f n a b | n==1 = sqrt a
           | True = sqrt (a+b*f (n-1) (3*a) (3*3*a))

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------------------------------------
   test1 = 1*sqrt (1+   3*sqrt (  3+  9*sqrt (  9+ 27*sqrt (27+
          81*sqrt (81+243*sqrt (243+729*sqrt 729))))))
   test2 = f 7 1 3