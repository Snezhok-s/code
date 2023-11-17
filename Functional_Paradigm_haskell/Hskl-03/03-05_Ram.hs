   -- ���������� �ᯮ�짮����� ४��ᨨ �� ��㬥�⠬
   -- ��� �襭��:
   --   (1) ���� ����� �.�����㤦���;
   --   (2) ����� �.���� 
   -- ********************

   -- =========================================
   -- �㭪�� f62 �����頥� ���祭�� ��ࠦ����
   --
   -- Sqrt(6+2*Sqrt(7+3*Sqrt(8+4*Sqrt(9+...))),
   --
   ------------------------------------------
   f62:: Double -> Double -> Double -> Double
   f62 n a b | n==1 = sqrt 6.0
        | True = sqrt (a+b*f62 (n-1) (a+1) (b+1))

   -- ===================
   f8:: Integer -> Double
   f8 n | n==1 = sqrt (8.0-sqrt(8.0+sqrt 8.0))
        | True = sqrt (8.0-sqrt(8.0+sqrt (8.0-f8 (n-1))))

   -- ====================
   f23:: Integer -> Double
   f23 n | n==1 = sqrt (23.0-2*sqrt(23.0+2*sqrt 23.0))
         | True = sqrt (23.0-2*sqrt(23.0+
                                    2*sqrt(23.0+2*f23 (n-1))))

   -- ***************************************************
   -- ��������! �� �襭�� �ᯮ�짮��� �㭪樮��� map...
   ------------------------------------------------------
   piVieta:: Int -> Double
   piVieta m = product (map coeff [1..m])
        where coeff n | n==1 = sqrt (1/2)
                      | True = sqrt (1/2+1/2*coeff (n-1))
                           
   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test1:: (Double,Integer)
   test1 = (f62 100 6 2,4)
   -----------------------------
   test2,test3:: (Double,Double)
   test2 = (f8 200,otvet)
              where otvet = 1.0+2.0*sqrt (3.0)*sin (pi/9.0)
   test3 = (f23 300, otvet)
              where otvet = 1.0+4.0*sqrt (3.0)*sin (pi/9.0)
   --------------------------------------------------------
   test4 k = (piVieta k, 2/pi)

   -----------------------
   f11:: Integer -> Double
   f11 n | n==1 = sqrt (11.0-2.0*sqrt(11.0+2.0*sqrt 11.0))
         | True = sqrt (11.0-2.0*sqrt(11.0+2.0*sqrt (11.0-2.0*f11 (n-1))))
   -----------------------------------------------------------------------
   test5 = (f11 3, otvet)
         where otvet = 1.0 + 4.0 * sin (pi/18.0)
