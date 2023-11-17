   -- ����������:
   --  (1) ����஥��� ��᫥����⥫쭮�� �����
   --      (�.�����, "� ��� ��㪨", 1984, 3);
   --  (2) ���᫥��� ������⢠ 蠣��;
   --  (3) ���᫥���  � � � � � � �, �.�. �������襣�
   --      �᫠ � ����祭��� ��᫥����⥫쭮��.
   -- �� ����� ���뢠���� ⠪�� ����祩 "3n+1"
   -- ******************************************
   import List
   ------------------------
   strange n | n==1   = [1]
             | even n = n : strange (n `div` 2)
             | True   = n : strange (3 * n + 1)
   --------------------------------------------
   strange' n | n==(-1) = [-1]
              | even n  = n : strange' (n `div` 2)
              | True    = n : strange' (3 * n - 1)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test1 = strange 7
   test2 = strange 27
   test3 = map (\x -> (x, (length . strange) x, maximum $ strange x))
               [1..50]
   test4 = maximum $ map (\(a,b,c) -> c) test3
   -------------------------------------------
   test5 = strange' (-3)   
   test6 = strange' (-5)   
   test7 = strange' (-17)  
   test8 = map (\x -> (x, (length . strange') x, minimum $ strange' x))
               [-10000..(-1)]
   test9 = nub $ map (\(a,b,c) -> c) test8
