   -- ��������樨 �ਡ���񭭮�� �襭�� �������� �����,
   -- �⭮������ � ⥮ਨ  � � � � � � � �  � � � � �
   -- ************************************************
   -- ���᫥��� n-�� ���筮� �㬬� �鸞, n-� 童�
   -- ���ண� ����� �㭪樥� f
   ----------------------------------
   s n f = sum $ take n $ map f [1..]

   -- ********************************************
   -- ���᫥��� ��᫥����⥫쭮�� ������ �㬬
   -- �鸞, n-� 童� ���ண� ����� �㭪樥� f
   --------------------------------------------
   lim n f = scanl (+) 0 (take n $ map f [1..])

   -- ***********************************************
   -- �㭪�� �����頥� ����譮��� ���᫥��� �㬬�
   -- �᫮���� �鸞, n-� 童� ���ண� ����� �㭪樥� f
   -----------------------------------------------------
   err n f = last' (lim n f)
        where last' lst = abs (last lst - last (init lst))

   -- ***********************************************
   -- �㭪樨 ��� ���஢���� ���᫥��� �㬬� �冷�
   --------------------------------------------------
   prov  = pi * pi/6.0
   prov' = pi * pi * pi * pi / 90.0

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   f  = \x -> 1.0 / x^2
   f' = \x -> 1.0 / x^4
   f''= \x -> 1 / x
   ------------------------
   test1 = (s 1000 f, prov)
   test2 = (s 10000 f', prov')
   test3 = (s 100 f'', s 5000 f'', s 10000 f'', s 15000 f'')
   test4 = (err 10 f, err 50 f, err 5000 f)