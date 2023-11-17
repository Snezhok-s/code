   -- ���������� ������஢���� ᡠ����஢�����
   -- (�ॢ����) ���⮪
   -- *******************
   import List

   -- =======================================
   -- ��������� ᡠ����஢����� (�ॢ�᭮�)
   -- ���⪨ � ���ࠢ����� "ᢥ��� ����"
   --------------------------------------
   foldb f [x] = x
   foldb f xs  = f (foldb f (take (length xs `div` 2) xs))
                   (foldb f (drop (length xs `div` 2) xs))

   -- =======================================
   -- ��������� ᡠ����஢����� (�ॢ�᭮�)
   -- ���⪨ � ���ࠢ����� "᭨�� �����"
   --------------------------------------
   foldb' f [x] = x
   foldb' f xs  = foldb' f (pair f xs)
        where pair f []             = []
              pair f [x]            = [x]
              pair f (x1 : x2 : xs) = f x1 x2 : pair f xs

   -- ********************************************
   -- ��������� ���஢�� ᫨ﭨ�� � foldb-�⨫�
   -----------------------------------------------
   sort' [] = []
   sort' xs = foldb merge (map (\z -> [z]) xs)
   -------------------------------------------
   merge:: Ord a => [a] -> [a] -> [a]
   merge [] ys = ys
   merge xs [] = xs
   merge (x:xs) (y:ys) | x<=y = x : merge   xs   (y:ys)
                       | True = y : merge (x:xs)   ys

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------------------------------
   test1 = (foldb  (+) [1..10000], foldb' (+) [1..10000])
   ------------------------------------------------------
   test2 = sort' z == sort z
        where z = [2200,2199..1]