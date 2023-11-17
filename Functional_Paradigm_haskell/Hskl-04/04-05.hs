   -- �������, ������������ ������ 1<=k<n ��������� ������, 
   -- ����������� n ���������, �  �����  ������, �� �������
   -- ��� ���� ������� ���������� ���������.
   --
   -- ��� �� ����� muLISP �����:
   --
   -- (DEFUN ABC (LAMBDA (LST k)
   --   (REVERSE (APPEND 
   --              (REVERSE (FIRSTN (MOD k (LENGTH LST)) LST)) 
   --              (REVERSE (NTHCDR (MOD k (LENGTH LST)) LST))
   --            )
   --   )
   -- ))
   -- ************************
   abc:: [Int] -> Int -> [Int]
   abc lst k = reverse (
                        (reverse (take rest lst)) ++ 
                        (reverse (drop rest lst))
                       ) 
        where rest = k `mod` length lst

   -- ****************************************
   -- ������������ ���������� ���� ������� abc
   -------------------------------------------
   abc':: [Int] -> Int -> [Int]
   abc' lst k = drop k lst ++ take k lst

   -- ***************************
   -- ��������� �������� �������:
   -----------------------------------------------
   test1 =   abc [1,2,3,4,5,6]  0 == [1,2,3,4,5,6]
          && abc [1,2,3,4,5,6]  1 == [2,3,4,5,6,1]
          && abc [1,2,3,4,5,6]  2 == [3,4,5,6,1,2]
          && abc [1,2,3,4,5,6]  3 == [4,5,6,1,2,3]
          && abc [1,2,3,4,5,6]  5 == [6,1,2,3,4,5]
          && abc [1,2,3,4,5,6]  6 == [1,2,3,4,5,6]
          && abc [1,2,3,4,5,6] 36 == [1,2,3,4,5,6]
          && abc [1,2,3,4,5,6] 38 == [3,4,5,6,1,2]
          && abc [1,2,3,4,5,6] 41 == [6,1,2,3,4,5]
   ------------------------------------------------
   test2 =   abc' [1,2,3,4,5,6] 0  == [1,2,3,4,5,6]
          && abc' [1,2,3,4,5,6] 1  == [2,3,4,5,6,1]
          && abc' [1,2,3,4,5,6] 2  == [3,4,5,6,1,2]
          && abc' [1,2,3,4,5,6] 3  == [4,5,6,1,2,3]
          && abc' [1,2,3,4,5,6] 5  == [6,1,2,3,4,5]
          && abc' [1,2,3,4,5,6] 6  == [1,2,3,4,5,6]