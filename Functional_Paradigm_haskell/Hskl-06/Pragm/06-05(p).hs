   -- ���������� �㭪権, ��������� ����� 
   -- ᠬ�� ������� ��᫥����⥫쭮��  ����-
   -- ������� ᨬ����� � �����⮬ ᫮��
   -- **********************************
   import List

   -- ***********************************************
   -- (1) � �ᯮ�짮������ �㭪樮����� map, groupBy)
   --------------------------------------------------
   cba:: [Char] -> Int
   cba word = maximum $ map (\x -> length x)
                            (groupBy (==) word)

   -- **********************************************************
   -- (2) �����室��� ������; ��� �ᯮ�짮����� �㭪樮�����
   -------------------------------------------------------------
   longest word = maximum $ bca word 0 []
   --------------------------------------
   bca:: [Char] -> Int -> [Int] -> [Int]
   bca word k lst | null word = lst
                  | null (tail word)
                              = (k+1) : lst 
                  | head word==head (tail word)
                              = bca (tail word) (k+1)      lst
                  | True      = bca (tail word)   0   ((k+1) : lst)

   -- **********************************************************
   -- (3) ������室��� ������; ��� �ᯮ�짮����� �㭪樮�����
   -------------------------------------------------------------
   bca':: [Char] -> Int -> Int -> Int
   bca' word k l | null word = l
                 | null (tail word)
                             = max (k+1) l
                 | head word==head (tail word)
                             = bca' (tail word) (k+1) l
                 | True      = bca' (tail word) 0 (max (k+1) l)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   -------------------------------------
   test1   = bca "abbcccddddeeeccb" 0 []
   test2   = bca "abcdeeeeeeeecbde" 0 []
   test3 x = ((==) (longest x) (cba x))
   test4 x = bca' x 0 0
