   import List (findIndices,nub,sort,groupBy,(\\))
   -- **********************************************
   -- �������, ��������� �� ������ lst ��� ��������,
   -- ����������� � �������� ��������� x
   -------------------------------------
   del el lst | null lst     = []
              | head lst==el = del el (tail lst)         
              | True         = head lst : del el (tail lst)

   -- ************************************************
   -- ������� ������� �� ������ lst ��� �������, ����-
   -- ������ ������� ��������� �� ��������� n
   -- (��� ������� lst1:=lst1)
   -- (������������� ����������)
   ------------------------------------------
   prov lst lst1 n | length lst<n || null lst
                          = lst
                   | kol (head lst) lst1==n  
                          = prov (del (head lst) lst) lst1 n
                   | True = head lst : prov (tail lst) lst1 n
   ----------------------------------------------------------
   -- �������, ����������� ���������� ���������
   -- ������� x � ������ lst
   ---------------------------
   kol x lst | null lst    = 0
             | head lst==x = 1 + kol x (tail lst)  
             | True        = kol x (tail lst)

   -- ******************************************
   -- �������, ������������� �� ��������� �����
   -- �����, ������� ����������� � �� n ���
   -- (������������� ����������)
   ----------------------------------------------
   result n word = vsp3 (vsp2 n (vsp1 word)) word
       where vsp2 n lst = map (\(a,b) -> if b==n then a else '#')
                              lst
             vsp3 lst word 
                 | null word            = []
                 | elem (head word) lst = vsp3 lst (tail word)
                 | True                 = head word : 
                                          vsp3 lst (tail word)
   -----------------------------------------------------------
   vsp1 word = map (\x -> (x,poisk x word)) (nub word)
          where poisk x word 
                  | null word    = 0
                  | head word==x = 1 + poisk x (tail word)
                  | True         = poisk x (tail word)

   -- ******************************************
   -- �������, ������������� �� ��������� �����
   -- �����, ������� ����������� � �� n ���
   -- (� �������������� ������������)
   -----------------------------------------------
   bca word n = (\\) word (concat (replicate n z))
           where z = map (\y -> head y)
                         (filter (\x -> length x==n) 
                                 (groupBy (==) $ sort word))

   -- ********************************************
   -- �������, ������������ ������� findIndices ��
   -- ���������� List;
   --  i - ������������� ��������; ��� ������ i:=0;
   --  k - ������������� ��������; ��� ������ k:=""
   ------------------------------------------------
   fun str s i k = 
        if null str
          then k
          else if head str==s
                 then fun (tail str) s (i+1) (k ++ show i++" ")
                 else fun (tail str) s (i+1)  k

   -- ***************************
   -- ��������� �������� �������:
   -----------------------------------------------------
   test1 = prov [1,1,1,4,5,6,7,7,7] [1,1,1,4,5,6,7,7,7] 3
           == [4,5,6]
   test2 = result 3 "111456777" == "456"
   test3 word s = (fun word s 0 "",findIndices (==s) word)
   -------------------------------------------------------
   test4 =   bca "aaaasssssweqew" 4 == "sssssweqew"
          && bca "aaaasssssweqew" 5 == "aaaaweqew"
          && bca "aaaasssssweqew" 2 == "aaaasssssq"
          && bca "aaaasssssweqew" 1 == "aaaasssssweew"
          && bca "aaaasssssweqew" 0 == "aaaasssssweqew"
