   -- ������������ �������, ������������ ������������ ���-
   -- �� ����������� ���������.
   --  ������������ �������:
   --  ++     - �������� ������������ (����������) �����;
   --  show f - �������, ������������� �������� ������� f
   --           � ��� String;
   --  fst    - ������� ��������� ������� ���������� ����;
   --  snd    - ������� ��������� ������� ���������� ����
   -- ***************************************************
   roots:: Float -> Float -> Float -> String
   roots a b c | abs (discrim a b c)<=0.0001
                      = "X="++show (oneRoot a b c)
               | discrim a b c>0
                      = "X1=" ++ show f ++ ", X2=" ++ show s
               | True = "�������������� ������ ���"
         ------------------------------------------
         where discrim a b c = b^2-4.0*a*c
               oneRoot a b c = -b/(2.0*a)
               f             = fst (twoRoots a b c)
               s             = snd (twoRoots a b c)

   --------------------------------------------
   -- ������� ���������� ����, ���������� �����
   ---------------------------------------------------
   twoRoots:: Float -> Float -> Float -> (Float,Float)
   twoRoots a b c = (d-e,d+e)
          where d = -b/(2.0*a)
                e = sqrt(b^2-4.0*a*c)/(2.0*a)

   -- ***************************
   -- ��������� �������� �������:
   --------------------------------------
   test =   roots 1 (-8)   16  == "X=4.0"
         && roots 1  0   (-16) == "X1=-4.0, X2=4.0"
         && roots 4  2      1  == "�������������� ������ ���"
