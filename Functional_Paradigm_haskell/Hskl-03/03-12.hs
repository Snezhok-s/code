   -- ����������  � � � � � � � � � � � �  ���� � ४��-
   -- ᨥ� ���襣� ���浪� �  ���  � ���⮩ ४��ᨥ�  ��
   -- �ਬ�� ����� ���᫥��� ���祭�� �㭪樨 ��������
   --  
   --   f(0)    = 0,
   --   f(1)    = 1,
   --   f(2n)   = f(n),
   --   f(2n+1) = f(n) + f(n+1),
   --
   -- ����� �८�ࠧ����  � ������ ���᫥��� ���祭��
   -- �㭪樨
   --
   --   g(0,i,j) = j,
   --   g(2n)    = g(n,i+j,j),
   --   g(2n+1)  = g(n,i,i+j).
   --
   -- ������ ���祭�� �㭪樨 f() ��室���� ⠪:
   --
   --   f(n)=g(n,1,0)
   --
   -- **************************
   fDejikst:: Integer -> Integer
   fDejikst n | n==0   = 0
              | n==1   = 1
              | even n = fDejikst (n `div` 2) 
              | True   = fDejikst ((n-1) `div` 2) +
                         fDejikst (((n-1) `div` 2) + 1)

   ---------------------------------------------------
   gDejikst:: Integer -> Integer -> Integer -> Integer
   gDejikst n i j | n==0   = j
                  | even n = gDejikst (n `div` 2)    (i+j)  j
                  | True   = gDejikst ((n-1) `div` 2)  i  (i+j)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   -----------------------------------------------------
   test =   fDejikst 12        == gDejikst 12        1 0
         && fDejikst 234       == gDejikst 234       1 0
         && fDejikst 1234      == gDejikst 1234      1 0 
         && fDejikst 12345     == gDejikst 12345     1 0 
         && fDejikst 123456    == gDejikst 123456    1 0 
         && fDejikst 123456789 == gDejikst 123456789 1 0
