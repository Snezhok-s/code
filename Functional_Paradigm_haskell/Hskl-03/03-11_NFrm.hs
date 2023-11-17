   -- ���������� �ਭ�⮩ � �몥 Haskell ᥬ��⨪�  ����-
   -- �����, �������饩�� � �롮� ��� ���᫥��� �� ४��-
   -- ᨢ��� �맮��  ᠬ��  ���譥�  �㭪樨  (⠪ ���뢠����
   -- � � � � � � � � � �  � � � � �  ���-।�樨).
   --    �᫨ �⮣� �� ᤥ����, � ���������  ���������筮���
   -- ����� ���᫥���.
   --    ���᭨� ᪠������ � ������� �㭪樨
   --                                  
   --          �1, �᫨ x=0;
   --   f(x,y)=�
   --          �f(x-1,f(x,y)), �᫨ x>0.
   --
   --   ���祭�� f(1,0) ����� ���᫨�� ���� ᯮᮡ���:
   --   (1) � ������� ��ଠ�쭮� ��� ���-।�樨:
   --
   --    f(1,0) = f(0,f(1,0)) = 1;
   --
   --   (2) � ������� �������⨢���� ���浪� ।�樨:
   --
   --    f(1,0) = f(0,f(1,0)) = f(0,f(0,f(1,0))) =
   --           = f(0,f(0,f(0,f(1,0)))) = ...
   --
   -- **********************************
   func1:: Integer -> Integer -> Integer
   func1 x y | x==0 = 1
             | True = func1 (x-1) (func1 x y)

   -- ==================================
   func2:: Integer -> Integer -> Integer
   func2 x y | y==0 = 1
             | True = func2 (func2 x y) (y-1) 

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test1 =   func1   1  0 == 1
          && func1   1  1 == 1
          && func1   2  0 == 1
          && func1 123 34 == 1
   ---------------------------
   test2 =   func2  1   0 == 1
          && func2  1   1 == 1
          && func2  0   2 == 1
          && func2 34 123 == 1
   --------------------------------------------------------
   test3 y = all (== 0) $ map (\x -> func1 x y - func2 x y)
                              [1..1000] 