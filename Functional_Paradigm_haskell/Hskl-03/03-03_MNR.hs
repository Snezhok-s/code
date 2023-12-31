   -- ���������� �ᯮ�짮����� ������������ ��ࠬ��஢ ���
   -- ॠ����樨 ��䬥��᪨� �㭪権 � �।���⮢ �  �⨫�
   -- ���᫨⥫쭮� ������, ����� ���뢠����
   -- � � � � � � �  � ����࠭�祭�묨 ॣ���ࠬ� (���)
   -- *************************************************

   -- ******************************************
   -- ������஢���� ����樨 "���⠭�� �������"
   -- �� ����᪥ res:=0, r3:=1
   -------------------------------
   sub1:: Int -> Int -> Int -> Int
   sub1 res x r3 | r3==x = res
                 | True  = sub1 (res+1) x (r3+1)

   -- ********************************************
   -- ������஢���� ����樨 "᫮����� ���� �ᥫ"
   -- �� ����᪥ res:=x, r4:=0
   -------------------------------------
   add:: Int -> Int -> Int -> Int -> Int
   add res x y r4 | y==r4 = res
                  | True  = add (res+1) x y (r4+1)

   -- *********************************************
   -- ������஢���� ����樨 "㬭������ ���� �ᥫ"
   -- �� ����᪥ res:=0
   ------------------------------
   mul:: Int -> Int -> Int -> Int
   mul res x y | y==0 = res
               | True = mul (res+x) x (y-1)

   -- *********************************************
   -- ������஢���� ����樨 "㬭������ ���� �ᥫ"
   -- �� ����᪥ res:=0
   -------------------------------
   mul':: Int -> Int -> Int -> Int
   mul' res x y | x==0 = res
                | True = mul' (res+y) (x-1) y

   -- *********************************************
   -- ������஢���� ����樨 "���⠭�� ���� �ᥫ"
   -- �� ����᪥ res:=x
   ------------------------------
   sub:: Int -> Int -> Int -> Int
   sub res x y | y==0 = res
               | True = sub (res-1) x (y-1)

   -- *********************************************
   -- ������஢���� ����樨 "楫��᫥���� �������
   -- ���� �ᥫ".
   -- �� ����᪥ res:=0, r4:=x
   --------------------------------------
   div':: Int -> Int -> Int -> Int -> Int
   div' res x y r4 | r4<y = res
                   | True = div' (res+1) x y (r4-y)

   -- ***********************************************
   -- ������஢���� �।���� "�ࠢ����� ���� �ᥫ":
   --
   --        |0, x>=y;
   -- f(x,y)=|
   --        |1, x<y
   --
   -- �� ����᪥ res:=0, r4:=0
   --------------------------------------
   less:: Int -> Int -> Int -> Int -> Int
   less res x y r4 | x==y  = res
                   | y==r4 = res
                   | x==r4 = 1
                   | True  = less res x y (r4+1)

   -- **********************************************
   -- ������஢���� ����樨 "��宦����� �������襣�
   -- ��饣� ����⥫� (���)".
   -- �� ����᪥ res:=0
   ------------------------------
   nod:: Int -> Int -> Int -> Int
   nod res x y | res/=0 = res
               | x==0   = nod y x y
               | y==0   = nod x x y
               | x>=y   = nod res (x-y) y
               | True   = nod res x (y-x)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test1 = sub1 0 11 1 == 10
   ----------------------------
   test2 =   add 5 5 10 0 == 15
          && add 2 2  3 0 ==  5
   -----------------------------
   test3 =   mul 0 5   10 ==  50
          && mul 0 7  111 == 777
          && mul 0 12  10 == 120
   -------------------------------
   test3' = (mul 0 3 4,mul' 0 3 4)
   -------------------------------
   test4 = sub 11 11 6 == 5
   -----------------------------
   test5 =   div' 0 10 5 10 == 2
          && div' 0 21 5 21 == 4
          && div' 0 32 8 32 == 4
   -----------------------------
   test6 =   less 0 5 4 0 == 0
          && less 0 4 5 0 == 1
          && less 0 5 5 0 == 0
   ---------------------------
   test7 =   nod 0  5  3 == 1
          && nod 0  2  6 == 2
          && nod 0 18 24 == 6
   --------------------------
  {-
    �ணࠬ�� ���᫥��� �ந�������� ���� �ᥫ. 
    R2:=x, R3:=y, R1 - ᮤ�ন� १���� ����樨

   ���������
   I1 J(3,4,20)

   I2 T(1,5)      �������� R5, R6, R7 �ᯮ������� ��� ���᫥���
   I3 T(2,6)       res+x (�. �㭪�� �� �몥 Haskell)
   I4 J(6,7,8)
   I5 S(5)
   I6 S(7)
   I7 J(1,1,4)
   I8 T(5,1)
   I9 Z(7)
                  
   I10 T(3,8)     �������� R8, R9, R10 �ᯮ������� ��� ���᫥���
   I11 J(8,10,16)  y-1 (�. �㭪�� �� �몥 Haskell)              
   I12 S(9)
   I13 J(8,9,16)
   I14 S(10)
   I15 J(1,1,12)
   I16 T(10,3)
   I17 Z(9)
   I18 Z(10)

   I19 J(1,1,1)
  -}
  
   -- *******************************
   -- ���४�஢�� �������� �㭪権
   ------------------------------------------------
   -- ������஢���� ����樨 "㬭������ ���� �ᥫ"
   -- �� ����᪥ res:=0, res':=0, res3':=1
   ----------------------------------------
   mul'':: Int -> Int -> Int -> Int
   mul'' res x y | x==0 = res
                 | True = mul'' (add res res y 0) (sub1 0 x 1) y
   -------------------------------------------------------------
   test9 = (mul 0 3 4, mul' 0 3 4, mul'' 0 3 4)

   -- *********************************************
   -- ������஢���� ����樨 "楫��᫥���� �������
   -- ���� �ᥫ".
   -- �� ����᪥ res:=0, r4:=x
   --------------------------------------
   div'':: Int -> Int -> Int -> Int -> Int
   div'' res x y r4 | less' False r4 y 0 = res
                    | True = div'' (res+1) x y (sub r4 r4 y)
   ---------------------------------------------------------
   test10 = div'' 0 12 4 12
   test11 = div'' 0 12 5 12

   -- ***********************************************
   -- ������஢���� �।���� "�ࠢ����� ���� �ᥫ":
   --
   --        |False, x>=y;
   -- f(x,y)=|
   --        |True,  x<y
   --
   -- �� ����᪥ res:=False, r4:=0
   -----------------------------------------
   less':: Bool -> Int -> Int -> Int -> Bool
   less' res x y r4 | x==y  = res
                    | y==r4 = res
                    | x==r4 = True
                    | True  = less' res x y (r4+1)
   -----------------------------------------------
   test12 = less' False 3 5 0
   test13 = less' False 5 5 0
   test14 = less' False 5 3 0
