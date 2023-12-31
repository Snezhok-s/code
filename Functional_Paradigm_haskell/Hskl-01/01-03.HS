   -- ���������� ᨭ⠪�� � ᥬ��⨪� �᫮����� ��ࠦ����,
   -- �࣠�����饣� ������⢥���� ��⢫����
   -- ******************************************************
   -- �㭪��, �뢮���� �� ��ࢮ� 㪠������ �㪢�  ��䠢��
   -- (������᪮�� � ���᪮��) �������� �㭪樮���쭮�� ��-
   -- �� �ணࠬ��஢����
   ----------------------
   langv:: Char -> String
   langv s = case (s) of
                 ('C') -> "Curry, Objective Caml, Clean"
                 ('E') -> "Erlang"
                 ('G') -> "Gofer"
                 ('H') -> "Haskell"
                 ('I') -> "ISWIM"
                 ('J') -> "Joy"
                 ('L') -> "LISP"
                 ('M') -> "Miranda, ML, Standart ML"
                 ('S') -> "Scheme"
                 ('�') -> "��䠫"
                 (_)   -> "�㭪樮����� �� ��� �������⥭"
   -----------------------------------------------------------
   -- �㭪��, ��������� �㬬� �ᥫ, ��⠢�����
   -- 㪠������ ���� ஦����� ����:     
   --
   --  ��᫮ "��������_�����" ���
   ---------------------------------------------
   day:: Integer -> [Char] -> Integer -> Integer 
   day d m y | d<1 || d>31 = error "������ ��� � �������� ���"
             | True = case (d,m,y) of 
                        (_, "������  ", _) -> d+y+ 1
                        (_, "���ࠫ� ", _) -> d+y+ 2
                        (_, "����   ", _) -> d+y+ 3
                        (_, "��५�  ", _) -> d+y+ 4
                        (_, "���     ", _) -> d+y+ 5
                        (_, "���    ", _) -> d+y+ 6
                        (_, "���    ", _) -> d+y+ 7
                        (_, "������ ", _) -> d+y+ 8
                        (_, "�������", _) -> d+y+ 9
                        (_, "������ ", _) -> d+y+10
                        (_, "�����  ", _) -> d+y+11
                        (_, "������� ", _) -> d+y+12
                        (_,_,_) -> error "����� � �������� ���"
   -- ***********************************************************
   -- ��㤠�� ��⮢� �ਬ���:
   ------------------------------
   test = test1 && test2
   -----------------------------
   test1 =   langv 'L' == "LISP"
          && langv 'H' == "Haskell"
          && langv 'C' == "Curry, Objective Caml, Clean"
          && langv 'S' == "Scheme"
          && langv '�' == "��䠫"
          && langv 'R' == "�㭪樮����� �� ��� �������⥭"
   -----------------------------------------------------------
   test2 =   day 01 "������  " 1550 == 1552
          && day 23 "���ࠫ� " 1230 == 1255
          && day 08 "����   " 1100 == 1111
          && day 01 "��५�  " 2007 == 2012
          && day 01 "�������"  900 ==  910
   ----------------------------------------
   test3 = day (-24) "�����  " 2004 
   test4 = day   24  "��"       2004
