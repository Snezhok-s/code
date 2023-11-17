   -- ������������ ������������� ����������
   -- ��� ������ � ���������
   ---------------------------------------
   import Prelude hiding (isAlpha,isUpper,
                          isLower,isDigit)
   ---------------------------------------
   isAlpha:: Char -> Bool
   isAlpha c = isUpper c || isLower c
   ----------------------------------
   isUpper:: Char -> Bool
   isUpper c = c>='A' && c<='Z'
   ----------------------------
   isLower:: (->) Char Bool
   isLower c = c>='a' && c<='z'
   ----------------------------
   isDigit:: Char -> Bool
   isDigit ch = ord ch >=48 && ord ch<=57
   --------------------------------------------------------
   -- �������, ������������ �� ���� ������� ��������������� 
   -- ������ � "���������������" ��������
   --------------------------------------
   abc:: Int -> Char   
   abc n | not (isAlpha w) = error "�� ���������� ������"
         | isLower w       = toUpper w 
         | True            = toLower w
                                where w = chr n
   --------------------------------------------
   -- ��������� �������� �������:
   ------------------------------------
   test1 =   isAlpha 'a' && isAlpha 'F'
          && isUpper 'B' && isLower 'a'
   ------------------------------------ 
   test2 =   abc 65=='a' && abc 90=='z' 
          && abc 97=='A' && abc 122=='Z' 
   -------------------------------------
   test3 =   isDigit '4' && isDigit '7'
          && isDigit '9'
