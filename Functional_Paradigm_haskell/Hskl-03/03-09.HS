   -- ����������  � � � � � � � �  � � � � � � � �  ��
   -- �ਬ�� �㭪権 ��� ��।������ ��⭮�� ������-
   -- ⥫쭮�� 楫��� �᫠
   -- *********************

   -- =================
   isEven:: Int -> Bool
   isEven 0 = True
   isEven n = isOdd (n-1)

   -- ================
   isOdd:: Int -> Bool
   isOdd 0 = False
   isOdd n = isEven (n-1)

   -- ***************************
   -- ��㤠�� ��⮢� �ਬ���:
   --------------------------------------------
   test =   isEven 0 && isEven 2 && isEven 1024
         && isOdd  1 && isOdd  3 && isOdd 1025
         && not (isEven 1)
         && not (isEven 255)
         && not (isOdd 2)
         && not (isOdd 256)
