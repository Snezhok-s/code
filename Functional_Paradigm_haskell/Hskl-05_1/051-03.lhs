>   import List (sort,groupBy,(\\),nub,insert)
>   import Random

-- >   import Data.Char

      ��訬 ᫥������ ������: ������� �㭪��, ���ન������
   �� ��������� ᫮�� word �㪢�, ����� ��������� � �� n
   ࠧ. �ᯮ�짮����� �㭪樮����� ��易⥫쭮!
      ����⨬, ��  �㭪樮����� �祭� �����,  ���⮬� ��ࢮ�
   �஡����� ���� �� �롮� (����⭮,  �� �㭪樮���� map
   � filter �ਤ���� �ᯮ�짮����).  �।������� ���砫�, ��
   �㪢� � ᫮�� �����஢���; �� ����� ᤥ���� ⠪ (word -
   �� �室��� ᫮��):
      sort word
      �����, �ᯮ���� �㭪樮���, ��������騩 �믮����� ���-
   ��஢�� ᮢ������� �㪢 � ᫮�� word:
      groupBy (==) $ sort word
      ������, ���ਬ��, ����稬 ��� word="asaswasaszewz":
      "asaswasaszewz" -> ["aaaa","e","ssss","ww","zz"]
      ��⠢�� ⮫쪮 "�㦭�" �㪢� � ������� 䨫���樨:
      filter (\x -> length x==n)
             (groupBy (==) $ sort word)
      ���ਬ��, ��� n=2 ����稬:
      ["aaaa","e","ssss","ww","zz"] -> ["ww","zz"]
      ����塞 ��譨� �㪢�:
      map (\y -> head y)
          (filter (\x -> length x==n)
                  (groupBy (==) $ sort word))
   � ��� ᮪�饭�� ��ᯮ��㥬�� ���몠����:
      where z = map (\y -> head y)
                    (filter (\x -> length x==n)
                            (groupBy (==) $ sort word))
      �⠪, ["ww","zz"] -> ['w','z'].
      ������� 㤠���� "�㪢�", �室�騥 � ᯨ᮪ z, �� ᫮��
   word.
      ����⨬, �� ������� ।�� �ᯮ��㥬�� �㭪�� (\\)
   �� ������⥪� List, ����� �����⢫�� �� 㤠�����; ��-
   ���� �।���⥫쭮 ��室���� �믮����� ᫥���騥  �८�-
   ࠧ������:
      ['w','z'] -> [['w','z'],['w','z']] -> ['w','z','w','z']
                -> "wzwz"
      �������, �㭪�� (\\) �������� �������:
      (\\) "asaswasaszewz" "wzwz" -> "asasasase"
      �⮣��� ���:

>   bca word n = (\\) word (concat (replicate n z))
>           where z = map (\y -> head y)
>                         (filter (\x -> length x==n)
>                                 (groupBy (==) $ sort word))

    ������� �஢��� �ਬ�⨢��� ���஢����:

>   test =   bca "aaaasssssweqew" 4 == "sssssweqew"
>         && bca "aaaasssssweqew" 5 == "aaaaweqew"
>         && bca "aaaasssssweqew" 2 == "aaaasssssq"
>         && bca "aaaasssssweqew" 1 == "aaaasssssweew"
>         && bca "aaaasssssweqew" 0 == "aaaasssssweqew"

      �믮���� ��砩��� ���஢����;  ��� �⮣�  �ந�����
   ������� k ��砩��� ����ࠫ��� �ᥫ �� 97 �� 122 (����
   ������ ��⨭᪨� �㪢 �� 'a' �� 'z'), � ��⥬ �८�ࠧ�-
   ��  ��  �᫠ � ᨬ���� (201 - ��砩��� ����ࠫ쭮� ��-
   ��):

>   wrd k = map chr
>             (take k $ randomRs (97,122) (mkStdGen 201):: [Int])

>   test1 = (w,sort w,sort $ bca w 2,2)
>       where w = wrd 20  

      ������� �����஢��� ��砩�� ��ࠧ�� ��ப� (��� ������
   ���������� �����) ��� ���஢���� �㭪樨 bca:

>   test2 = map (\k -> map chr
>                   (take 20 $ randomRs (97,122) (mkStdGen k):: [Int]))
>               [1,11..200000]

   � �஢��� ���஢����:

>   test3 = map (\w -> (w,sort w,sort $ bca w 1,1)) test2

      � ᮦ������, ��� �஢�ન १���⮢ ���஢���� ����室���
   ������⢨� �ணࠬ����, ����� ����������� �ᯥ譮��� ��⮢.
   ���⮬� ��� ���쭥�襩 ��⮬�⨧�樨 ���஢���� ��ᯮ��㥬��
   �ࠪ㫮� ��⮢, � ����⢥ ���ண� �㤥� �ᯮ�짮���� ��㣠� 
   ॠ������ �㭪樨 bca:

>   -- �㭪��, ���ન����� �� ��������� ᫮��
>   -- �㪢�, ����� ��������� � �� n ࠧ
>   -- (����䥪⨢��� ॠ������)
>   ----------------------------------------------
>   result n word = vsp3 (vsp2 n (vsp1 word)) word
>       where vsp2 n lst = map (\(a,b) -> if b==n then a else '#')
>                              lst
>             vsp3 lst word 
>                 | null word            = []
>                 | elem (head word) lst = vsp3 lst (tail word)
>                 | True                 = head word : 
>                                          vsp3 lst (tail word)
>   -----------------------------------------------------------
>   vsp1 word = map (\x -> (x,poisk x word)) (nub word)
>          where poisk x word 
>                  | null word    = 0
>                  | head word==x = 1 + poisk x (tail word)
>                  | True         = poisk x (tail word)

   �⮣��� ��� �㭪樨 ��� ���஢���� ⠪�� (� GHCi):

>   test4 = map (\w -> (w,bca w 1,1)) lst ==
>           map (\w -> (w,result 1 w,1)) lst
>       where lst = test2 

   ������� ���஢����: True.
   �஡����: � ��, �᫨ �㭪�� test4 ����� False?

>   test5 | num /= length z1 = z1 !! num
>         | True              = ""
>       where num = length $ takeWhile (==True) fun 
>             fun = zipWith (==) (map (\w -> (w,bca w 1)) z1)
>                                (map (\w -> (w,result 1 w)) z2)
>             lst = test2 
>             z1  = insert "abcdefgh" lst
>             z2  = insert "cab" lst 