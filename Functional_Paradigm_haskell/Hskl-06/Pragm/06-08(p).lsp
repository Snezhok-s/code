   ; ���������� �㭪樨, �������饩 ����� ᠬ�� ������� ��᫥-
   ; ����⥫쭮�� ����������� ᨬ����� � �����⮬ ᫮��
   ; -----------------------------------------------------
   (DEFUN F (LAMBDA (Word K L)
      (BCA (UNPACK Word) K L)
   ))
   ; ---------------------------
   (DEFUN BCA (LAMBDA (Word K L)
      (COND ( (NULL Word) L )
            ( (NULL (CDR Word)) (MAX (+ K 1) L) ) 
            ( (EQ (CAR Word) (CADR Word))
                                (BCA (CDR Word) (+ K 1) L) )
            (  T  (BCA (CDR Word) 0 (MAX (+ K 1) L)) )
      )
   ))
   ; ---------------------------
   ; ��㤠�� ��⮢� �ਬ���:
   ; --------------------------------
   (EQUAL (F asdasdfadfaaaaaa 0 0) 6)
   (EQUAL (F ABCDEFGH         0 0) 1)
   (EQUAL (F SssSSBSSSnSS     0 0) 5)
   (EQUAL (F A                0 0) 1)  
   ; --------------------------------
   (RDS)