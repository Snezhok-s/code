   -- Укажите  п р а г м а т и к у  (цель, назначение)
   -- приведённых ниже функций
   -- ************************
   ud1:: [Integer] -> Bool 
   ud1 lst = elem ((sum . tail . init) lst) lst

   -----------------------
   ud2:: [Integer] -> Bool 
   ud2 lst = elem (product [1..minimum lst]) lst

   -----------------------------------
   ud3 lst = elem (sum (ud3' lst)) lst

   -----------------------------
   ud3':: [Integer] -> [Integer]
   ud3' []       = []
   ud3' [x]      = []       
   ud3' (x:y:xs) = y : ud3' xs

   --------------------------
   ud4 lst = elem (a ^ b) lst
          where a = (head . tail) lst
                b = last lst
                 
   -- ***************************
   -- Неудачные тестовые примеры:
   ------------------------------
   test1 = ud1 [1,2,3,4,9] 
   test2 = ud2 [3,4,9]
   test3 = ud3 [1,2,6,4,9]
