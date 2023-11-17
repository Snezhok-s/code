
-----------------------
   
   -- Сторона квадрата:
   a = 10

   -- Стороны прямоугольника:
   b = 9
   c = 11

   sSquare    a   = a*a
   sRectangle b c = b*c

   sBigger sSquare sRectangle | sSquare > sRectangle = "Квадрат"
                              | otherwise            = "Квадрат"

-----------------------
   test = sSquare 5 == 25 &&
          sSquare 0 == 0  && 
          sSquare 2 == 4  &&
          sRectangle 1 2 == 2  &&
          sRectangle 9 9 == 81 &&
          sRectangle 7 4 == 28 &&
          sBigger 1 2 == 2 &&
          sBigger 1 1 == 1 &&
          sBigger 3 2 == 3
-----------------------
  
