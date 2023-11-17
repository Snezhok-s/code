   { Демонстрация описания и вызова функционала Map. }
   {                                                 }
   { Функция, формальным параметром которой является }
   { функция, называется  ф у н к ц и о н а л о м.   }
   {                                                 }
   {  Автор: М.В.Швецкий (27.11.2016)                }
   { ----------------------------------------------- }
   PROGRAM Primer_9;
      {$F+}                      { Опция компилятора }
      Uses CRT;
      const N=10;
      { ----------------------------- }
      type IA = Array[1..N] of Integer;
           F  = Function (X: Char)   : Char;
           F1 = Function (X: Integer): Integer;
           p  = ^IA;             { Тип "указатель на массив" }
      { ---------------------------------------------------- }
      var  Fun  : F;
           Fun1 : F1;
           g    : p;             { Указатель на массив }
           w    : p;             { Указатель на массив }
           i    : Integer;
           Slovo: String;
  { ------------------------- }
   FUNCTION R (x: Char): Char;
   BEGIN
      R := UpCase(x);
    { R := Chr(Ord(x)+32); }
   END;
  { -------------------------------- }
   FUNCTION R1 (x: Integer): Integer;
   BEGIN
      R1 := x Div 2
   END;

   { ------------------------------------- }
   { Моделирование функционала map         }
   { ------------------------------------- }
   FUNCTION Map (G: F; Str: String): String;
      var i: Integer;
          r: String;
   BEGIN
      r:='';
      For i:=1 to Length(Str) do
        r := Concat(r,G(Str[i]));
      Map := r
   END;

   { ----------------------------- }
   { Моделирование функционала map }
   { ----------------------------- }
   FUNCTION Map_1 (G: F1; d: p): p;
      var i: Integer;
   BEGIN
      For i:=1 to N do
        d^[i] := G(d^[i]);
      Map_1 := d
   END;
  { --- }
   BEGIN
      Randomize;
      WriteLn; Write('Элементы массива: ');
      For i:=1 to N do
        begin g^[i]:=Random(80); Write(g^[i]:2,' ') end;
      WriteLn;
      w:=Map_1(R1,g);
      Write('Результат       : ');
      For i:=1 to N do Write(w^[i]:2,' ');
      WriteLn; WriteLn;
      { ------------- }
      Slovo:='';
      For i:=1 to 10 do
        Slovo := Slovo + Chr(Ord('a')+Random(5));
      WriteLn('Слово           : ',Slovo);
      WriteLn('Результат       : ',Map(R,Slovo));
      WriteLn;
      Repeat until KeyPressed
   END.
