with Iterators.From.Elements;

package Iterators.Std with Preelaborate is

   --  Iterators for default Ada types: booleans, strings...

   package Booleans is new From.Elements (Boolean);

   package Strings is new From.Elements (String);

end Iterators.Std;
