with System;
package On_Call_Producer_Parameters is
   On_Call_Producer_Priority : constant System.Priority := 5;
   procedure On_Call_Producer_Operation (Load : Positive);
end On_Call_Producer_Parameters;
