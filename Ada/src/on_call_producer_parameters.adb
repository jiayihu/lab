
with Production_Workload;
with Ada.Text_IO;
package body On_Call_Producer_Parameters is
   procedure On_Call_Producer_Operation (Load : Positive) is
   begin
      --  we execute the required amount of excess workload
      Production_Workload.Small_Whetstone (Load);
      --  then we report nominal completion of current activation
      Ada.Text_IO.Put_Line ("End of sporadic activation.");
   end On_Call_Producer_Operation;
end On_Call_Producer_Parameters;
