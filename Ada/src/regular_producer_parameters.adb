with On_Call_Producer;
with Production_Workload;
with Activation_Log_Reader;
with Auxiliary;
with Ada.Text_IO;
package body Regular_Producer_Parameters is
   --  approximately 5,001,000 processor cycles of Whetstone load
   --  on an ERC32 (a radiation-hardened SPARC for space use) at 10 Hz
   Regular_Producer_Workload : constant Positive := 756;
   --  approximately 2,500,500 processor cycles
   On_Call_Producer_Workload : constant Positive := 278;
   --  the parameter used to query the condition
   --  for the activation of On_Call_Producer
   Activation_Condition : constant Auxiliary.Range_Counter := 2;
   procedure Regular_Producer_Operation is
   begin
      --  we execute the guaranteed level of workload
      Production_Workload.Small_Whetstone (Regular_Producer_Workload);
      --  then we check whether we need to farm excess load out to
      --  On_Call_Producer
      if Auxiliary.Due_Activation (Activation_Condition) then
         --  if yes, then we issue the activation request with a parameter
         --  that determines the workload request
         if not On_Call_Producer.Start (On_Call_Producer_Workload) then
            --  we capture and report failed activation
            Ada.Text_IO.Put_Line ("Failed sporadic activation.");
         end if;
      end if;
      --  we check whether we need to release Activation_Log
      if Auxiliary.Check_Due then
         Activation_Log_Reader.Signal;
      end if;
      --  finally we report nominal completion of the current activation
      Ada.Text_IO.Put_Line ("End of cyclic activation.");
   end Regular_Producer_Operation;
end Regular_Producer_Parameters;
