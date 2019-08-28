with Production_Workload;
with Activation_Log;
with Ada.Real_Time;
with Ada.Text_IO;
package body Activation_Log_Reader_Parameters is
   --  approximately 1,250,250 processor cycles of Whetstone load
   --  on an ERC32 (a radiation-hardened SPARC for space use) at 10 Hz
   Load : constant Positive := 139;
   procedure Activation_Log_Reader_Operation is
      Interrupt_Arrival_Counter : Activation_Log.Range_Counter := 0;
      Interrupt_Arrival_Time : Ada.Real_Time.Time;
   begin
      --  we perform some work
      Production_Workload.Small_Whetstone (Load);
      --  then we read into the Activation_Log buffer
      Activation_Log.Activation_Log.Read (Interrupt_Arrival_Counter,
         Interrupt_Arrival_Time);
      --  and finally we report nominal completion of current activation
      Ada.Text_IO.Put_Line ("End of parameterless sporadic activation.");
   end Activation_Log_Reader_Operation;
end Activation_Log_Reader_Parameters;
