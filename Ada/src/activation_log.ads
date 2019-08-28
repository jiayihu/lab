with Activation_Log_Parameters;
with Ada.Real_Time;
package Activation_Log is
   type Range_Counter is mod 100;
   protected Activation_Log
     with Priority =>
       Activation_Log_Parameters.Activation_Log_Priority
   is
      procedure Write;
      --  retrieves the last activation record: non-suspending operation
      procedure Read
           (Last_Activation  : out Range_Counter;
            Last_Active_Time : out Ada.Real_Time.Time);
   private
      Activation_Counter : Range_Counter := 0;
      Activation_Time : Ada.Real_Time.Time;
   end Activation_Log;

   -- @: Esporta la Write del PO
   procedure Write renames Activation_Log.Write;
   procedure Read
        (Last_Activation  : out Range_Counter;
         Last_Active_Time : out Ada.Real_Time.Time)
     renames Activation_Log.Read;
end Activation_Log;
