with Request_Buffer;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
package body On_Call_Producer is
   --  to hide the implementation of the event buffer
   function Start (Activation_Parameter : Positive) return Boolean is
   begin
      return Request_Buffer.Deposit (Activation_Parameter);
   end Start;
   task body On_Call_Producer is
      Current_Workload : Positive;
   begin
      --  for tasks to achieve simultaneous activation
      Activation_Manager.Activation_Sporadic;
      loop
         --  suspending request for activation event with data exchange
         Current_Workload := Request_Buffer.Extract;
         --  non-suspending operation code
         On_Call_Producer_Parameters.On_Call_Producer_Operation
           (Current_Workload);
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("Something has gone wrong here: " & Exception_Information (Error));
   end On_Call_Producer;
end On_Call_Producer;
