with Ada.Real_Time;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
package body Regular_Producer is
   Period : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds
       (Regular_Producer_Parameters.Regular_Producer_Period);
   task body Regular_Producer is
      use Ada.Real_Time;
      --  for periodic suspension
      Next_Time : Ada.Real_Time.Time;
   begin
      --  for tasks to achieve simultaneous activation
      Activation_Manager.Activation_Cyclic (Next_Time);
      loop
         Next_Time := Next_Time + Period;
         --  non-suspending operation code
         Regular_Producer_Parameters.Regular_Producer_Operation;
         --  time-based activation event
         delay until Next_Time; --  delay statement at end of loop
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("Something has gone wrong here: " & Exception_Information (Error));
   end Regular_Producer;
end Regular_Producer;
