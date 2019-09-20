with Ada.Real_Time; use Ada.Real_Time;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Execution_Time; use Ada.Execution_Time;

package body Regular_Producer is
   Period : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds
       (Regular_Producer_Parameters.Regular_Producer_Period);
   task body Regular_Producer is
      --  for periodic suspension
      Next_Time : Ada.Real_Time.Time;

      -- to measure WCET
      Exe_Time : Duration;
      Last : CPU_Time := Ada.Execution_Time.Clock;
   begin
      --  for tasks to achieve simultaneous activation
      Activation_Manager.Activation_Cyclic (Next_Time);
      loop
         Next_Time := Next_Time + Period;
         --  non-suspending operation code
         Regular_Producer_Parameters.Regular_Producer_Operation;

         Exe_Time := To_Duration (Ada.Execution_Time.Clock - Last);
         Ada.Text_IO.Put_Line ("Elapsed time: " & Duration'Image (Exe_Time) & " seconds");
         Last := Ada.Execution_Time.Clock;

         Next_Time := Next_Time + Period;
         
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
