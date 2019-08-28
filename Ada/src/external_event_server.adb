with Event_Queue;
with Activation_Manager;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
package body External_Event_Server is
   procedure Wait renames Event_Queue.Handler.Wait;
   task body External_Event_Server is
   begin
      --  for tasks to achieve simultaneous activation
      Activation_Manager.Activation_Sporadic;
      loop
         --  suspending request for external activation event
         Wait;
         --  non-suspending operation code
         External_Event_Server_Parameters.Server_Operation;
      end loop;
   exception
      when Error : others =>
         --  last rites: for example
         Ada.Text_IO.Put_Line
           ("Something has gone wrong here: " & Exception_Information (Error));
   end External_Event_Server;
end External_Event_Server;
