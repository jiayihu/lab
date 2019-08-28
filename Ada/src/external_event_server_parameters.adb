
with Activation_Log;
package body External_Event_Server_Parameters is
   procedure Server_Operation is
   begin
      --  we record an entry in the Activation_Log buffer
      Activation_Log.Write;
   end Server_Operation;
end External_Event_Server_Parameters;
