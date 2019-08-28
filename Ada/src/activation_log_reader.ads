with Activation_Log_Reader_Parameters;
package Activation_Log_Reader is
   --  non-suspending parameterless operation
   --+  with no queuing of activation requests
   procedure Signal;
   procedure Wait;
   task Activation_Log_Reader
     with Priority =>
       Activation_Log_Reader_Parameters.Activation_Log_Reader_Priority;
       --  assigned by deadline monotonic analysis
end Activation_Log_Reader;
