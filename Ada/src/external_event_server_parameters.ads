--  with Ada.Interrupts.Names;
with System;
package External_Event_Server_Parameters is
   --  a target-specific interrupt
   --  The_Interrupt : constant Ada.Interrupts.Interrupt ID :=
   --       Ada.Interrupts.Names.External_Interrupt_2;
   --  the interrupt priority should be at the appropriate level

   -- @: Dovrebbe essere System.Interrupt_Priority'Last
   Event_Queue_Priority : constant System.Interrupt_Priority :=
     System.Interrupt_Priority'First;

   --  the interrupt sporadic priority is determined by deadline
   --  monotonic analysis
   External_Event_Server_Priority : constant System.Priority := 11;
   procedure Server_Operation;
end External_Event_Server_Parameters;
