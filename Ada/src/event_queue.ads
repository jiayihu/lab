with External_Event_Server_Parameters;
package Event_Queue is
   protected Handler
     with Interrupt_Priority =>
       External_Event_Server_Parameters.Event_Queue_Priority
       --  must be in the range of System.Interrupt_Priority
   is
      procedure Signal;
        --  with Attach_Handler => Some_Interrupt_ID;
      entry Wait;
   private
      --  entry barrier must be simple (i.e. boolean expression)
      Barrier : Boolean := False;
   end Handler;
end Event_Queue;
