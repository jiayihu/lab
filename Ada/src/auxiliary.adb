
package body Auxiliary is
   Request_Counter : Range_Counter := 0;
   Run_Count : Run_Counter := 0;
   --  we establish an arbitrary criterion for the activation of
   --  On_Call_Producer
   function Due_Activation (Param : Range_Counter) return Boolean is
   begin
      Request_Counter := Request_Counter + 1;

      return (Request_Counter = Param);
   end Due_Activation;
   --  we establish an arbitrary criterion for the activation of
   --  Activation_Log_Reader
   function Check_Due return Boolean is
      Divisor : Natural;
   begin
      Run_Count := Run_Count + 1;
      Divisor := Natural (Run_Count) / Factor;
      --  we force a check due according to an arbitrary criterion
      return ((Divisor * Factor) = Natural (Run_Count));
   end Check_Due;
end Auxiliary;
