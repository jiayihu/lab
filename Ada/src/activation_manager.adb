package body Activation_Manager is
   procedure Activation_Sporadic is
   begin
      delay until Activation_Time;
   end Activation_Sporadic;

   procedure Activation_Cyclic
        (Next_Time : out Ada.Real_Time.Time) is
   begin
      Next_Time := Activation_Time;
      delay until Activation_Time;
   end Activation_Cyclic;
end Activation_Manager;
