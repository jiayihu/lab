package Auxiliary is
   type Range_Counter is mod 5;
   function Due_Activation (Param : Range_Counter) return Boolean;
   type Run_Counter is mod 1_000;
   Factor : constant Natural := 3;
   function Check_Due return Boolean;
end Auxiliary;
