package body Activation_Log is
   protected body Activation_Log is
      procedure Write is
      begin
         Activation_Counter := Activation_Counter + 1;
         Activation_Time := Ada.Real_Time.Clock;
      end Write;
      procedure Read (Last_Activation  : out Range_Counter;
                      Last_Active_Time : out Ada.Real_Time.Time) is
      begin
         Last_Activation := Activation_Counter;
         Last_Active_Time := Activation_Time;
      end Read;
   end Activation_Log;
end Activation_Log;
