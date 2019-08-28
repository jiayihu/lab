package body Event_Queue is
   protected body Handler is
      procedure Signal is
      begin
         Barrier := True;
      end Signal;
      entry Wait when Barrier is
      begin
         Barrier := False;
      end Wait;
   end Handler;
end Event_Queue;
