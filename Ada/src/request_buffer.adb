
with Request_Buffer_Parameters;
package body Request_Buffer is
   type Request_Buffer_Index is
      mod Request_Buffer_Parameters.Request_Buffer_Range;
   type Request_Buffer_T is array (Request_Buffer_Index) of Positive;
   protected Request_Buffer
     with Priority =>
       Request_Buffer_Parameters.Request_Buffer_Priority
       --  must be ceiling of users' priority
   is
      procedure Deposit
           (Activation_Parameter : Positive;
            Response             : out Boolean);
      entry Extract (Activation_Parameter : out Positive);
   private
      My_Request_Buffer : Request_Buffer_T;
      Insert_Index : Request_Buffer_Index := Request_Buffer_Index'First;
      Extract_Index : Request_Buffer_Index := Request_Buffer_Index'First;
      --  the Request_Buffer is initially empty
      Current_Size : Natural := 0;
      --  the guard is initially closed
      --  so that the first call to Extract will block
      Barrier : Boolean := False;
   end Request_Buffer;
   --  we encapsulate the call to protected procedure Deposit in a function
   --  that returns a Boolean value designating the success or failure of
   --  the operation. This coding style allows for a more elegant coding
   --  of the call
   function Deposit (Activation_Parameter : Positive) return Boolean is
      Response : Boolean;
   begin
      Request_Buffer.Deposit (Activation_Parameter, Response);
      return Response;
   end Deposit;
   --  we encapsulate the call to protected entry Extract in a function
   --  that returns the Positive value designating the workload level passed
   --  by Regular_Producer on to On_Call_Producer. This coding style allows
   --  for a more elegant coding of the call
   function Extract return Positive is
      Activation_Parameter : Positive;
   begin
      Request_Buffer.Extract (Activation_Parameter);
      return Activation_Parameter;
   end Extract;
   protected body Request_Buffer is
      entry Extract (Activation_Parameter : out Positive)
         when Barrier is
      begin
         Activation_Parameter := My_Request_Buffer (Extract_Index);
         Extract_Index := Extract_Index + 1;
         Current_Size := Current_Size - 1;
         --  we close the barrier when the buffer is empty
         --  this also prevents the counter from becoming negative
         Barrier := (Current_Size /= 0);
      end Extract;
      procedure Deposit
           (Activation_Parameter : Positive;
            Response             : out Boolean) is
      begin
         if Current_Size < Natural (Request_Buffer_Index'Last) then
            My_Request_Buffer (Insert_Index) := Activation_Parameter;
            Insert_Index := Insert_Index + 1;
            Current_Size := Current_Size + 1;
            Barrier := True;
            Response := True;
         else
            --  there is no room for insertion, hence the Deposit returns
            --  with a failure (we might have used as well an over-writing
            --  policy as long as the call returned)
            Response := False;
         end if;
      end Deposit;
   end Request_Buffer;
end Request_Buffer;
