with On_Call_Producer_Parameters;
package On_Call_Producer is
   --  non-suspending operation with queuing of data
   function Start (Activation_Parameter : Positive) return Boolean;
   task On_Call_Producer
   --  assigned by deadline monotonic analysis
   with Priority =>
      On_Call_Producer_Parameters.On_Call_Producer_Priority;
end On_Call_Producer;
