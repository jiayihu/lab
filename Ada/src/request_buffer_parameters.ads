with System;
package Request_Buffer_Parameters is

   Request_Buffer_Priority : constant System.Priority := 9;
   --  proper analysis will determine the appropriate size of the request
   --  buffer
   Request_Buffer_Range : constant Positive := 5;
end Request_Buffer_Parameters;
