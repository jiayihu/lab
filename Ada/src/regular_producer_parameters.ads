with System;
package Regular_Producer_Parameters is
   Regular_Producer_Priority : constant System.Priority := 7;
   Regular_Producer_Period : constant Natural := 1_000; -- in milliseconds
   procedure Regular_Producer_Operation;
end Regular_Producer_Parameters;
