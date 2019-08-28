with External_Event_Server_Parameters;
package External_Event_Server is
   task External_Event_Server
     with Priority =>
       External_Event_Server_Parameters.External_Event_Server_Priority;
end External_Event_Server;
