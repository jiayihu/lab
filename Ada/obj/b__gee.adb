pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__gee.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__gee.adb");
pragma Suppress (Overflow_Check);

package body ada_main is

   E104 : Short_Integer; pragma Import (Ada, E104, "ada__tags_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "system__soft_links_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "system__exception_table_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "system__bb__timing_events_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada__real_time_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "system__bb__execution_time_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__tasking__protected_objects_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "system__tasking__protected_objects__multiprocessors_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "system__tasking__restricted__stages_E");
   E009 : Short_Integer; pragma Import (Ada, E009, "activation_log_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "activation_manager_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "auxiliary_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "external_event_server_parameters_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "event_queue_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "external_event_server_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "production_workload_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "activation_log_reader_parameters_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "activation_log_reader_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "on_call_producer_parameters_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "request_buffer_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "on_call_producer_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "regular_producer_parameters_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "regular_producer_E");

   Sec_Default_Sized_Stacks : array (1 .. 5) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := 0;
      Time_Slice_Value := 0;
      WC_Encoding := 'b';
      Locking_Policy := 'C';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := 'F';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 1;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 5;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E049 := E049 + 1;
      Ada.Tags'Elab_Body;
      E104 := E104 + 1;
      System.Bb.Timing_Events'Elab_Spec;
      E118 := E118 + 1;
      E051 := E051 + 1;
      Ada.Real_Time'Elab_Body;
      E125 := E125 + 1;
      System.Bb.Execution_Time'Elab_Body;
      E174 := E174 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E011 := E011 + 1;
      System.Tasking.Protected_Objects.Multiprocessors'Elab_Body;
      E150 := E150 + 1;
      System.Tasking.Restricted.Stages'Elab_Body;
      E155 := E155 + 1;
      Activation_Log'Elab_Spec;
      E009 := E009 + 1;
      Activation_Manager'Elab_Spec;
      E142 := E142 + 1;
      E178 := E178 + 1;
      E159 := E159 + 1;
      Event_Queue'Elab_Spec;
      E157 := E157 + 1;
      External_Event_Server'Elab_Spec;
      External_Event_Server'Elab_Body;
      E161 := E161 + 1;
      E129 := E129 + 1;
      E007 := E007 + 1;
      Activation_Log_Reader'Elab_Spec;
      Activation_Log_Reader'Elab_Body;
      E005 := E005 + 1;
      E165 := E165 + 1;
      Request_Buffer'Elab_Body;
      E167 := E167 + 1;
      On_Call_Producer'Elab_Spec;
      On_Call_Producer'Elab_Body;
      E163 := E163 + 1;
      E176 := E176 + 1;
      Regular_Producer'Elab_Spec;
      Regular_Producer'Elab_Body;
      E170 := E170 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_gee");

   procedure main is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
   end;

--  BEGIN Object file/option list
   --   /home/parallels/Desktop/repo/lab/Ada/obj/activation_log_parameters.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/activation_log.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/activation_manager.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/auxiliary.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/external_event_server_parameters.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/event_queue.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/external_event_server.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/production_workload.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/activation_log_reader_parameters.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/activation_log_reader.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/on_call_producer_parameters.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/request_buffer_parameters.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/request_buffer.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/on_call_producer.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/regular_producer_parameters.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/regular_producer.o
   --   /home/parallels/Desktop/repo/lab/Ada/obj/gee.o
   --   -L/home/parallels/Desktop/repo/lab/Ada/obj/
   --   -L/home/parallels/Desktop/repo/lab/Ada/obj/
   --   -L/home/parallels/opt/GNAT/2018-arm-elf/arm-eabi/lib/gnat/ravenscar-full-stm32f4/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
