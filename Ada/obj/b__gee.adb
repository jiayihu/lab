pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__gee.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__gee.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "system__soft_links__initialize_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__io_exceptions_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "ada__numerics_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "interfaces__c_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "system__os_lib_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "ada__tags_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "ada__streams_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "system__file_control_block_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "system__finalization_root_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "ada__finalization_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "system__file_io_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "ada__real_time_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ada__text_io_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "system__tasking__initialization_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "system__tasking__protected_objects_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "system__tasking__protected_objects__entries_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "system__tasking__queuing_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "ada__synchronous_task_control_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "system__tasking__stages_E");
   E009 : Short_Integer; pragma Import (Ada, E009, "activation_log_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "activation_manager_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "auxiliary_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "external_event_server_parameters_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "event_queue_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "external_event_server_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "production_workload_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "activation_log_reader_parameters_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "activation_log_reader_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "on_call_producer_parameters_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "request_buffer_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "on_call_producer_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "regular_producer_parameters_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "regular_producer_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "request_buffer__finalize_body");
      begin
         E181 := E181 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "activation_log_reader__finalize_body");
      begin
         E005 := E005 - 1;
         F2;
      end;
      E171 := E171 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "event_queue__finalize_spec");
      begin
         F3;
      end;
      E009 := E009 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "activation_log__finalize_spec");
      begin
         F4;
      end;
      E147 := E147 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "ada__synchronous_task_control__finalize_spec");
      begin
         F5;
      end;
      E159 := E159 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F6;
      end;
      E102 := E102 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__text_io__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__file_io__finalize_body");
      begin
         E121 := E121 - 1;
         F8;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

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

   type No_Param_Proc is access procedure;

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

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, False, True, True, False, False, 
           False, False, False, True, True, True, True, False, 
           False, False, True, False, True, True, False, True, 
           True, False, True, True, True, True, False, False, 
           False, False, False, True, False, False, True, False, 
           False, False, True, True, False, False, False, True, 
           False, False, False, True, False, False, False, False, 
           False, False, False, True, False, True, True, True, 
           False, False, True, False, True, True, True, False, 
           True, True, False, True, True, True, True, False, 
           False, True, False, False, False, True, False, False, 
           True, False, True, False),
         Count => (0, 0, 0, 1, 0, 0, 4, 0, 4, 0),
         Unknown => (False, False, False, False, False, False, False, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E021 := E021 + 1;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E054 := E054 + 1;
      E011 := E011 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E105 := E105 + 1;
      Ada.Numerics'Elab_Spec;
      E132 := E132 + 1;
      Interfaces.C'Elab_Spec;
      E067 := E067 + 1;
      System.Os_Lib'Elab_Body;
      E126 := E126 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E107 := E107 + 1;
      Ada.Streams'Elab_Spec;
      E104 := E104 + 1;
      System.File_Control_Block'Elab_Spec;
      E129 := E129 + 1;
      System.Finalization_Root'Elab_Spec;
      E124 := E124 + 1;
      Ada.Finalization'Elab_Spec;
      E122 := E122 + 1;
      System.File_Io'Elab_Body;
      E121 := E121 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E100 := E100 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E102 := E102 + 1;
      System.Tasking.Initialization'Elab_Body;
      E153 := E153 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E058 := E058 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E159 := E159 + 1;
      System.Tasking.Queuing'Elab_Body;
      E157 := E157 + 1;
      Ada.Synchronous_Task_Control'Elab_Spec;
      E147 := E147 + 1;
      System.Tasking.Stages'Elab_Body;
      E163 := E163 + 1;
      Activation_Log'Elab_Spec;
      E009 := E009 + 1;
      Activation_Manager'Elab_Spec;
      E143 := E143 + 1;
      E188 := E188 + 1;
      E173 := E173 + 1;
      Event_Queue'Elab_Spec;
      E171 := E171 + 1;
      External_Event_Server'Elab_Spec;
      External_Event_Server'Elab_Body;
      E175 := E175 + 1;
      Production_Workload'Elab_Spec;
      E131 := E131 + 1;
      E007 := E007 + 1;
      Activation_Log_Reader'Elab_Spec;
      Activation_Log_Reader'Elab_Body;
      E005 := E005 + 1;
      E179 := E179 + 1;
      Request_Buffer'Elab_Body;
      E181 := E181 + 1;
      On_Call_Producer'Elab_Spec;
      On_Call_Producer'Elab_Body;
      E177 := E177 + 1;
      E186 := E186 + 1;
      Regular_Producer'Elab_Spec;
      Regular_Producer'Elab_Body;
      E184 := E184 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_gee");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/activation_log_parameters.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/activation_log.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/activation_manager.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/auxiliary.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/external_event_server_parameters.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/event_queue.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/external_event_server.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/production_workload.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/activation_log_reader_parameters.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/activation_log_reader.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/on_call_producer_parameters.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/request_buffer_parameters.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/request_buffer.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/on_call_producer.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/regular_producer_parameters.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/regular_producer.o
   --   /Users/jiayihu/Downloads/Guide Extended Example/obj/gee.o
   --   -L/Users/jiayihu/Downloads/Guide Extended Example/obj/
   --   -L/Users/jiayihu/Downloads/Guide Extended Example/obj/
   --   -L/users/jiayihu/opt/gnat/2019/lib/gcc/x86_64-apple-darwin17.7.0/8.3.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
