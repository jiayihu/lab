pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Community 2019 (20190517-83)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_gee" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#ed2b5a37#;
   pragma Export (C, u00001, "geeB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0f7d71d4#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#aef99703#;
   pragma Export (C, u00004, "activation_log_readerB");
   u00005 : constant Version_32 := 16#b714540f#;
   pragma Export (C, u00005, "activation_log_readerS");
   u00006 : constant Version_32 := 16#aa19594d#;
   pragma Export (C, u00006, "activation_log_reader_parametersB");
   u00007 : constant Version_32 := 16#695e751b#;
   pragma Export (C, u00007, "activation_log_reader_parametersS");
   u00008 : constant Version_32 := 16#5444e136#;
   pragma Export (C, u00008, "activation_logB");
   u00009 : constant Version_32 := 16#f22a4922#;
   pragma Export (C, u00009, "activation_logS");
   u00010 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#4d58644d#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#085b6ffb#;
   pragma Export (C, u00012, "systemS");
   u00013 : constant Version_32 := 16#bd45c2cc#;
   pragma Export (C, u00013, "system__secondary_stackB");
   u00014 : constant Version_32 := 16#4dcf97e2#;
   pragma Export (C, u00014, "system__secondary_stackS");
   u00015 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00015, "adaS");
   u00016 : constant Version_32 := 16#d90c4a0d#;
   pragma Export (C, u00016, "ada__exceptionsB");
   u00017 : constant Version_32 := 16#16307b94#;
   pragma Export (C, u00017, "ada__exceptionsS");
   u00018 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00018, "ada__exceptions__last_chance_handlerB");
   u00019 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00019, "ada__exceptions__last_chance_handlerS");
   u00020 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00020, "system__exception_tableB");
   u00021 : constant Version_32 := 16#55f506b9#;
   pragma Export (C, u00021, "system__exception_tableS");
   u00022 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00022, "system__exceptionsB");
   u00023 : constant Version_32 := 16#6038020d#;
   pragma Export (C, u00023, "system__exceptionsS");
   u00024 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00024, "system__exceptions__machineB");
   u00025 : constant Version_32 := 16#d27d9682#;
   pragma Export (C, u00025, "system__exceptions__machineS");
   u00026 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00026, "system__exceptions_debugB");
   u00027 : constant Version_32 := 16#76d1963f#;
   pragma Export (C, u00027, "system__exceptions_debugS");
   u00028 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00028, "system__img_intB");
   u00029 : constant Version_32 := 16#0a808f39#;
   pragma Export (C, u00029, "system__img_intS");
   u00030 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00030, "system__storage_elementsB");
   u00031 : constant Version_32 := 16#259825ff#;
   pragma Export (C, u00031, "system__storage_elementsS");
   u00032 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#5679b13f#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00034, "system__traceback_entriesB");
   u00035 : constant Version_32 := 16#0800998b#;
   pragma Export (C, u00035, "system__traceback_entriesS");
   u00036 : constant Version_32 := 16#bb296fbb#;
   pragma Export (C, u00036, "system__traceback__symbolicB");
   u00037 : constant Version_32 := 16#c84061d1#;
   pragma Export (C, u00037, "system__traceback__symbolicS");
   u00038 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00038, "ada__exceptions__tracebackB");
   u00039 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00039, "ada__exceptions__tracebackS");
   u00040 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00040, "system__address_imageB");
   u00041 : constant Version_32 := 16#a9b7f2c1#;
   pragma Export (C, u00041, "system__address_imageS");
   u00042 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00042, "system__wch_conB");
   u00043 : constant Version_32 := 16#13264d29#;
   pragma Export (C, u00043, "system__wch_conS");
   u00044 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00044, "system__wch_stwB");
   u00045 : constant Version_32 := 16#3e376128#;
   pragma Export (C, u00045, "system__wch_stwS");
   u00046 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00046, "system__wch_cnvB");
   u00047 : constant Version_32 := 16#1c91f7da#;
   pragma Export (C, u00047, "system__wch_cnvS");
   u00048 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00048, "interfacesS");
   u00049 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00049, "system__wch_jisB");
   u00050 : constant Version_32 := 16#9ce1eefb#;
   pragma Export (C, u00050, "system__wch_jisS");
   u00051 : constant Version_32 := 16#86dbf443#;
   pragma Export (C, u00051, "system__parametersB");
   u00052 : constant Version_32 := 16#40b73bd0#;
   pragma Export (C, u00052, "system__parametersS");
   u00053 : constant Version_32 := 16#75bf515c#;
   pragma Export (C, u00053, "system__soft_links__initializeB");
   u00054 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00054, "system__soft_links__initializeS");
   u00055 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00055, "system__stack_checkingB");
   u00056 : constant Version_32 := 16#86e40413#;
   pragma Export (C, u00056, "system__stack_checkingS");
   u00057 : constant Version_32 := 16#9fcf5d7f#;
   pragma Export (C, u00057, "system__tasking__protected_objectsB");
   u00058 : constant Version_32 := 16#15001baf#;
   pragma Export (C, u00058, "system__tasking__protected_objectsS");
   u00059 : constant Version_32 := 16#3b415298#;
   pragma Export (C, u00059, "system__soft_links__taskingB");
   u00060 : constant Version_32 := 16#e939497e#;
   pragma Export (C, u00060, "system__soft_links__taskingS");
   u00061 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00061, "ada__exceptions__is_null_occurrenceB");
   u00062 : constant Version_32 := 16#e1d7566f#;
   pragma Export (C, u00062, "ada__exceptions__is_null_occurrenceS");
   u00063 : constant Version_32 := 16#fde20231#;
   pragma Export (C, u00063, "system__task_primitivesS");
   u00064 : constant Version_32 := 16#352452d1#;
   pragma Export (C, u00064, "system__os_interfaceB");
   u00065 : constant Version_32 := 16#b9c37c0a#;
   pragma Export (C, u00065, "system__os_interfaceS");
   u00066 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00066, "interfaces__cB");
   u00067 : constant Version_32 := 16#467817d8#;
   pragma Export (C, u00067, "interfaces__cS");
   u00068 : constant Version_32 := 16#64ad9f76#;
   pragma Export (C, u00068, "interfaces__c__extensionsS");
   u00069 : constant Version_32 := 16#b870d14d#;
   pragma Export (C, u00069, "system__os_constantsS");
   u00070 : constant Version_32 := 16#7a0a06a1#;
   pragma Export (C, u00070, "system__task_primitives__operationsB");
   u00071 : constant Version_32 := 16#1951cab5#;
   pragma Export (C, u00071, "system__task_primitives__operationsS");
   u00072 : constant Version_32 := 16#89b55e64#;
   pragma Export (C, u00072, "system__interrupt_managementB");
   u00073 : constant Version_32 := 16#1a73cd21#;
   pragma Export (C, u00073, "system__interrupt_managementS");
   u00074 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00074, "system__multiprocessorsB");
   u00075 : constant Version_32 := 16#30f7f088#;
   pragma Export (C, u00075, "system__multiprocessorsS");
   u00076 : constant Version_32 := 16#2b2125d3#;
   pragma Export (C, u00076, "system__os_primitivesB");
   u00077 : constant Version_32 := 16#0fa60a0d#;
   pragma Export (C, u00077, "system__os_primitivesS");
   u00078 : constant Version_32 := 16#e0fce7f8#;
   pragma Export (C, u00078, "system__task_infoB");
   u00079 : constant Version_32 := 16#8841d2fa#;
   pragma Export (C, u00079, "system__task_infoS");
   u00080 : constant Version_32 := 16#2281c1c8#;
   pragma Export (C, u00080, "system__taskingB");
   u00081 : constant Version_32 := 16#34147ee0#;
   pragma Export (C, u00081, "system__taskingS");
   u00082 : constant Version_32 := 16#3cdd1378#;
   pragma Export (C, u00082, "system__unsigned_typesS");
   u00083 : constant Version_32 := 16#6ec3c867#;
   pragma Export (C, u00083, "system__stack_usageB");
   u00084 : constant Version_32 := 16#3a3ac346#;
   pragma Export (C, u00084, "system__stack_usageS");
   u00085 : constant Version_32 := 16#4e0ce0a1#;
   pragma Export (C, u00085, "system__crtlS");
   u00086 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00086, "system__ioB");
   u00087 : constant Version_32 := 16#961998b4#;
   pragma Export (C, u00087, "system__ioS");
   u00088 : constant Version_32 := 16#1036f432#;
   pragma Export (C, u00088, "system__tasking__debugB");
   u00089 : constant Version_32 := 16#de1ac8b1#;
   pragma Export (C, u00089, "system__tasking__debugS");
   u00090 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00090, "system__concat_2B");
   u00091 : constant Version_32 := 16#0afbb82b#;
   pragma Export (C, u00091, "system__concat_2S");
   u00092 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00092, "system__concat_3B");
   u00093 : constant Version_32 := 16#032b335e#;
   pragma Export (C, u00093, "system__concat_3S");
   u00094 : constant Version_32 := 16#273384e4#;
   pragma Export (C, u00094, "system__img_enum_newB");
   u00095 : constant Version_32 := 16#6917693b#;
   pragma Export (C, u00095, "system__img_enum_newS");
   u00096 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00096, "system__img_lliB");
   u00097 : constant Version_32 := 16#19143a2a#;
   pragma Export (C, u00097, "system__img_lliS");
   u00098 : constant Version_32 := 16#59776cb6#;
   pragma Export (C, u00098, "activation_log_parametersS");
   u00099 : constant Version_32 := 16#09924dd9#;
   pragma Export (C, u00099, "ada__real_timeB");
   u00100 : constant Version_32 := 16#69ea8064#;
   pragma Export (C, u00100, "ada__real_timeS");
   u00101 : constant Version_32 := 16#f4e097a7#;
   pragma Export (C, u00101, "ada__text_ioB");
   u00102 : constant Version_32 := 16#3913d0d6#;
   pragma Export (C, u00102, "ada__text_ioS");
   u00103 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00103, "ada__streamsB");
   u00104 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00104, "ada__streamsS");
   u00105 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00105, "ada__io_exceptionsS");
   u00106 : constant Version_32 := 16#d398a95f#;
   pragma Export (C, u00106, "ada__tagsB");
   u00107 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00107, "ada__tagsS");
   u00108 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00108, "system__htableB");
   u00109 : constant Version_32 := 16#8c99dc11#;
   pragma Export (C, u00109, "system__htableS");
   u00110 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00110, "system__string_hashB");
   u00111 : constant Version_32 := 16#2ec7b76f#;
   pragma Export (C, u00111, "system__string_hashS");
   u00112 : constant Version_32 := 16#b8e72903#;
   pragma Export (C, u00112, "system__val_lluB");
   u00113 : constant Version_32 := 16#51139e9a#;
   pragma Export (C, u00113, "system__val_lluS");
   u00114 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00114, "system__val_utilB");
   u00115 : constant Version_32 := 16#a4fbd905#;
   pragma Export (C, u00115, "system__val_utilS");
   u00116 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00116, "system__case_utilB");
   u00117 : constant Version_32 := 16#378ed9af#;
   pragma Export (C, u00117, "system__case_utilS");
   u00118 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00118, "interfaces__c_streamsB");
   u00119 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00119, "interfaces__c_streamsS");
   u00120 : constant Version_32 := 16#ec083f01#;
   pragma Export (C, u00120, "system__file_ioB");
   u00121 : constant Version_32 := 16#af2a8e9e#;
   pragma Export (C, u00121, "system__file_ioS");
   u00122 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00122, "ada__finalizationS");
   u00123 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00123, "system__finalization_rootB");
   u00124 : constant Version_32 := 16#47a91c6b#;
   pragma Export (C, u00124, "system__finalization_rootS");
   u00125 : constant Version_32 := 16#e4774a28#;
   pragma Export (C, u00125, "system__os_libB");
   u00126 : constant Version_32 := 16#d8e681fb#;
   pragma Export (C, u00126, "system__os_libS");
   u00127 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00127, "system__stringsB");
   u00128 : constant Version_32 := 16#684d436e#;
   pragma Export (C, u00128, "system__stringsS");
   u00129 : constant Version_32 := 16#f5c4f553#;
   pragma Export (C, u00129, "system__file_control_blockS");
   u00130 : constant Version_32 := 16#96e582b6#;
   pragma Export (C, u00130, "production_workloadB");
   u00131 : constant Version_32 := 16#d22b1898#;
   pragma Export (C, u00131, "production_workloadS");
   u00132 : constant Version_32 := 16#cd2959fb#;
   pragma Export (C, u00132, "ada__numericsS");
   u00133 : constant Version_32 := 16#03e83d1c#;
   pragma Export (C, u00133, "ada__numerics__elementary_functionsB");
   u00134 : constant Version_32 := 16#edc89b7f#;
   pragma Export (C, u00134, "ada__numerics__elementary_functionsS");
   u00135 : constant Version_32 := 16#e5114ee9#;
   pragma Export (C, u00135, "ada__numerics__auxB");
   u00136 : constant Version_32 := 16#9f6e24ed#;
   pragma Export (C, u00136, "ada__numerics__auxS");
   u00137 : constant Version_32 := 16#0cccd408#;
   pragma Export (C, u00137, "system__fat_llfS");
   u00138 : constant Version_32 := 16#6533c8fa#;
   pragma Export (C, u00138, "system__machine_codeS");
   u00139 : constant Version_32 := 16#b2a569d2#;
   pragma Export (C, u00139, "system__exn_llfB");
   u00140 : constant Version_32 := 16#b425d427#;
   pragma Export (C, u00140, "system__exn_llfS");
   u00141 : constant Version_32 := 16#502e73ef#;
   pragma Export (C, u00141, "system__fat_fltS");
   u00142 : constant Version_32 := 16#3da42afa#;
   pragma Export (C, u00142, "activation_managerB");
   u00143 : constant Version_32 := 16#8a84be4d#;
   pragma Export (C, u00143, "activation_managerS");
   u00144 : constant Version_32 := 16#c18e469f#;
   pragma Export (C, u00144, "ada__real_time__delaysB");
   u00145 : constant Version_32 := 16#0a5c26d7#;
   pragma Export (C, u00145, "ada__real_time__delaysS");
   u00146 : constant Version_32 := 16#d10be6c8#;
   pragma Export (C, u00146, "ada__synchronous_task_controlB");
   u00147 : constant Version_32 := 16#241197a3#;
   pragma Export (C, u00147, "ada__synchronous_task_controlS");
   u00148 : constant Version_32 := 16#510c5068#;
   pragma Export (C, u00148, "ada__task_identificationB");
   u00149 : constant Version_32 := 16#c716434e#;
   pragma Export (C, u00149, "ada__task_identificationS");
   u00150 : constant Version_32 := 16#0b29e756#;
   pragma Export (C, u00150, "system__tasking__utilitiesB");
   u00151 : constant Version_32 := 16#0f670827#;
   pragma Export (C, u00151, "system__tasking__utilitiesS");
   u00152 : constant Version_32 := 16#0a1cacd7#;
   pragma Export (C, u00152, "system__tasking__initializationB");
   u00153 : constant Version_32 := 16#fc2303e6#;
   pragma Export (C, u00153, "system__tasking__initializationS");
   u00154 : constant Version_32 := 16#6213e14a#;
   pragma Export (C, u00154, "system__tasking__task_attributesB");
   u00155 : constant Version_32 := 16#e81a3c25#;
   pragma Export (C, u00155, "system__tasking__task_attributesS");
   u00156 : constant Version_32 := 16#2e4883f4#;
   pragma Export (C, u00156, "system__tasking__queuingB");
   u00157 : constant Version_32 := 16#6dba2805#;
   pragma Export (C, u00157, "system__tasking__queuingS");
   u00158 : constant Version_32 := 16#92cd7102#;
   pragma Export (C, u00158, "system__tasking__protected_objects__entriesB");
   u00159 : constant Version_32 := 16#7daf93e7#;
   pragma Export (C, u00159, "system__tasking__protected_objects__entriesS");
   u00160 : constant Version_32 := 16#100eaf58#;
   pragma Export (C, u00160, "system__restrictionsB");
   u00161 : constant Version_32 := 16#4329b6aa#;
   pragma Export (C, u00161, "system__restrictionsS");
   u00162 : constant Version_32 := 16#69297ce2#;
   pragma Export (C, u00162, "system__tasking__stagesB");
   u00163 : constant Version_32 := 16#bfc55b2f#;
   pragma Export (C, u00163, "system__tasking__stagesS");
   u00164 : constant Version_32 := 16#7382e823#;
   pragma Export (C, u00164, "system__tasking__rendezvousB");
   u00165 : constant Version_32 := 16#5618a4d0#;
   pragma Export (C, u00165, "system__tasking__rendezvousS");
   u00166 : constant Version_32 := 16#891dbac0#;
   pragma Export (C, u00166, "system__tasking__entry_callsB");
   u00167 : constant Version_32 := 16#6342024e#;
   pragma Export (C, u00167, "system__tasking__entry_callsS");
   u00168 : constant Version_32 := 16#6368532a#;
   pragma Export (C, u00168, "system__tasking__protected_objects__operationsB");
   u00169 : constant Version_32 := 16#ba36ad85#;
   pragma Export (C, u00169, "system__tasking__protected_objects__operationsS");
   u00170 : constant Version_32 := 16#a7ffcf69#;
   pragma Export (C, u00170, "event_queueB");
   u00171 : constant Version_32 := 16#15d6e62c#;
   pragma Export (C, u00171, "event_queueS");
   u00172 : constant Version_32 := 16#e514315e#;
   pragma Export (C, u00172, "external_event_server_parametersB");
   u00173 : constant Version_32 := 16#d28c6a7d#;
   pragma Export (C, u00173, "external_event_server_parametersS");
   u00174 : constant Version_32 := 16#19df5657#;
   pragma Export (C, u00174, "external_event_serverB");
   u00175 : constant Version_32 := 16#251f7cf2#;
   pragma Export (C, u00175, "external_event_serverS");
   u00176 : constant Version_32 := 16#5bc711f4#;
   pragma Export (C, u00176, "on_call_producerB");
   u00177 : constant Version_32 := 16#69402ece#;
   pragma Export (C, u00177, "on_call_producerS");
   u00178 : constant Version_32 := 16#d362b51c#;
   pragma Export (C, u00178, "on_call_producer_parametersB");
   u00179 : constant Version_32 := 16#db01204f#;
   pragma Export (C, u00179, "on_call_producer_parametersS");
   u00180 : constant Version_32 := 16#7531eaf6#;
   pragma Export (C, u00180, "request_bufferB");
   u00181 : constant Version_32 := 16#9d86f059#;
   pragma Export (C, u00181, "request_bufferS");
   u00182 : constant Version_32 := 16#2ebb56e5#;
   pragma Export (C, u00182, "request_buffer_parametersS");
   u00183 : constant Version_32 := 16#2ee12a00#;
   pragma Export (C, u00183, "regular_producerB");
   u00184 : constant Version_32 := 16#32fc83aa#;
   pragma Export (C, u00184, "regular_producerS");
   u00185 : constant Version_32 := 16#1ede97f6#;
   pragma Export (C, u00185, "regular_producer_parametersB");
   u00186 : constant Version_32 := 16#5e062f88#;
   pragma Export (C, u00186, "regular_producer_parametersS");
   u00187 : constant Version_32 := 16#3a6c640f#;
   pragma Export (C, u00187, "auxiliaryB");
   u00188 : constant Version_32 := 16#e63055a9#;
   pragma Export (C, u00188, "auxiliaryS");
   u00189 : constant Version_32 := 16#e31b7c4e#;
   pragma Export (C, u00189, "system__memoryB");
   u00190 : constant Version_32 := 16#512609cf#;
   pragma Export (C, u00190, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.memory%s
   --  system.memory%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  interfaces.c.extensions%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.fat_flt%s
   --  system.fat_llf%s
   --  ada.numerics.aux%s
   --  ada.numerics.aux%b
   --  ada.numerics.elementary_functions%s
   --  ada.numerics.elementary_functions%b
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.tasking%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_llu%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.real_time.delays%s
   --  ada.real_time.delays%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.tasking.initialization%s
   --  system.tasking.task_attributes%s
   --  system.tasking.task_attributes%b
   --  system.tasking.initialization%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%s
   --  system.tasking.utilities%b
   --  ada.task_identification%s
   --  ada.task_identification%b
   --  ada.synchronous_task_control%s
   --  ada.synchronous_task_control%b
   --  system.tasking.entry_calls%s
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.entry_calls%b
   --  system.tasking.rendezvous%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  activation_log_parameters%s
   --  activation_log%s
   --  activation_log%b
   --  activation_manager%s
   --  activation_manager%b
   --  auxiliary%s
   --  auxiliary%b
   --  external_event_server_parameters%s
   --  external_event_server_parameters%b
   --  event_queue%s
   --  event_queue%b
   --  external_event_server%s
   --  external_event_server%b
   --  production_workload%s
   --  production_workload%b
   --  activation_log_reader_parameters%s
   --  activation_log_reader_parameters%b
   --  activation_log_reader%s
   --  activation_log_reader%b
   --  on_call_producer_parameters%s
   --  on_call_producer_parameters%b
   --  request_buffer_parameters%s
   --  request_buffer%s
   --  request_buffer%b
   --  on_call_producer%s
   --  on_call_producer%b
   --  regular_producer_parameters%s
   --  regular_producer_parameters%b
   --  regular_producer%s
   --  regular_producer%b
   --  gee%b
   --  END ELABORATION ORDER

end ada_main;
