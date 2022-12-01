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
                    "GNAT Version: 12.1.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_day1" & ASCII.NUL;
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
   u00001 : constant Version_32 := 16#6706bfd5#;
   pragma Export (C, u00001, "day1B");
   u00002 : constant Version_32 := 16#7320ff5f#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#4c09e7c8#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#a94883d4#;
   pragma Export (C, u00005, "ada__strings__text_buffersB");
   u00006 : constant Version_32 := 16#bb49bb93#;
   pragma Export (C, u00006, "ada__strings__text_buffersS");
   u00007 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00007, "ada__stringsS");
   u00008 : constant Version_32 := 16#5e074051#;
   pragma Export (C, u00008, "systemS");
   u00009 : constant Version_32 := 16#c71e6c8a#;
   pragma Export (C, u00009, "system__exception_tableB");
   u00010 : constant Version_32 := 16#d32c3648#;
   pragma Export (C, u00010, "system__exception_tableS");
   u00011 : constant Version_32 := 16#fd5f5f4c#;
   pragma Export (C, u00011, "system__soft_linksB");
   u00012 : constant Version_32 := 16#16d53d45#;
   pragma Export (C, u00012, "system__soft_linksS");
   u00013 : constant Version_32 := 16#e12f1eb0#;
   pragma Export (C, u00013, "system__secondary_stackB");
   u00014 : constant Version_32 := 16#85d06901#;
   pragma Export (C, u00014, "system__secondary_stackS");
   u00015 : constant Version_32 := 16#8757dfc5#;
   pragma Export (C, u00015, "ada__exceptionsB");
   u00016 : constant Version_32 := 16#021fcb5c#;
   pragma Export (C, u00016, "ada__exceptionsS");
   u00017 : constant Version_32 := 16#0740df23#;
   pragma Export (C, u00017, "ada__exceptions__last_chance_handlerB");
   u00018 : constant Version_32 := 16#6dc27684#;
   pragma Export (C, u00018, "ada__exceptions__last_chance_handlerS");
   u00019 : constant Version_32 := 16#6ca2ff63#;
   pragma Export (C, u00019, "system__exceptionsS");
   u00020 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00020, "system__exceptions__machineB");
   u00021 : constant Version_32 := 16#8bdfdbe3#;
   pragma Export (C, u00021, "system__exceptions__machineS");
   u00022 : constant Version_32 := 16#7706238d#;
   pragma Export (C, u00022, "system__exceptions_debugB");
   u00023 : constant Version_32 := 16#6e091802#;
   pragma Export (C, u00023, "system__exceptions_debugS");
   u00024 : constant Version_32 := 16#ee8e331a#;
   pragma Export (C, u00024, "system__img_intS");
   u00025 : constant Version_32 := 16#ec4fa52d#;
   pragma Export (C, u00025, "system__storage_elementsB");
   u00026 : constant Version_32 := 16#33895fa5#;
   pragma Export (C, u00026, "system__storage_elementsS");
   u00027 : constant Version_32 := 16#5c7d9c20#;
   pragma Export (C, u00027, "system__tracebackB");
   u00028 : constant Version_32 := 16#d89db4ec#;
   pragma Export (C, u00028, "system__tracebackS");
   u00029 : constant Version_32 := 16#5f6b6486#;
   pragma Export (C, u00029, "system__traceback_entriesB");
   u00030 : constant Version_32 := 16#961bffdd#;
   pragma Export (C, u00030, "system__traceback_entriesS");
   u00031 : constant Version_32 := 16#a0281f47#;
   pragma Export (C, u00031, "system__traceback__symbolicB");
   u00032 : constant Version_32 := 16#d9e66ad1#;
   pragma Export (C, u00032, "system__traceback__symbolicS");
   u00033 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00033, "ada__exceptions__tracebackB");
   u00034 : constant Version_32 := 16#eb07882c#;
   pragma Export (C, u00034, "ada__exceptions__tracebackS");
   u00035 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00035, "system__address_imageB");
   u00036 : constant Version_32 := 16#ffebdd6b#;
   pragma Export (C, u00036, "system__address_imageS");
   u00037 : constant Version_32 := 16#fd158a37#;
   pragma Export (C, u00037, "system__wch_conB");
   u00038 : constant Version_32 := 16#87046332#;
   pragma Export (C, u00038, "system__wch_conS");
   u00039 : constant Version_32 := 16#5c289972#;
   pragma Export (C, u00039, "system__wch_stwB");
   u00040 : constant Version_32 := 16#aa154f33#;
   pragma Export (C, u00040, "system__wch_stwS");
   u00041 : constant Version_32 := 16#002bec7b#;
   pragma Export (C, u00041, "system__wch_cnvB");
   u00042 : constant Version_32 := 16#81c4a942#;
   pragma Export (C, u00042, "system__wch_cnvS");
   u00043 : constant Version_32 := 16#edec285f#;
   pragma Export (C, u00043, "interfacesS");
   u00044 : constant Version_32 := 16#e538de43#;
   pragma Export (C, u00044, "system__wch_jisB");
   u00045 : constant Version_32 := 16#3473cb68#;
   pragma Export (C, u00045, "system__wch_jisS");
   u00046 : constant Version_32 := 16#821dff88#;
   pragma Export (C, u00046, "system__parametersB");
   u00047 : constant Version_32 := 16#8a93e4f7#;
   pragma Export (C, u00047, "system__parametersS");
   u00048 : constant Version_32 := 16#37c92568#;
   pragma Export (C, u00048, "system__soft_links__initializeB");
   u00049 : constant Version_32 := 16#2ed17187#;
   pragma Export (C, u00049, "system__soft_links__initializeS");
   u00050 : constant Version_32 := 16#8599b27b#;
   pragma Export (C, u00050, "system__stack_checkingB");
   u00051 : constant Version_32 := 16#38089e5b#;
   pragma Export (C, u00051, "system__stack_checkingS");
   u00052 : constant Version_32 := 16#7e7d940a#;
   pragma Export (C, u00052, "ada__strings__utf_encodingB");
   u00053 : constant Version_32 := 16#84aa91b0#;
   pragma Export (C, u00053, "ada__strings__utf_encodingS");
   u00054 : constant Version_32 := 16#d1d1ed0b#;
   pragma Export (C, u00054, "ada__strings__utf_encoding__wide_stringsB");
   u00055 : constant Version_32 := 16#a373d741#;
   pragma Export (C, u00055, "ada__strings__utf_encoding__wide_stringsS");
   u00056 : constant Version_32 := 16#c2b98963#;
   pragma Export (C, u00056, "ada__strings__utf_encoding__wide_wide_stringsB");
   u00057 : constant Version_32 := 16#22a4a396#;
   pragma Export (C, u00057, "ada__strings__utf_encoding__wide_wide_stringsS");
   u00058 : constant Version_32 := 16#c3fbe91b#;
   pragma Export (C, u00058, "ada__tagsB");
   u00059 : constant Version_32 := 16#8bc79dfc#;
   pragma Export (C, u00059, "ada__tagsS");
   u00060 : constant Version_32 := 16#3548d972#;
   pragma Export (C, u00060, "system__htableB");
   u00061 : constant Version_32 := 16#dfde18ba#;
   pragma Export (C, u00061, "system__htableS");
   u00062 : constant Version_32 := 16#1f1abe38#;
   pragma Export (C, u00062, "system__string_hashB");
   u00063 : constant Version_32 := 16#789b98c5#;
   pragma Export (C, u00063, "system__string_hashS");
   u00064 : constant Version_32 := 16#a42d3f08#;
   pragma Export (C, u00064, "system__unsigned_typesS");
   u00065 : constant Version_32 := 16#856c5db1#;
   pragma Export (C, u00065, "system__val_lluS");
   u00066 : constant Version_32 := 16#273bd629#;
   pragma Export (C, u00066, "system__val_utilB");
   u00067 : constant Version_32 := 16#dc19d6f7#;
   pragma Export (C, u00067, "system__val_utilS");
   u00068 : constant Version_32 := 16#b98923bf#;
   pragma Export (C, u00068, "system__case_utilB");
   u00069 : constant Version_32 := 16#91149704#;
   pragma Export (C, u00069, "system__case_utilS");
   u00070 : constant Version_32 := 16#e56aa583#;
   pragma Export (C, u00070, "ada__text_ioB");
   u00071 : constant Version_32 := 16#a21a351b#;
   pragma Export (C, u00071, "ada__text_ioS");
   u00072 : constant Version_32 := 16#b4f41810#;
   pragma Export (C, u00072, "ada__streamsB");
   u00073 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00073, "ada__streamsS");
   u00074 : constant Version_32 := 16#367911c4#;
   pragma Export (C, u00074, "ada__io_exceptionsS");
   u00075 : constant Version_32 := 16#5fc04ee2#;
   pragma Export (C, u00075, "system__put_imagesB");
   u00076 : constant Version_32 := 16#f204fbed#;
   pragma Export (C, u00076, "system__put_imagesS");
   u00077 : constant Version_32 := 16#22b9eb9f#;
   pragma Export (C, u00077, "ada__strings__text_buffers__utilsB");
   u00078 : constant Version_32 := 16#608bd105#;
   pragma Export (C, u00078, "ada__strings__text_buffers__utilsS");
   u00079 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00079, "interfaces__c_streamsB");
   u00080 : constant Version_32 := 16#82d73129#;
   pragma Export (C, u00080, "interfaces__c_streamsS");
   u00081 : constant Version_32 := 16#0095760f#;
   pragma Export (C, u00081, "system__crtlS");
   u00082 : constant Version_32 := 16#1aa716c1#;
   pragma Export (C, u00082, "system__file_ioB");
   u00083 : constant Version_32 := 16#22a58504#;
   pragma Export (C, u00083, "system__file_ioS");
   u00084 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00084, "ada__finalizationS");
   u00085 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00085, "system__finalization_rootB");
   u00086 : constant Version_32 := 16#11f533c1#;
   pragma Export (C, u00086, "system__finalization_rootS");
   u00087 : constant Version_32 := 16#3d77d417#;
   pragma Export (C, u00087, "system__os_libB");
   u00088 : constant Version_32 := 16#a46b900e#;
   pragma Export (C, u00088, "system__os_libS");
   u00089 : constant Version_32 := 16#6e5d049a#;
   pragma Export (C, u00089, "system__atomic_operations__test_and_setB");
   u00090 : constant Version_32 := 16#57acee8e#;
   pragma Export (C, u00090, "system__atomic_operations__test_and_setS");
   u00091 : constant Version_32 := 16#99643a74#;
   pragma Export (C, u00091, "system__atomic_operationsS");
   u00092 : constant Version_32 := 16#29cc6115#;
   pragma Export (C, u00092, "system__atomic_primitivesB");
   u00093 : constant Version_32 := 16#6790db1b#;
   pragma Export (C, u00093, "system__atomic_primitivesS");
   u00094 : constant Version_32 := 16#7f1e3740#;
   pragma Export (C, u00094, "interfaces__cB");
   u00095 : constant Version_32 := 16#1bfc3385#;
   pragma Export (C, u00095, "interfaces__cS");
   u00096 : constant Version_32 := 16#256dbbe5#;
   pragma Export (C, u00096, "system__stringsB");
   u00097 : constant Version_32 := 16#c5854049#;
   pragma Export (C, u00097, "system__stringsS");
   u00098 : constant Version_32 := 16#fcf6b740#;
   pragma Export (C, u00098, "system__file_control_blockS");
   u00099 : constant Version_32 := 16#1982dcd0#;
   pragma Export (C, u00099, "system__memoryB");
   u00100 : constant Version_32 := 16#05837281#;
   pragma Export (C, u00100, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.atomic_operations%s
   --  system.img_int%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
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
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.utf_encoding%s
   --  ada.strings.utf_encoding%b
   --  ada.strings.utf_encoding.wide_strings%s
   --  ada.strings.utf_encoding.wide_strings%b
   --  ada.strings.utf_encoding.wide_wide_strings%s
   --  ada.strings.utf_encoding.wide_wide_strings%b
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.atomic_primitives%s
   --  system.atomic_primitives%b
   --  system.atomic_operations.test_and_set%s
   --  system.atomic_operations.test_and_set%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  ada.tags%s
   --  ada.tags%b
   --  ada.strings.text_buffers%s
   --  ada.strings.text_buffers%b
   --  ada.strings.text_buffers.utils%s
   --  ada.strings.text_buffers.utils%b
   --  system.put_images%s
   --  system.put_images%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  day1%b
   --  END ELABORATION ORDER

end ada_main;
