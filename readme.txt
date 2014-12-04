--------------------------------------------------------------------------------
                               Telemetry Library
--------------------------------------------------------------------------------

Description
----------------------------------------
Telemetry library is a rather simple framework that is intended to ease use of 
telemetry API provided by SCS Software games (at the moment only by Euro Truck 
Simulator 2). It started only as simple translation of SDK header files into 
pascal, but has grown significantly since then. It contains set of functions and
classes which encapsulates calls to the API and many more. You should refer to 
source files and documentation for details about individual functions and 
classes and their use.
Currently, it is developed in Delphi 7 Personal, so it can be compiled in it
and possibly in newer versions of Delphi as well. FreePascal/Lazarus is not 
supported and probably will not compile it (but support for these compilers is
planned).
A word of warning - do not compile this library into 64bit application/dll. 
64bit mode is not fully supported and probably will not work. Adding 64bit 
compatibility is planned in longer horizon.



Content
----------------------------------------
This section describes nature of content of individual folders in this project.

./

  Root folder of the project, contains licenses and readme file.

./Original source

  Original source files (headers and examples) of the SDK provided by 
  SCS Software. They can also be downloaded from this page 
  http://www.eurotrucksimulator2.com/mod_tools.php. They are provided alongside
  this project as a reference for translations and debugging.

./Telemetry API Headers

  Header files (originally *.h files) translated to pascal.

./Headers Tester

  Simple application used for basic check of translated headers (syntax checks).

./Condensed API Headers

  Contains file that is created by merging all translated header files into one 
  conglomerate. This file is there to simplify units dependency - you can use 
  only one unit in uses clause instead of several individual units.

./Condenser

  Application used to merge translated header files into one file.

./Condensed Tester

  Simple program used for basic control of condensed (merged) headers.

./Telemetry Library/Source

  All source files for Telemetry Library. All files directly in this folder are
  considered as "core" units of the Telemetry Library, but there are also 
  several subdirectories containing individual specialized parts of the library,
  namely:

  ./Telemetry Library/Source/Libs
      Other libraries that are used inside the Telemetry Library, for example 
      CRC32 calculations.

  ./Telemetry Library/Source/Log
      Units containing classes used for logging of telemetry and API actions
      (textual and binary loggers).

  ./Telemetry Library/Source/SCS
      Examples provided by SCS Software along with the SDK translated to pascal.
      They are not exact translations, rather pascal reimplementation designed 
      to imitate behavior of originals.

  ./Telemetry Library/Source/Comm
      Communication part, currently under development.

./Telemetry Library/Tester

  Application used for testing and debugging of Telemetry Library. Actual code
  of this app changes very often, according to what part of the library needs
  testing or is under development.

./Telemetry Library/Examples

  Example programs and plugins showing how to use Telemetry Library and its 
  parts. Currently, following examples are included:

  ./Telemetry Library/Examples/TextLogger
      Plugin implementing text logger (output is plaintext file).

  ./Telemetry Library/Examples/BinaryLogger
      Plugin implementing binary logger (output is binary file of special 
      format - you can find complete specification of this format in 
      documentation).

  ./Telemetry Library/Examples/LogConverter
      Small program implementing binary log to text log converter. You can use 
      this program to convert binary logs to human-readable form.

./Telemetry Library/SCS Examples

  Plugins implementing translated examples provided with the SDK. Folders with 
  individual plugins are named the same as folders with corresponding examples 
  in SDK. 
  There is also a simple program that is not part of SDK (located in 
  subdirectory telemetry_mem_reader) - it shows a possible way how to utilize 
  telemetry_mem example. It should work both with translated and original 
  telemetry_mem example plugin.  

./Telemetry Library/AutoDocumentation

  Contains utilities (or link where you can get them) and other materials used 
  for automatic generation of documentation.

./Telemetry Library/Documentation

  Automatically generated (using PasDoc) documentation of Telemetry Library in 
  form of HTML pages. Note that this documentation is far from complete, but it
  covers important parts.



Licensing
----------------------------------------
Everything (source codes, executables/binaries, configurations, etc.), with few 
exceptions mentioned below, is licensed under Mozilla Public License Version 
2.0. You can find full text of this license in file mpl_license.txt or on web 
page https://www.mozilla.org/MPL/2.0/.
Exception being following folders and their entire content:

./Original source  

  This folder contains original SDK sources which have their own license. See 
  individual subdirectories for information about actual license.

./Telemetry Library/Documentation  

  This folder contains documentations for Telemetry Library in form of HTML 
  pages. Everything in this folder is licensed under the terms of Creative 
  Commons Attribution-ShareAlike 4.0 (CC BY-SA 4.0) license. You can find full 
  legal code in file CC_BY-SA_4.0.txt or on web page 
  http://creativecommons.org/licenses/by-sa/4.0/legalcode. Short wersion is 
  available on web page http://creativecommons.org/licenses/by-sa/4.0/

./Telemetry Library/AutoDocumentation/Docs 

  Content of this folder is licensed under Creative Commons 
  Attribution-ShareAlike 4.0 (CC BY-SA 4.0) license (see above for details).

FastMM
  Some parts of this project are using FastMM as memory manager. For information
  about FastMM license, please refer to its source files.



Repositories
----------------------------------------
You can get actual copies of Telemetry Library on these git repositories:

https://bitbucket.org/ncs-sniper/telemetrylibrary
https://github.com/ncs-sniper/TelemetryLibrary



Authors, contacts
----------------------------------------
František Milt, frantisek.milt@gmail.com



Copyright
----------------------------------------
©2013-2014 František Milt, all rights reserved