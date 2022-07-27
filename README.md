# XA80
XA80 - X-Assembler for x80 processors
WARNING - THIS IS UNRELEASED AND VERY MUCH WORK IN PROGRESS

#### Synopsis
XA80 is a command line tool that allows the cross assembly of source files aimed at x80 processors (8080,8085,Z80,Z180). It takes an input file (e.g. myfile.z80 or test.asm) and creates the following output files, some of which are optional:

* .d80 file containing debug information such as symbols, source lines, etc. (NOT IMPLEMENTED)
* .hex file containing output information in .hex format which is human readable
* .lst file containing a listing of the assembler output
* .log file containing errors encountered during the assembly
* .map file containing the symbol information
* .o80 file which creates the object code and symbol information suitable for use with a linker (NOT IMPLEMENTED)

#### Development Status
This is very much experimental and was developed by the author as a learning tool for how assemblers work in general.
Please don't use this for anything serious that you would object to losing. Whilst having been extensively tested, and coming with 
working examples, there is no guarantee that it will work correctly with all input files.

#### Development Environment
To compile this software, you will need [Lazarus](https://www.lazarus-ide.org/index.php?page=downloads) 2.10 or later. It has been tested on Windows. As it is
only a simple text and file based application, it should be relatively easy to recompile on other hosts which are
supported by the Lazarus ecosystem, for example macOS, Linux, Raspberry Pi, etc.

#### Dependencies
To modify the grammar for the opcode compiler, or XA80 itself, will require the use of a tool called LaCoGen (Lazarus Compiler Generator).
LaCoGen is [available from this GitHub](https://github.com/duncanamps/lacogen1).

#### Documentation
The [docs/](https://github.com/duncanamps/xa80/tree/main/docs) folder contains a user guide and a technical document explaining how some of the internal features work.

#### Folder Structure
Folders are organised as follows:

* [root](https://github.com/duncanamps/xa80/tream/main) the Lazarus project files, licence and .gitignore
  * [docs/](https://github.com/duncanamps/xa80/tree/main/docs) - Documentation (user manual, technical notes)
  * [lac/](https://github.com/duncanamps/xa80/tree/main/lac) - The LaCoGen grammar for XA80. The xa80.lac file is compiled into xa80.lacobj
  * [opcodes/](https://github.com/duncanamps/xa80/tree/main/opcodes) - The folder containing the opcode compiler oc_comp (see readme.txt)
    * [opcodes/lac/](https://github.com/duncanamps/xa80/tree/main/opcodes/lac) - Grammar for the opcode compiler, opcode_compiler.lac compiles info opcode_compiler.lacobj
	* [opcodes/source/](https://github.com/duncanamps/xa80/tree/main/opcodes/source) - The source files describing the different combinations of instructions and operands
  * [test_files/](https://github.com/duncanamps/xa80/tree/main/test_files) - A set of test files to check that things work, and also includes some deliberate fails to check the assembler response
  * [units/](https://github.com/duncanamps/xa80/tree/main/units) - The bulk of the source code resides in here

#### Known Issues 
* The software is anything but complete and doesn't correspond with the user manual
* Only 8080 and Z80 are being catered for at this time

#### Author
Duncan Munro
