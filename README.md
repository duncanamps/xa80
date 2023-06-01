# XA80

XA80 - X-Assembler for x80 processors
WARNING - THIS IS UNRELEASED AND VERY MUCH WORK IN PROGRESS

#### Synopsis

XA80 is a command line tool that allows the cross assembly of source files aimed at x80 processors (8080,8085,Z80,Z180). It takes an input file (e.g. myfile.z80 or test.asm) and creates the following output files, some of which are optional:

* .dbg80 file containing debug information such as symbols, source lines, etc. (NOT IMPLEMENTED)
* .hex   file containing output information in .hex format which is human readable
* .lst   file containing a listing of the assembler output
* .log   file containing errors encountered during the assembly
* .map   file containing the symbol information
* .obj80 file which creates the object code and symbol information suitable for use with a linker (NOT IMPLEMENTED)

#### Key features

Here are some of the key features of XA80:

* Two pass assembler with multi-processor and mnemonic handling (8080, 8085, Z80, Z180)
  * Opcode compiler so you can add your own secret/hidden instructions and extend to other processor variants in the "family"
* Multiple grammar options to allow different assembler styles (under development)
  * Also customisable so you can, within reason, add or amend grammars to simulate many assemblers
  * Macro capability with nesting of macros allowed
  * Conditional assembly with IF / IFDEF / IFNDEF statements, depending on grammar
  * Full expression evaluator with definable precedence
* Rich set of command line parameters
* Environment variable for commonly used parameters
* Runs on any hardware supported by Lazarus/FPC (Windows, macOS, Linux, etc. etc.)

#### Development Status

This is very much experimental and was developed by the author as a learning tool for how assemblers work in general.
Please don't use this for anything serious that you would object to losing. Whilst having been extensively tested, and coming with 
working examples, there is no guarantee that it will work correctly with all input files.

#### Development Environment

To modify and compile this software, you will need [Lazarus](https://www.lazarus-ide.org/index.php?page=downloads) 2.10 or later. It has been tested on Windows. As it is
only a simple text and file based application, it should be relatively easy to recompile on other hosts which are
supported by the Lazarus ecosystem in 32 bit flavours and above, including:

* Linux
* macOS
* Raspberry Pi
* WinCE
* Windows

You don't have to download Lazarus and recompile, just use the binaries if they are sufficient for your needs.

#### Dependencies

To modify the grammar for the opcode compiler, or XA80 itself, will require the use of a tool called LaCoGen (Lazarus Compiler Generator).
LaCoGen is [available from this GitHub](https://github.com/duncanamps/lacogen1). The grammar is contained in the .lac files and for the
most part can be left alone. It's only if you want to invoke new directives or new opcodes that you would need to get involved in changing
the grammar files and recompiling with LaCoGen.
***NOTE***: *The dependency on LaCoGen is being removed as XA80 moves towards hand-written lexers and parsers for flexibility and performance*.

#### Documentation

The [docs/](https://github.com/duncanamps/xa80/tree/main/docs) folder contains a user guide and a technical document explaining how some of the internal features work.

#### Folder Structure

Folders are organised as follows:

* [root](https://github.com/duncanamps/xa80/tream/main) the Lazarus project files, licence and .gitignore
  * [docs/](https://github.com/duncanamps/xa80/tree/main/docs) - Documentation (user manual, technical notes)
  * [lac/](https://github.com/duncanamps/xa80/tree/main/lac) - The LaCoGen grammar for XA80. The xa80.lac file is compiled into xa80.lacobj
  * [opcodes/](https://github.com/duncanamps/xa80/tree/main/opcodes) - The folder containing the opcode compiler oc_comp (see readme.txt in the folder)
    * [opcodes/lac/](https://github.com/duncanamps/xa80/tree/main/opcodes/lac) - Grammar for the opcode compiler, opcode_compiler.lac compiles info opcode_compiler.lacobj
	* [opcodes/source/](https://github.com/duncanamps/xa80/tree/main/opcodes/source) - The source files describing the different combinations of instructions and operands
  * [test_files/](https://github.com/duncanamps/xa80/tree/main/test_files) - A set of test files to check that things work, and also includes some deliberate fails to check the assembler response
  * [units/](https://github.com/duncanamps/xa80/tree/main/units) - The bulk of the source code resides in here

#### Known Issues 

* The software is anything but complete and doesn't correspond with the user manual
* Only 8080 and Z80 are being catered for at this time but hoping to include 8085 and Z180 soon
* Object files and debug files are not currently being generated, however .hex and .com files can be generated

#### Author

Duncan Munro  <duncan@duncanamps.com>
