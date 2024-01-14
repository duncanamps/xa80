# xa80 V1.0(DEV)

xa80 - X-Assembler for x80 processors

![xa80_short](https://github.com/duncanamps/xa80/assets/6016794/5a307bc0-b6b6-458f-91a2-7ae34e710088)

This is V1.0 (Development) which is in development, and therefore incomplete. For the latest stable release, please see [Release Package V0.3.1](https://github.com/duncanamps/xa80/releases/tag/v0.3.1).

#### Synopsis

xa80 is a command line tool that allows the cross assembly of source files aimed at x80 processors (8080,8085,Z80,Z180). It takes an input file (e.g. myfile.z80 or test.asm) and creates the following output files, some of which are optional:

* .hex file containing output information in the industry standard Intel .hex format
* .lst file containing a listing of the assembler output
* .log file containing errors encountered during the assembly
* .map file containing the symbol information
* .com file containing the actual machine code which can be executed on, for example, CP/M machines
* .obj80 file containing object code (fixed/relocatable segments, import/export info, fixup tables)

#### Key features

Here are some of the key features of xa80:

* Open source
* Two pass assembler
* Supports mnemonics from different processors (8080, 8085, Z80, Z180) as a baked in standard
* Ability to add additional opcode maps as external files
* Opcode compiler so you can add your own secret/hidden instructions and extend to other processor variants in the "family"
* Macro capability with nested expansion of macros allowed
* Conditional assembly with IF / IFDEF / IFNDEF statements
* Repetition through REPEAT and WHILE statements
* Full expression evaluator with many functions and string handling capability
* Segmented model with fixed and relocatable segments
* Rich set of command line parameters
* xa80 Environment variable for commonly used parameters
* Runs on any hardware supported by Lazarus/FPC (Windows, macOS, Linux, etc. etc.)
* Fast - will assemble the CP/M BDOS22.ASM (3,289 lines) and CCP22.ASM files (1,325 lines) with map file and listing outputs (total 105 pages) in approx 0.15 seconds using a Core i7 laptop, Acer Aspire 5 A515-56

#### Development Status

This is very much experimental and was developed by the author as a learning tool for how assemblers, lexical analysers and parsers work in general.
Please don't use this for anything serious that you would object to losing. Whilst having been extensively tested, and coming with 
working examples, there is no guarantee that it will work correctly with all input files.

#### Development Environment

To modify and compile this software, you will need [Lazarus](https://www.lazarus-ide.org/index.php?page=downloads) 2.1.0 or later. It has been
tested on Windows and Linux. As it is only a simple text and file based application, it should be relatively easy to recompile on other hosts
which are supported by the Lazarus ecosystem in 32 and 64 bit flavours, including:

* Android
* FreeBSD
* iOS
* Linux
* macOS
* Raspberry Pi
* WinCE
* Windows

**Tip**: For most people, it won't be necessary to alter or recompile the software. Just use the pre-compiled binaries available from this repository if they are
sufficient for your needs. All you will need is **xa80** or **xa80.exe** depending on your operating system. There are packages available containing the binary and manuals.

#### Dependencies

To modify the grammar for the opcode compiler, or xa80 itself, will require the use of a tool called LaCoGen (Lazarus Compiler Generator).
LaCoGen is [available from this GitHub](https://github.com/duncanamps/lacogen1). The grammar to deal with operands is contained in the .lac file and for the
most part can be left alone. It's only if you want to add new functions or operators that you would need to get involved in changing
the grammar file and recompiling with LaCoGen; 99.9% of people won't need to look at this.

#### Documentation

The [docs/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/docs) folder contains a user guide explaining how the assembler is used, and also the opcode compiler if you want to get into the detail.

#### Folder Structure

Folders are organised as follows:

* [root](https://github.com/duncanamps/xa80/tree/main) the Lazarus project files, licence and .gitignore
  * [binaries/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/docs) - Precompiled binaries for various systems  	
  * [docs/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/docs) - Documentation (user manual, technical notes)
  * [lac/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/lac) - The LaCoGen operand grammar for xa80. The xa80oper.lac file is compiled into xa80oper.lacobj which is loaded into the assembler as a resource file. If you don't need to change the basic grammar for operands, then this can be left alone
  * [lexer_parser/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/lexer_parser) - A lightweight lexical analyser which is used to split or pre-parse the input into labels, commands, instructions, operands and comments
  * [opcodes/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/opcodes) - The folder containing the opcode compiler oc_comp (see readme.txt in the folder)
    * [opcodes/lac/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/opcodes/lac) - Grammar for the opcode compiler, opcode_compiler.lac compiles info opcode_compiler.lacobj
	* [opcodes/source/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/opcodes/source) - The source files describing the different combinations of instructions and operands
  * [test_files/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/test_files) - A set of test files to check that things work, and also includes some deliberate fails to check the assembler response to warning and error conditions
  * [units/](https://github.com/duncanamps/xa80/tree/V1.0.DEV/units) - The bulk of the source code resides in here

#### Known Issues 

* No major issues identified at this time

#### Development roadmap

* V0.3.2 - **Current Stable Release**
* V1.0   - **Current Development**: Introduce segmented architecture, object files, debug info generation
* V2.0   - FUTURE: Introduce 24 bit capability

#### Author

Duncan Munro  <duncan@duncanamps.com>
