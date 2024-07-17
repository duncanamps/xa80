# xa80 LaCoGen Files

A group of files containing the LaCoGen (Lazarus Compiler Generator) files for the xa80 cross assembler.

LaCoGen is a tool in the style of the UNIX utilities, LEX and YACC in that it creates the means to instance a lexical analyser and a LALR(1) parser. These form the core of the xa80 assembler and deal with a number of important functions:

* Keyword and operator recognition (lexical analysis)
* Directive syntax
* Operand syntax
* Operator precedence and equation evaluation (i.e. with <code>1 + 2 * 3</code> it needs to know that <code>2 * 3</code> gets worked out first)

You can visit the open-source LaCoGen project at <a href="https://github.com/duncanamps/lacogen1">https://github.com/duncanamps/lacogen1</a>

The files in this folder are:

<table>
  <tr><td><b>File</b></td><td><b>Purpose</b></td></tr>
  <tr><td valign="top">laco.bat</td><td valign="top">MS-DOS batch file to invoke the LaCoGen compiler to create the output files</code> </td></tr>
  <tr><td valign="top">lacogen_module.pas</td><td valign="top">The main code module for the LaCoGen wrapper. This contains the lexical analyser and parser that are used by xa80.</td></tr>
  <tr><td valign="top">lacogen_types.pas</td><td valign="top">An auxilliary file which contains the definition of a number of types and structures used by xa80 and <code>lacogen_module.pas</code>.</td></tr>
  <tr><td valign="top">xa80oper.lac</td><td valign="top">The main LaCoGen file. This contains the lexical definitions and grammar definitions that form the basis for the operation of the assembler. It's unlikely you would want to change this unless you wish to add extra features or functionality to the assembler.</td></tr>
  <tr><td valign="top">xa80oper.lacobj</td><td valign="top">The LaCoGen object file created by using LaCoGen to compile the lexical definitions and grammar in <code>xa80oper.lac</code>. If any changes are made to xa80oper.lac, the compilation process will need to be re-run as it's ultimately the .lacobj file that is used by the software.</td></tr>
</table>


#### Author

Duncan Munro  <duncan@duncanamps.com>
