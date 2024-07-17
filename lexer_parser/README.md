# xa80 lexer_parser Files

A group of files containing a lightweight lexical analyser that is used by xa80 to do the initial chunking of the input line. The input line is split into labels, comments, operands, outer parenthesis, etc.

This is necessary as it's not easy for traditional parser grammar to handle the potential ambiguity of outer parenthesis which are used for three things: 1. to indicate indirection for some Z80 operands, 2. a grouping operator for equations, or 3. enclosure of the parameters of a function.

For example:

* <code>COUNT EQU 5*(3+7)</code> uses the parenthesis as a grouping operator to indicate precedence
* <code> LD A,(HL)</code> uses parenthesis as an indirection to indicate the address pointed to by HL
* <code>ASC('A')-10</code> uses the parenthesis to define function parameters
* <code>JP (5+2*9)</code> is ambiguous as under the xa80 grammar it could be a grouping operator or an indirection. For info, this ends up being treated as indirection as the parenthesis are outside the rest of the equation

The main activities are:

* Create a NFA (Non-deterministic Finite Automata) from a set of rules. Builds up the NFA tree
* Create a DFA (Deterministic Finite Automata) from the NFA
* Use the DFA to execute the lexical analyser to break the input line into chunks as described at the top of the page

The files in this folder are:

<table>
  <tr><td><b>File</b></td><td><b>Purpose</b></td></tr>
  <tr><td valign="top">units/udmdfa.pas</td><td valign="top">Contains the routines to turn a NFA created by <code>udmnfa.pas</code> into a DFA</td></tr>
  <tr><td valign="top">units/udmnfa.pas</td><td valign="top">Contains the routines to construct a NFA from scratch.</td></tr>
  <tr><td valign="top">units/udmset.pas</td><td valign="top">Some set definition and arithmetic routines.</td></tr>
  <tr><td valign="top">units/udmyala.pas</td><td valign="top">Contains a lexical analyser using the DFA created by <code>udmdfa.pas</code>.</td></tr>
</table>


#### Author

Duncan Munro  <duncan@duncanamps.com>
