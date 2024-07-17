# xa80 Calibration Files

A group of calibration files for the xa80 cross assembler. They fall into the following categories:

<table>
  <tr><td><b>Group</b></td><td><b>Contents</b></td></tr>
  <tr><td valign="top">Source&nbsp;files</td><td valign="top">These are the files that are used as the basis for the main assembler tests. They will have the template <code>test*.asm</code> for example <code>test_z80.asm</code> is used to test the assembler with <code>--processor=z80</code></td></tr>
  <tr><td valign="top">Output&nbsp;files</td><td valign="top">The files that the assembler creates from the source files. For example, <code>test_z80.asm</code> will yield <code>test_z80.lst</code> and so on. It is expected that these files will create predictable and consistent content.</td></tr>
  <tr><td valign="top">Calibration&nbsp;files</td><td valign="top">These have the template <code>calibration*.*</code> are reference copies of the output files. It is expected that the output files will be a byte for byte match for these - any discrepancies should be investigated.</td></tr>
  <tr><td valign="top">Error&files</td><td valign="top">Error files follow the template <code>ezzzz*.asm</code> where the letter 'e' is followed by four digits, for example <code>e2004_expected_number.asm</code>. The purpose of the file is to deliberately force a terminal error message by testing the assembler response to an inducing condition.</td></tr>
  <tr><td valign="top">Warning&nbsp;files</td><td valign="top">Warning files follow the template <code>wzzzz*.asm</code> where the letter 'w' is followed by four digits, for example <code>w1003_label_redefined.asm</code>. The purpose of the file is to deliberately force a warning message by testing the assembler response to an inducing condition.</td></tr>
</table>


#### Author

Duncan Munro  <duncan@duncanamps.com>
