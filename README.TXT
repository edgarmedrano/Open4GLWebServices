INSTALATION

Put the entire o4glws directory somewhere in the PROPATH and make a copy the 
file o4glws.i in any directory in the PROPATH of the deployment machine, 
because it's needed to compile the deployed webservice adapter procedures. 

INCLUDED FILES

o4glws\Adapter.p        Webservice adapter generator 
o4glws\o4glws.i         Library included in every adapter
o4glws\o4glws.w         Wizard to generate adapter/WSDL files
o4glws\procDlg.w        Dialog to select the procedures to include in the
                        webservice
o4glws\procInfo.i       Temp-table definitions used by all programs 
o4glws\procInfo.p       Extracts the information about internal procedure, 
                        functions and temp-tables of a procedure file
o4glws\README.TXT       This file
o4glws\transparent.ico  The wizard's icon
o4glws\Wizard3.gif      One of the wizard's images 
o4glws\WSDL.p           WSDL adapter generator
o4glws\sample\          Samples of the generated code 
o4glws\templates\       Templates used to generate adapters

KNOWN ISSUES
You cann't choose which internal procedures to publish.
The security procedure isn't getting the parameters of created date and time.

HISTORY

2006.02.19  Version 0.1 alpha 
Initial release
