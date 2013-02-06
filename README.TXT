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
o4glws\License.txt      Software license
o4glws\transparent.ico  The wizard's icon
o4glws\Wizard3.gif      One of the wizard's images 
o4glws\Success.gif      One of the wizard's images 
o4glws\Error.gif        One of the wizard's images 
o4glws\WSDL.p           WSDL adapter generator
o4glws\sample\          Samples of the generated code 
o4glws\templates\       Templates used to generate adapters

TODO
- Implement the password Digest specified in the WS-I security profile, right now only password text is supported
- Give the user the hability to choose which internal procedures to publish.
- Give the user the hability to preset some parameter's values.
- Implement user defined request parameters, they will help to communicate the security procedure with the Webservices methods.
- Give the user the hability to define the webservices methods and parameters without requiring external procedure files, this way they may generate webservice skeletons.
- Establish a workaround to facilitate the direct XML output instead of relaying in the node creation/dumping that may be inadequate for large amounts of data. 

HISTORY

2009.10.22  Version 0.3
BUGFIX: Some datatypes where not well implemented and cannot be handled by Axis 1.1
BUGFIX: Adapter.p was not generating the call to procedure adapter before calling subprocedure

2006.05.01  Version 0.2 beta
BUGFIX: The message saying "Unable to use Namespace: []" was removed
BUGFIX: The malformed webservice address is now correct
BUGFIX: The Generated WSDL wasn't describing exactly the output parameters of the Adapters 
CHANGE: Buffer handlers for output tables are no longer needed
ADDED:  Date parameters are now supported
ADDED:  Optimized speed for output tables
ADDED:  Expiration date and time parameters were added to securityTemplate.p
ADDED:  Fault report

2006.02.19  Version 0.1 alpha 
Initial release
