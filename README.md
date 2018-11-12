# Open 4GL WebServices
### If you are looking for the tidbits README take a look at the [README.TXT](https://github.com/edgarmedrano/Open4GLWebServices/blob/master/README.TXT)  file.

The Open 4GL Webservices Project offers an opensource SOAP implementation to OpenEdge (PROGRESS) developers. 
This is an alternative to the commercial version distributed by PROGRESS, but this version doesn't require Sonic MQ 
or an AppServer to run properly.

O4GLWS provides a wizard application and a framework to make it easy to publish OpenEdge procedures as webservices 
generating the WSDL and webservice proxy code, without requiring the developer to learn XML or the framework itself.

With the wizard application, developers are able to select an existing OpenEdge procedure and produce the WSDL file 
and/or a proxy procedure that handle the SOAP call and translate between SOAP and PROGRESS datatypes. 
It also can handle the WS-I security recommendation requiring a custom procedure to authenticate the user/password.

Also, the generated proxy procedure is based on procedure templates (skeletons), these templates allow the developer 
to choose how to deploy the webservice. The developer can choose between webspeed, a batch version to call it from a CGI, 
a socket based version, or a custom template, etc.

For further information about this project, take a look at http://o4glws.sourceforge.net/

