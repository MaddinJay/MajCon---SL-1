# RTTS - RunTime Type Service

RTTS (RunTime Type Services) allows to get the definition of variables or to create them during program execution. RTTS is made of 2 components:

- RTTI (RunTime Type Identification) is used to get the definition of existing variables or existing types
- RTTC (RunTime Type Creation) is used to create new variables with any definition; they must be followed by the ABAP statement CREATE DATA ... TYPE HANDLE ... to create the variable.

RTTI and RTTC may be called using methods in CL_ABAP_*DESCR classes. Each class have both RTTI and RTTC methods.

## Links
- [SAP Doku inkl. Testreport](https://help.sap.com/docs/SUPPORT_CONTENT/abapobjects/3353526555.html)
- [Beispiele Codezentrale](https://codezentrale.de/abap-rtti-rttc-rtts-verwendung-von-typdescriptoren/)
- [Datenreferenzen](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abendata_reference_type.htm)
