# RTTS - RunTime Type Service

RTTS (RunTime Type Services) ermöglicht es, die Definition von Variablen abzurufen oder sie während der Programmausführung zu erstellen. RTTS besteht aus 2 Komponenten:

- RTTI (RunTime Type Identification) wird verwendet, um die Definition bestehender Variablen oder bestehender Typen abzurufen.
- RTTC (RunTime Type Creation) wird verwendet, um neue Variablen mit beliebiger Definition zu erstellen; sie müssen durch die ABAP-Anweisung CREATE DATA ... TYPE HANDLE ... gefolgt werden, um die Variable zu erstellen.

RTTI und RTTC können über Methoden in den Klassen CL_ABAP_*DESCR aufgerufen werden. Jede Klasse verfügt über sowohl RTTI- als auch RTTC-Methoden.

Die Hierarchy der Type Description Klassen stellt sich wie folgt da:

![Hierarchy](https://github.com/MaddinJay/MajCon---SL-1/blob/main/Practice%20Phase%20Summer%2024/Framework%20Analysis/RTTS_Hierarchy.png)

Die Superklasse CL_ABAP_TYPEDESCR hat mehrere Unterklassen, um beispielsweise mit jeder Art von Typ umzugehen. Das Arbeiten mit diesem Vererbungshierarchie bedeutet, dass Casts verwendet werden müssen, insbesondere Downcasts, um Informationen zur Laufzeit abzurufen. Alle Möglichkeiten zur Informationsabfrage und Typenerstellung im Detail zu erläutern, würde den Rahmen sprengen. Überprüfen Sie die Informationen, Optionen und verschiedenen Methoden, die in der Klassendokumentation verwendet werden können, z. B. mithilfe der F2-Hilfe in ADT, um weitere Details zu erhalten.

## Analyse des Frameworks auf Basis des Wissens UP to PL

Das Framework besitzt Komponenten eines sauberen Frameworks.

### Positiv fällt auf, ...:

..., dass die Klassen TYPE spezifisch separariert wurden, so dass je nachdem welche Objekte erzeugt werden sollen, eine spezifische Klasse verwendet werden kann (Separation of concerns). 
Der Zugriff bzw. die Verwendung der Klassen und Methoden ist gut zu erarbeiten, wenn auch eine offizielle übersichtliche Dokumentation fehlt. Am ehesten kann man im [ABAP CheatSheet](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/06_Dynamic_Programming.md#runtime-type-services-rtts) den Aufbau und die Verwendung der Klassen nachvollziehen. Einen ewig langen Report als DEMO ohne weitere Erklärungen anzuführen, lässt aber ein wenig die Liebe zum Detail vermissen. 

### Dokumentation
Ist teilweise vorhanden, aber nicht recht ausführlich. Das Coding selbst ist nicht mit ABAP DOCs dokumentiert, da es wohl noch mit der SAPGUI erstellt wurde. Zu den einzelnen Methoden ist jeweils aber eine Long-Documentation vorhanden, welche die Funktionsweise beschreibt. Ein echter Plus-Punkt :).

Mit Hilfe des ABAP CheatSheets lässt sich die Klassenhierarchy und Verwendung relativ gut erarbeiten. Hier könnten die Beispiele noch expliziter aufgelistet werden, anstatt ein ewig langes DEMO Programm anzuführen. Aber, we all love debugging :)

### Interfaces/API
Die Klasseninstanzen lassen sich jeweils über eine statische Constructor-Methode erzeugen. Im Super-Constructor wird stets die LOAD_CLASS Methode aller Sub-Klassen gerufen, im Constructor der Subklasse wird ein SET HANDLER ausgeführt, um das Objekt zu instanziieren. Interessantes Konstrukt. Mir ergibt sich nicht so ganz, wieso dies so gemacht wird. Erscheint auch ein wenig unübersichtlich.
Für den Caller ergibt sich die Schnittstelle aus den öffentlichen Attributen und Methoden der Super-Klasse und Sub-Klasse. Es werden keine Interfaces verwendet.

#### Patterns/Static Classes:

#### Testing:

#### Information Hiding:

#### Naming:

#### Methoden / Messaging:


## Analyse des Frameworks auf Basis des Wissens UP to SL

## Links
- [SAP Doku inkl. Testreport](https://help.sap.com/docs/SUPPORT_CONTENT/abapobjects/3353526555.html)
- [Beispiele Codezentrale](https://codezentrale.de/abap-rtti-rttc-rtts-verwendung-von-typdescriptoren/)
- [Datenreferenzen](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abendata_reference_type.htm)
- [ABAP CheatSheet](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/06_Dynamic_Programming.md#runtime-type-services-rtts)
