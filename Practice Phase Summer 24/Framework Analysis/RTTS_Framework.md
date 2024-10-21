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
Die Klasseninstanzen lassen sich jeweils über eine statische Constructor-Methode erzeugen (FACTORY method). Für den Caller ergibt sich die Schnittstelle aus den öffentlichen Attributen und Methoden der Super-Klasse und Sub-Klasse. Es werden keine Interfaces verwendet. Die öffentlichen Methoden sind symmetrisch deklariert über die Klassen hinweg, was die Lesbarkeit verbessert.

Die deklarierten Konstanten und Attribute sollten besser in ein Interface ausgelagert werden, wenn sie schon öffentlich zugänglich gamacht werden. Bei den Konstanten stellt sich die Frage, ob diese wirklich zwingend öffnentlich sein müssen oder nicht doch in der Superklasse als PROTECTED deklariert werden sollten. Hier verstehe ich das Konzept aber noch zu wenig, um dies final beurteilen zu können.

Die Konstanten könnten je Themenblock zusammengefasst werden mit Hilfe von Strukturen. Generell würde ich diese in ein separates Interface auslagern, falls sie Klassenübergreifend genutzt werden sollen.

#### Patterns/Static Classes:
Für die Instanziierung wird je Klasse eine FACTORY Methode implementiert, was ok ist. Für die Instanziierung des Objekts wird ein Event erzeugt, dessen Deklaration in der Super-Klasse erfolgt und dessen Implementation in der Klasse selbst ausgeprägt ist. Dies wird ein wenig überkandidiert. Ich würde die Events rausnehmen und die Instanziierung des Objekts direkt im Konstruktor machen.

#### Testing:
Es sind keine UNIT Tests implmentiert.

#### Information Hiding:
Die Attribute der Klassen sind teilweise öffentlich, so dass sie von aussen zugreifbar und änderbar sind. Diese Attribute sollten PRIVATE sein, der Zugriff hierauf über GETTER Methoden ermöglicht werden.

#### Naming:
Das Naming der Klassen und Methoden ist okay. Die Methoden sind lesbar. 

#### Methoden / Messaging:
Die Klassen besitzen sowohl Query- als auch Commands Methoden. Command Methoden werden vor allem da verwendet, wo auf C/C++ Implementation zugriffen oder aber System-Calls abgesetzt werden. 

Das Coding der Methoden ist nicht übersichtlich und nicht symmetrisch angeordnet. Es fehlt jegliche Dokumentation. Die Methoden sind jedoch einigermassen klein gehalten und tun meist auch nur eine Sache (SRP). Es existieren viele Getter-Methoden - vermutlich weil von aussen auf die Objektinfos zugegriffen werden soll, um mit dem Objekt arbeiten zu können. 

Grundsätzlich sind die Unterklassen klein gehalten. Eine weitere Kapselung in weitere Klassen wäre jedoch denkbar.

## Analyse des Frameworks auf Basis des Wissens UP to SL

Konzentrieren wir uns auf die Themen Atomic Design und Elegant Objects so fällt auf:

### STATISCHE OBJEKTE
Die Klassen verwenden das FACTORY PATTERN, um die Objekte zu instantiieren. Hier wäre ein Wrapper denkbar, welcher die Instanziierung der einzelnen Objekte übernimmt, so dass man die Statischen Methoden und Attribute los wird.

### CONSTRUCTORS
Im Konstruktor erfolgt die Instanziierung der Objekte via Event-Handling. Gemäss Elegant Objects sollte dies nicht erfolgen. Wir sollten lediglich Zuweisungen machen.

### VERERBUNG
Die Super-Klasse vererbt an die einzelnen Unterklassen, was das Konstrukt auf den ersten Blick unübersichtlich erscheinen lässt. Hier könnte man gemäss Elegant Objects besser mit Interfaces arbeiten und die Logik via Interfaces splitten. 

### ATOMIC DESIGN
Durch die Auslagerung der Domainlogik in KERNEL MODULES erfolgt eine gewisse Atomisierung. Die Logik in den einzelnen Methoden könnte jedoch weiter ausgelagert werden, in spearate Klassen. 

Z.B. könnte in der Klasse CL_ABAP_CLASSDESCR die Logik der Methode GET_FRIEND_TYPES in eine separate Klasse ausgelagert werden. 


## Links
- [SAP Doku inkl. Testreport](https://help.sap.com/docs/SUPPORT_CONTENT/abapobjects/3353526555.html)
- [Beispiele Codezentrale](https://codezentrale.de/abap-rtti-rttc-rtts-verwendung-von-typdescriptoren/)
- [Datenreferenzen](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abendata_reference_type.htm)
- [ABAP CheatSheet](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/06_Dynamic_Programming.md#runtime-type-services-rtts)
