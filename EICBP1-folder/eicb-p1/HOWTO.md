# Anleitung für das erste Praktikum
**Einführung in den Compilerbau, Wintersemester 2024/25**

## Voraussetzungen

* Eine Java Virtual Machine, Version 11 oder 17. Neuere Java-Versionen können funktionieren, werden aber von uns nicht offiziell unterstützt.

## Einrichtung

Für die **Ersteinrichtung** benötigen Sie eine Internetverbindung.

Dieses Projekt verwendet [Gradle 7.5](https://docs.gradle.org/7.5.1/userguide/userguide.html) als Buildwerkzeug. Falls Gradle nicht auf Ihrem System verfügbar ist, können Sie die "Gradle Wrapper" genannten Skripte `gradlew` (Linux und macOS) bzw. `gradlew.bat` (Windows) anstelle des hier in der Anleitung verwendeten `gradle`-Befehls verwenden.

Führen Sie bitte folgendes Kommando aus:

	$ gradle mavlc
	
Falls Sie den Gradle Wrapper benutzen wollen, würden Sie stattdessen folgendes Kommando verwenden:

	$ ./gradlew mavlc

Dies erstellt im Verzeichnis `build/` die Startskripte `mavlc` (für Linux und macOS) und `mavlc.bat` (für Windows) für den von Ihnen zu entwickelnden MAVL-Compiler.

## Entwickeln und Testen

Während der Entwicklung können Sie die Übersetzung der Quellen mit

    $ gradle classes

starten. Dies übersetzt nur die geänderten Klassen. Wenn Sie eine komplette Neuübersetzung anstoßen möchten, führen Sie zunächst diesen Befehl aus:

	$ gradle clean

Das Projekt enthält die öffentlichen Testfälle der Praktikumsaufgaben, die Sie mittels

	$ gradle test

ausführen können. Das Kommando gibt nur eine Zusammenfassung auf der Konsole aus; den detaillierten Testreport finden Sie in der Datei `build/reports/tests/test/index.html`.

## Abgabe

Mit

	$ gradle prepareSubmission -PGroupNumber=??

erstellen Sie ein Archiv, welches Sie anschließend über den Moodle-Kurs abgeben können. Ersetzen Sie dabei ?? durch Ihre Gruppennummer.

## Compiler Driver

Um den Compiler auszuführen, nutzen Sie das zu Beginn generierte Startscript (siehe oben). Mit 

	$ build/mavlc --help

erhalten Sie eine Liste aller Optionen, mit denen Sie das Verhalten des Compilers steuern können.  
Beachten Sie, dass einige dieser Optionen erst in den späteren Praktika nutzbar sind.

Um beispielsweise den von Ihrem Parser generierten AST im [Graphviz DOT-Format](http://graphviz.org) ausgeben zu lassen, nutzen Sie den Befehl

	$ build/mavlc helloworld.mavl --dump-dot-ast

Wenn Sie die Graphviz-Werkzeuge auf Ihrem System installiert haben, können Sie aus der DOT-Datei anschließend ein Bild erzeugen:

	$ dot -Tpng -o helloworld.png helloworld.syn.dot

## Bekannte Probleme

* Unter Windows funktioniert das Startskript `mavlc.bat` nicht, wenn der Projektpfad nicht-ASCII-Zeichen (also insbesondere Umlaute) enthält.
