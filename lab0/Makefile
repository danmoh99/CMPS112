JAVASRC = lab0.java
SOURCES = README Makefile $(JAVASRC)
MAINCLASS = lab0
CLASSES = lab0.class
JARFILE = lab0
SUBMIT = submit cmps112-wm.w19 lab0

all: $(JARFILE)

$(JARFILE): $(CLASSES)
	echo Main-class: $(MAINCLASS) > Manifest
	jar cvfm $(JARFILE) Manifest $(CLASSES)
	rm Manifest
	chmod +x $(JARFILE)

$(CLASSES): $(JAVASRC)
	javac -Xlint $(JAVASRC)

clean:
	rm $(JARFILE) $(CLASSES)

submit: $(SOURCES)
	$(SUBMIT) $(SOURCES)

check:
	ls /afs/cats.ucsc.edu/class/cmps112-wm.w19/lab0/dimohamm/
        
