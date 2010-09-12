FLEXPATH=/home/mkrentovskiy/Flex/sdks/3.0.0
SOURCES = broadcast.swf view_lead.swf view_other.swf

all:	$(SOURCES)
	mv *.swf ../wwwroot/swf

$(SOURCES): %.swf: %.mxml
	$(FLEXPATH)/bin/mxmlc ./$< -sp ./ -o ./$@ -use-network=false
