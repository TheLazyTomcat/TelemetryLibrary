start /wait .\PostProcessor\PostProcessor.exe "..\..\Documentation\"
.\GraphViz\dot.exe -Tpng ..\Documentation\GVUses.dot > ..\Documentation\GVUses.png
.\GraphViz\dot.exe -Tpng ..\Documentation\GVClasses.dot > ..\Documentation\GVClasses.png