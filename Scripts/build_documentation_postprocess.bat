pushd .\

cd "..\Telemetry Library\AutoDocumentation"

start /wait .\PostProcessor\PostProcessor.exe "..\..\Documentation\"
.\GraphViz\dot.exe -T png ..\Documentation\GVUses.dot > ..\Documentation\GVUses.png
.\GraphViz\dot.exe -T png ..\Documentation\GVClasses.dot > ..\Documentation\GVClasses.png

popd