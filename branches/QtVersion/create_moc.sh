#!/bin/bash

mocPath=/cygdrive/c/Qt/2010.02.1/qt/bin/moc.exe
filePaths="gui/GraphicsItem.h,gui/MainWindow.h,gui/MainScene.h,FolderItem.h,gui/IconSprite.h,gui/MainPanel.h,gui/ScrollBar.h,gui/ScrollPane.h"

filePathsArray=$(echo $filePaths | tr "," "\n")

for filePath in $filePathsArray
do	
	filename=$(basename $filePath)
	filenameNoExt=${filename%.*}
	outputFilePath=moc/moc_$filenameNoExt.cpp
	tempFilePath=$outputFilePath.tmp
	
	echo Generating "$outputFilePath"...
	
	$mocPath $filePath -o $tempFilePath
	sed -e "1i#include <stable.h>" $tempFilePath > $outputFilePath
	rm $tempFilePath
done