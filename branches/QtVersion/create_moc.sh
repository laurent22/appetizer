#!/bin/bash

mocPath=/cygdrive/c/Qt/2010.02.1/qt/bin/moc.exe
filePaths="gui/GraphicsItem.h,gui/MainWindow.h,gui/MainScene.h,FolderItem.h,gui/IconSprite.h"

filePathsArray=$(echo $filePaths | tr "," "\n")

for filePath in $filePathsArray
do
	echo Converting "$filePath"...
	
	filename=$(basename $filePath)
	filenameNoExt=${filename%.*}
	outputFilePath=moc/moc_$filenameNoExt.cpp
	tempFilePath=$outputFilePath.tmp
	
	$mocPath $filePath -o $tempFilePath
	sed -e "1i#include <stable.h>" $tempFilePath > $outputFilePath
	rm $tempFilePath
done