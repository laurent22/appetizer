#!/bin/bash

mocPath=/cygdrive/c/Qt/2010.02.1/qt/bin/moc.exe
filePaths="guilib/GraphicsItem.h,gui/MainWindow.h,gui/MainScene.h,FolderItem.h,gui/IconSprite.h,gui/MainPanel.h,gui/ScrollBar.h,gui/ScrollPane.h,guilib/GraphicsWindow.h"

filePathsArray=$(echo $filePaths | tr "," "\n")

for filePath in $filePathsArray
do	
	filename=$(basename $filePath)
	filenameNoExt=${filename%.*}
	outputFilePath=moc/moc_$filenameNoExt.cpp
	tempFilePath=$outputFilePath.tmp
	
	echo Generating "$outputFilePath"...
	
	# Generating the moc file...
	$mocPath $filePath -o $tempFilePath
	
	# Adding the precompiled header on top of the file.
	sed -e "1i#include <stable.h>" $tempFilePath > $outputFilePath
	
	# Removing timestamp from the file as we don't want to mark it as
	# "changed" for Git if nothing has actually changed.
	sed -i '6 d' $outputFilePath 
	sed -i '5 d' $outputFilePath
	sed -i '4 d' $outputFilePath
	
	rm $tempFilePath
done