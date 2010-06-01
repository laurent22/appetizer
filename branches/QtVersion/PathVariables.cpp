/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <FilePaths.h>
#include <PathVariables.h>

using namespace appetizer;


VariableToStringMap PathVariables::variableToStringMap_;


QString PathVariables::resolveVariables(const QString& input) {
  QRegExp r("(\\$\\(.*\\))", Qt::CaseInsensitive); // Matches $(Drive), etc.
  r.setMinimal(true);
  int previousPos = 0;
  int pos = 0;
  QString output = "";

  while ((pos = r.indexIn(input, pos)) != -1) {
     QString var = r.cap(1);
     QString varName = var.mid(2, var.length() - 3);
     QString path = getVariablePath(varName);

     output += input.mid(previousPos, pos - previousPos);
     output += path;

     pos += r.matchedLength();
     previousPos = pos;
  }

  if (previousPos < input.length()) output += input.mid(previousPos, input.length());

  return output;
}


QString PathVariables::getVariablePath(const QString& variable) {
  if (variableToStringMap_.find(variable) != variableToStringMap_.end()) return variableToStringMap_[variable];

  QString output;

  if (variable == "Drive") output = FilePaths::GetApplicationDrive();

  if (output != "") variableToStringMap_[variable] = output;

  return output;
}