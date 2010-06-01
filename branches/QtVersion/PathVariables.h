/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_PathVariables_H
#define Appetizer_PathVariables_H
namespace appetizer {

typedef std::map<QString, QString> VariableToStringMap;

class PathVariables {

public:

  static QString resolveVariables(const QString& input);
  static QString getVariablePath(const QString& input);

private:

  static VariableToStringMap variableToStringMap_;

};

}
#endif // Appetizer_PathVariables_H
