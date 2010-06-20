/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_ConfigDialog_H
#define Appetizer_ConfigDialog_H

#include <UserSettings.h>

namespace appetizer {


class ConfigDialog : public QDialog {

  Q_OBJECT

public:

  ConfigDialog(QWidget* parent = NULL);
  ~ConfigDialog();

private:

  QTabWidget* tabWidget_;
  QBoxLayout* topLayout_;

};

}
#endif // Appetizer_ConfigDialog_H
