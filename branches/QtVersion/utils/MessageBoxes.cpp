/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <MessageBoxes.h>
using namespace appetizer;


int MessageBoxes::error(const QString& message) {
  QMessageBox m;
  m.setText(message);
  m.setIcon(QMessageBox::Critical);
  return m.exec();
}


int MessageBoxes::warning(const QString& message) {
  QMessageBox m;
  m.setText(message);
  m.setIcon(QMessageBox::Warning);
  return m.exec();
}


int MessageBoxes::info(const QString& message) {
  QMessageBox m;
  m.setText(message);
  m.setIcon(QMessageBox::Information);
  return m.exec();
}


int MessageBoxes::confirmation(const QString& message) {
  QMessageBox m;
  m.setText(message);
  m.setStandardButtons(QMessageBox::Ok | QMessageBox::Cancel);
  m.setIcon(QMessageBox::Question);
  return m.exec();
}