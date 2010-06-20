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


struct SettingWidgetInfo {
  UserSetting* setting;
  QWidget* control;
};


typedef std::vector<SettingWidgetInfo> SettingWidgetVector;


class ConfigDialog : public QDialog {

  Q_OBJECT

public:

  ConfigDialog(QWidget* parent = NULL);
  ~ConfigDialog();
  void loadSettings(UserSettings* settings); 
  inline UserSettingsVector modifiedSettings() const { return modifiedSettings_; }

private:

  QTabWidget* tabWidget_;
  QVBoxLayout* topLayout_;
  QHBoxLayout* buttonBarLayout_;
  UserSettings* settings_;
  std::vector<QWidget*> tabPanels_;
  QPushButton* saveButton_;
  QPushButton* cancelButton_;
  void addSettingControlToLayout_(UserSetting* setting, QFormLayout* layout);
  SettingWidgetVector settingControls_;
  UserSettingsVector modifiedSettings_;

private slots:

  void saveButton_clicked();
  void cancelButton_clicked();

};

}
#endif // Appetizer_ConfigDialog_H
