/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <ConfigDialog.h>

using namespace appetizer;


ConfigDialog::ConfigDialog(QWidget* parent): QDialog(parent) {
  topLayout_ = new QBoxLayout(QBoxLayout::Up, this);
  setLayout(topLayout_);

  tabWidget_ = new QTabWidget(this);
  topLayout_->addWidget(tabWidget_);
}


ConfigDialog::~ConfigDialog() {

}


void ConfigDialog::loadSettings(UserSettings* settings) {
  settings_ = settings;

  std::vector<QString> labels = UserSettings::instance()->getGroupLabels();

  for (int i = 0; i < (int)labels.size(); i++) {
    QString label = labels[i];
    QWidget* panel = new QWidget(this);
    QFormLayout* layout = new QFormLayout(this);
    panel->setLayout(layout);

    UserSettingsVector groupSettings = settings_->getSettingsByGroup(label);

    for (int j = 0; j < (int)groupSettings.size(); j++) {
      UserSetting* setting = groupSettings[j];
      addSettingControlToLayout_(setting, layout);
    }

    tabWidget_->addTab(panel, label);
    tabPanels_.push_back(panel);
  }
}


void ConfigDialog::addSettingControlToLayout_(UserSetting* setting, QFormLayout* layout) {
  QVariant::Type type = setting->value().type();

}