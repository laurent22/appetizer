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
    UserSettingsVector groupSettings = settings_->getSettingsByGroup(label);

    for (int j = 0; j < (int)groupSettings.size(); j++) {
      UserSetting* setting = groupSettings[j];
      addSettingControlToLayout_(setting, layout);
    }

    panel->setLayout(layout);

    tabWidget_->addTab(panel, label);
    tabPanels_.push_back(panel);
  }
}


void ConfigDialog::addSettingControlToLayout_(UserSetting* setting, QFormLayout* layout) {
  if (setting->label() == "") return;

  UserSetting::ControlType controlType = setting->controlType();
  QString label = setting->label();

  QWidget* control = NULL;
  
  if (controlType == UserSetting::CheckBox) {
    QCheckBox* c = new QCheckBox(label);
    label = "";
    control = static_cast<QWidget*>(c);
  } else if (controlType == UserSetting::TextBox) {
    QLineEdit* c = new QLineEdit();
    control = static_cast<QWidget*>(c);
  } else if (controlType == UserSetting::SpinBox) {
    QSpinBox* c = new QSpinBox();
    control = static_cast<QSpinBox*>(c);
  } else if (controlType == UserSetting::ComboBox) {
    QComboBox* c = new QComboBox();
    control = static_cast<QComboBox*>(c);

    for (int i = 0; i < setting->options().size(); i++) {
      std::pair<QString, QString> option = setting->options().at(i);
      c->addItem(option.first, QVariant(option.second));
    }
  }

  if (control) {
    layout->addRow(label, control);
  }

  
}