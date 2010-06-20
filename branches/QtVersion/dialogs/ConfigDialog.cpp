/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <ConfigDialog.h>

using namespace appetizer;


ConfigDialog::ConfigDialog(QWidget* parent): QDialog(parent) {
  topLayout_ = new QVBoxLayout(this);
  setLayout(topLayout_);

  tabWidget_ = new QTabWidget(this);
  topLayout_->addWidget(tabWidget_);

  buttonBarLayout_ = new QHBoxLayout(this);
  topLayout_->addLayout(buttonBarLayout_);

  buttonBarLayout_->addStretch();

  saveButton_ = new QPushButton(_("Save"));
  saveButton_->adjustSize();
  saveButton_->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Fixed);
  QObject::connect(saveButton_, SIGNAL(clicked()), this, SLOT(saveButton_clicked()));
  buttonBarLayout_->addWidget(saveButton_);

  cancelButton_ = new QPushButton(_("Cancel"));
  cancelButton_->adjustSize();
  cancelButton_->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Fixed);
  QObject::connect(cancelButton_, SIGNAL(clicked()), this, SLOT(cancelButton_clicked()));
  buttonBarLayout_->addWidget(cancelButton_);
}


ConfigDialog::~ConfigDialog() {

}


void ConfigDialog::saveButton_clicked() {
  modifiedSettings_.clear();

  for (int i = 0; i < (int)settingControls_.size(); i++) {
    SettingWidgetInfo info = settingControls_[i];
    //if (info.setting->value() == info.initialValue) modifiedSettings_.push_back(info.setting);
  }

  close();
}


void ConfigDialog::cancelButton_clicked() {
  close();
}


void ConfigDialog::loadSettings(UserSettings* settings) {
  settings_ = settings;

  // TODO: delete existing controls? or reuse existing ones?

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
  
  if (controlType == UserSetting::CheckBox) { // CHECKBOX

    QCheckBox* c = new QCheckBox(label);
    c->setChecked(setting->value().toBool());
    label = "";
    control = static_cast<QWidget*>(c);

  } else if (controlType == UserSetting::TextBox) { // TEXTBOX

    QLineEdit* c = new QLineEdit();
    c->setText(setting->value().toString());
    control = static_cast<QWidget*>(c);

  } else if (controlType == UserSetting::SpinBox) { // SPINBOX

    QSpinBox* c = new QSpinBox();
    c->setValue(setting->value().toInt());
    c->setMinimum(setting->minValue());
    c->setMaximum(setting->maxValue());
    control = static_cast<QSpinBox*>(c);

  } else if (controlType == UserSetting::ComboBox) { // COMBOBOX

    QComboBox* c = new QComboBox();
    control = static_cast<QComboBox*>(c);
    int selectedIndex = 0;

    for (int i = 0; i < (int)setting->options().size(); i++) {
      std::pair<QString, QString> option = setting->options().at(i);
      if (selectedIndex <= 0) {
        if (option.second != "") {
          if (option.second == setting->value().toString()) selectedIndex = i;
        } else {
          if (option.first == setting->value().toString()) selectedIndex = i;
        }
      }
      c->addItem(option.first, QVariant(option.second));
    }

    c->setCurrentIndex(selectedIndex);

  }

  if (control) {
    layout->addRow(label, control);

    SettingWidgetInfo info;
    info.setting = setting;
    info.control = control;
    settingControls_.push_back(info);
  }

}